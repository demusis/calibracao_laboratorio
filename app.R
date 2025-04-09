# Painel para análise de regressão com transformações (WLS)
# Autores: Carlo Ralph De Musis e Eguiberto Bernardes Fraga Júnior
# Data: 09/04/2025 

library(shiny)
library(readxl)
library(openxlsx)
library(lmtest)
library(DT)
library(ggplot2)
library(dplyr)
library(boot)
library(robustbase)  # para lmrob

ui <- fluidPage(
  titlePanel("POLITEC - Análises de Regressão por Mínimos Quadrados Ponderados"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput("file_excel", "Carregue a planilha Excel (contendo colunas y, x, substancia)",
                accept = c(".xlsx")),
      
      actionButton("rodar", "Executar Análises"),
      hr(),
      
      # Slider para o número de reamostragens do bootstrap (tanto para IC dos coeficientes 
      # como para repetição de holdout)
      sliderInput(
        inputId = "num_boot",
        label = "Número de repetições:",
        min = 100,
        max = 10000,
        value = 300,
        step = 100
      ),
      hr(),
      
      # Checkbox para escolher regressão robusta ou não
      checkboxInput("robusto", "Usar regressão robusta?", value = FALSE),
      hr(),
      
      # Slider para definir a fração de dados de treino (holdout) em cada repetição
      sliderInput(
        inputId = "frac_treino",
        label = "Proporção para Treino (Holdout):",
        min = 0.1,
        max = 0.9,
        value = 0.7,
        step = 0.1
      ),
      
      hr(),
      
      actionButton("sobre", "Sobre...")
      
      
      # downloadButton("download_csv", "Baixar Resultados (CSV)"),
      # downloadButton("download_excel", "Baixar Resultados (Excel)")
    ),
    
    mainPanel(
      fluidRow(
        column(12, align = "center",
               downloadButton("download_csv", "Baixar Resultados (CSV)"),
               downloadButton("download_excel", "Baixar Resultados (Excel)")
        )
      ),
      hr(),
      
      DTOutput("tabela_resultados"),
      hr(),
      
      tableOutput("tabela_coeficientes"),
      hr(),
      
      plotOutput("plot_regressao"),
      plotOutput("plot_residuos"),
      plotOutput("plot_scale_location")
    )
  )
)

server <- function(input, output, session) {
  
  # Lista de modelos (transformações)
  modelos <- list(
    list(nome = "1",
         formula = y ~ x,
         peso = function(d) rep(1, nrow(d))),          # w = 1
    
    list(nome = "1/x",
         formula = y ~ x,
         peso = function(d) 1 / (d$x)),                  # w = 1/x
    
    list(nome = "1/x^2",
         formula = y ~ x,
         peso = function(d) 1 / (d$x^2) ),                # w = 1/x²
    
    list(nome = "1/y",
         formula = y ~ x,
         peso = function(d) 1 / (d$y) ),                  # w = 1/y
    
    list(nome = "1/y^2",
         formula = y ~ x,
         peso = function(d) 1 / (d$y^2) )                 # w = 1/y²
  )
  
  # Reactive para a base
  dados <- reactive({
    req(input$file_excel)
    read_excel(input$file_excel$datapath)
  })
  
  # EventReactive para rodar as análises
  resultados_react <- eventReactive(input$rodar, {
    
    df <- dados()
    
    if(!all(c("y","x","substancia") %in% names(df))){
      showModal(modalDialog(
        title = "Erro",
        "A planilha deve conter as colunas: y, x, substancia.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    lista_subst <- unique(df$substancia)
    
    # Data frame de resultados (sem r2_holdout; com mad_holdout e rmse_holdout)
    df_result <- data.frame(
      substancia = character(),
      modelo = character(),
      coef_intercepto = numeric(),
      coef_inclinacao = numeric(),
      p_valor_regressao = numeric(),
      r2 = numeric(),
      p_valor_breusch = numeric(),
      p_valor_shapiro = numeric(),
      AIC = numeric(),
      BIC = numeric(),
      rmse_holdout = numeric(),
      mad_holdout = numeric(),
      re_holdout = numeric(),
      stringsAsFactors = FALSE
)
    
    # Lista para armazenar modelos + dados (para geração de gráficos e bootstrap de coef.)
    lista_modelos <- list()
    
    set.seed(42)  # Para reprodutibilidade
    
    for(s in lista_subst){
      df_sub <- subset(df, substancia == s)
      
      for(mod_info in modelos){
        
        
        # ---------- Calcula o vetor de pesos -------------------------
        w_vec <- tryCatch(mod_info$peso(df_sub), error = function(e) NULL)
        if (is.null(w_vec) || any(!is.finite(w_vec))) {
          warning("Pesos inválidos para ", mod_info$nome)
          next
        }
        # ---------------------------------------------------------------

        # Ajuste do modelo em TODO o conjunto (para obter p-valores, R2 etc.)
        ajuste_completo <- tryCatch({
          if (input$robusto) {
            lmrob(y ~ x, data = df_sub, weights = w_vec)
          } else {
            lm(y ~ x, data = df_sub, weights = w_vec)
          }
        }, error = function(e) NULL)
        
        if(is.null(ajuste_completo)){
          # Se não foi possível ajustar o modelo, preenchendo com NA
          new_line <- data.frame(
            substancia = s,
            modelo = mod_info$nome,
            coef_intercepto = NA,
            coef_inclinacao = NA,
            p_valor_regressao = NA,
            r2 = NA,
            p_valor_breusch = NA,
            p_valor_shapiro = NA,
            AIC = NA,
            BIC = NA,
            rmse_holdout = NA,
            mad_holdout = NA,
            re_holdout = NA,
            stringsAsFactors = FALSE
          )
          
          df_result <- rbind(df_result, new_line)
          next
        }
        
        # ----
        # Extraindo métricas de todo o ajuste (F, p-valor, R2, AIC, BIC, Breusch, Shapiro)
        # ----
        is_robust <- inherits(ajuste_completo, "lmrob")
        
        if (!is_robust) {
          sum_mod <- summary(ajuste_completo)
          # Estatística F e p-valor
          f_est <- sum_mod$fstatistic
          p_valor_reg <- pf(f_est[1], f_est[2], f_est[3], lower.tail = FALSE)
          # R2
          r2_val <- sum_mod$r.squared
          
          coefs <- tryCatch(coef(ajuste_completo), error = function(e) NULL)
          if (!is.null(coefs) && length(coefs) >= 2) {
            intercepto <- coefs[1]
            inclinacao <- coefs[2]
          }  
            
        } else {
          
          p_valor_reg <- NA
          r2_val <- NA
          
          intercepto <- NA
          inclinacao <- NA
        }
        
        # Breusch-Pagan
        bp_test <- tryCatch(bptest(ajuste_completo), error = function(e) list(p.value=NA))
        p_valor_bp <- bp_test$p.value
        
        # Shapiro-Wilk
        sw_test <- tryCatch(shapiro.test(residuals(ajuste_completo)), error = function(e) list(p.value=NA))
        p_valor_sw <- sw_test$p.value
        
        # AIC e BIC
        aic_val <- tryCatch(AIC(ajuste_completo), error = function(e) NA)
        bic_val <- tryCatch(BIC(ajuste_completo), error = function(e) NA)
        
        # -----------------------
        # Repetição do holdout (n = input$num_boot) para RMSE e MAD
        # -----------------------
        rmse_vec <- numeric(input$num_boot)
        mad_vec  <- numeric(input$num_boot)
        re_vec  <- numeric(input$num_boot)
        
         for(i in seq_len(input$num_boot)){
          # Gerando partição treino/teste para cada repetição
          n <- nrow(df_sub)
          idx_treino <- sample(seq_len(n), size = floor(input$frac_treino * n))
          dados_treino <- df_sub[idx_treino, ]
          dados_teste  <- df_sub[setdiff(seq_len(n), idx_treino), ]
          
          if (nrow(dados_teste) == 0) {
            # Se por acaso não sobrou nada pro teste, ignoramos
            rmse_vec[i] <- NA
            mad_vec[i]  <- NA
            re_vec[i]  <- NA
            next
          }
          
          w_train <- mod_info$peso(dados_treino)
          w_test  <- mod_info$peso(dados_teste)
          
          # Ajusta modelo (robusto ou não) nos dados de treino
          ajuste_hold <- tryCatch({
            if (input$robusto) {
              # lmrob(mod_info$formula, data = dados_treino, weights = mod_info$peso(dados_treino))
              lmrob(y ~ x, data = dados_treino, weights = mod_info$peso(dados_treino))
            } else {
              lm(y ~ x, data = dados_treino, weights = mod_info$peso(dados_treino))
            }
          }, error = function(e) NULL)
          
          if(is.null(ajuste_hold)){
            rmse_vec[i] <- NA
            mad_vec[i]  <- NA
            re_vec[i]  <- NA
            next
          }
          
          # Predição no teste
          pred_teste <- predict(ajuste_hold, newdata = dados_teste)
          obs_teste  <- dados_teste$y
          
          rmse_vec[i] <- sqrt(mean((obs_teste - pred_teste)^2))
          mad_vec[i]  <- mean(abs(obs_teste - pred_teste))
          re_vec <- sum(abs((pred_teste - obs_teste)/obs_teste)*100)
        }
        
        # Média dos valores de RMSE, MAD e RE
        rmse_hold <- mean(rmse_vec, na.rm = TRUE)
        mad_hold  <- mean(mad_vec,  na.rm = TRUE)
        re_hold   <- mean(re_vec,  na.rm = TRUE)
        
        new_line <- data.frame(
          substancia = s,
          modelo = mod_info$nome,
          coef_intercepto = intercepto,
          coef_inclinacao = inclinacao,
          p_valor_regressao = p_valor_reg,
          r2 = r2_val,
          p_valor_breusch = p_valor_bp,
          p_valor_shapiro = p_valor_sw,
          AIC = aic_val,
          BIC = bic_val,
          rmse_holdout = rmse_hold,
          mad_holdout = mad_hold,
          re_holdout = re_hold,
          stringsAsFactors = FALSE
        )
        
        df_result <- rbind(df_result, new_line)
        
        # Guardamos o modelo e dados (para gráficos e bootstrap de coef.)
        key_model <- paste(s, mod_info$nome, sep = "_")
        lista_modelos[[key_model]] <- list(
          model   = ajuste_completo,  # ajuste com TODOS os dados
          data    = df_sub,
          formula = mod_info$formula
        )
      }
    }
    
    list(
      tabela = df_result,
      modelos = lista_modelos
    )
  })
  
  # Tabela principal (DT)
  output$tabela_resultados <- renderDT({
    req(resultados_react())
    
    df_result <- resultados_react()$tabela
    
    # Remove quaisquer nomes de linha existentes
    rownames(df_result) <- NULL
    
    datatable(
      df_result,
      rownames = FALSE, 
      selection = "single",
      options = list(pageLength = 10)
    ) %>%
      formatSignif(columns = c("coef_intercepto", "coef_inclinacao",
                               "p_valor_regressao","r2","p_valor_breusch",
                               "p_valor_shapiro","AIC","BIC",
                               "rmse_holdout","mad_holdout", "re_holdout"),
                   digits = 7)
  })
  
  # Download CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("ResultadosRegressao_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      req(resultados_react())
      df_result <- resultados_react()$tabela
      write.csv(df_result, file, row.names = FALSE)
    }
  )
  
  # Download Excel
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("ResultadosRegressao_", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      req(resultados_react())
      df_result <- resultados_react()$tabela
      write.xlsx(df_result, file = file, rowNames = FALSE)
    }
  )
  
  # Tabela de coeficientes (com IC bootstrap)
  tabela_coefs <- reactiveVal(data.frame())
  
  observeEvent(input$tabela_resultados_rows_selected, {
    linha_sel <- input$tabela_resultados_rows_selected
    if(length(linha_sel) == 0) return(NULL)
    
    df_result <- resultados_react()$tabela
    lista_mod <- resultados_react()$modelos
    
    sel_data <- df_result[linha_sel, ]
    subst_sel <- sel_data$substancia
    modelo_sel <- sel_data$modelo
    
    key_model <- paste(subst_sel, modelo_sel, sep = "_")
    pack <- lista_mod[[key_model]]
    
    if(is.null(pack)){
      output$plot_regressao <- renderPlot({})
      output$plot_residuos <- renderPlot({})
      output$plot_scale_location <- renderPlot({})
      tabela_coefs(data.frame())
      return(NULL)
    }
    

    # --- Antes de renderizar o gráfico ---
    mod <- pack$model
    df_sub <- pack$data
    form_mod <- pack$formula
    
    # Filtra valores válidos
    df_mod_filtrado <- df_sub %>%
      filter(!is.na(x) & !is.na(y) & is.finite(x) & is.finite(y))
    
    # Gera predições com intervalos de confiança
    preds <- tryCatch({
      predict(mod, newdata = df_mod_filtrado, interval = "confidence")
    }, error = function(e) NULL)
    
    # --- Gráfico de Regressão ---
    output$plot_regressao <- renderPlot({
      if (nrow(df_mod_filtrado) == 0 || is.null(preds)) {
        plot.new()
        title("Nenhum ponto disponível ou erro na predição.")
        return(NULL)
      }
      
      df_plot <- df_mod_filtrado
      df_plot$fit <- preds[, "fit"]
      df_plot$lwr <- preds[, "lwr"]
      df_plot$upr <- preds[, "upr"]
      
      gg <- ggplot(df_plot, aes(x = x, y = y)) +
        geom_point() +
        geom_line(aes(y = fit), color = "blue", linewidth = 1) +
        geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
        labs(
          x = "x",
          y = "y",
          title = paste0("Ponderador dos resíduos: ", modelo_sel, " (subst.: ", subst_sel, ")",
                         if (input$robusto) " [Robusto]" else " [Clássico]")
        )
      
      print(gg)
    })
    
    
    
    
    
    
    # Plot Resíduos vs Fitted
    output$plot_residuos <- renderPlot({
      df_res <- data.frame(
        fitted = fitted(mod),
        resid  = residuals(mod)
      ) %>%
        filter_all(all_vars(!is.na(.)))  # remove NA se houver
      
      if (nrow(df_res) == 0) {
        plot.new()
        title("Não há resíduos válidos a exibir.")
        return(NULL)
      }
      
      gg_res <- ggplot(df_res, aes(x = fitted, y = resid)) +
        geom_point() +
        geom_hline(yintercept = 0, linetype = "dashed") +
        labs(
          x = "Valores estimados",
          y = "Resíduos",
          title = paste("Resíduos vs. Valores estimados", 
                        if (input$robusto) "(Robusto)" else "(Clássico)")
        )
      print(gg_res)
    })
    
    # Plot Scale-Location
    output$plot_scale_location <- renderPlot({
      df_res <- data.frame(
        fitted = fitted(mod),
        resid  = residuals(mod)
      ) %>%
        filter_all(all_vars(!is.na(.)))
      
      if (nrow(df_res) == 0) {
        plot.new()
        title("Não há resíduos válidos a exibir.")
        return(NULL)
      }
      
      df_res$sqrt_abs_resid <- sqrt(abs(df_res$resid))
      
      gg_sc <- ggplot(df_res, aes(x = fitted, y = sqrt_abs_resid)) +
        geom_point() +
        geom_smooth(method="loess", formula = y ~ x, se=FALSE) +
        labs(
          x = "Valores estimados",
          y = expression(sqrt("|Resíduo|")),
          title = paste("Escala-locação",
                        if (input$robusto) "(Robusto)" else "(Clássico)")
        )
      print(gg_sc)
    })
  })
  
  # Render da tabela de coeficientes (acima dos gráficos) - formatando para 7 caracteres
  output$tabela_coeficientes <- renderTable({
    req(nrow(tabela_coefs()) > 0)
    
    df_show <- tabela_coefs()
    df_show[] <- lapply(df_show, function(x) {
      if (is.numeric(x)) {
        formatC(x, digits=7, format="g")
      } else {
        x
      }
    })
    df_show
  })

  observeEvent(input$sobre, {
    showModal(modalDialog(
      title = "Sobre este aplicativo",
      HTML("
      <p><strong>Perícia Oficial e Identificação Técnica do Estado de Mato Grosso</strong><br>
      Análises de Regressão por Mínimos Quadrados Ponderados</p>
      
      <p><strong>Autores:</strong> Carlo Ralph De Musis e Eguiberto Bernardes Fraga Júnior<br>
      <strong>Data:</strong> 09/04/2025</p>
      
      <p><strong>Repositório:</strong><br>
      <a href='https://github.com/demusis/calibracao_laboratorio' target='_blank'>
      https://github.com/demusis/calibracao_laboratorio</a></p>
    "),
      easyClose = TRUE,
      footer = modalButton("Fechar")
    ))
  })
  
  }









shinyApp(ui = ui, server = server)