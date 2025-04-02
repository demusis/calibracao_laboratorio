# Painel para análise de regressão com transformações
# Autores: Carlo Ralph De Musis e Eguiberto Bernardes Fraga Júnior
# Data: 02/04/2025

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
  titlePanel("Análises de Regressão com Transformações"),
  
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
      
      downloadButton("download_csv", "Baixar Resultados (CSV)"),
      downloadButton("download_excel", "Baixar Resultados (Excel)")
    ),
    
    mainPanel(
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
    list(nome = "y ~ x",         formula = y ~ x),
    list(nome = "y ~ 1/x",       formula = y ~ I(1/x)),
    list(nome = "y ~ 1/x^2",     formula = y ~ I(1/x^2)),
    list(nome = "1/y ~ x",       formula = I(1/y) ~ x),
    list(nome = "1/y^2 ~ x",     formula = I(1/y^2) ~ x)
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
      p_valor_regressao = numeric(),
      r2 = numeric(),
      p_valor_breusch = numeric(),
      p_valor_shapiro = numeric(),
      AIC = numeric(),
      BIC = numeric(),
      rmse_holdout = numeric(),
      mad_holdout = numeric(),
      stringsAsFactors = FALSE
    )
    
    # Lista para armazenar modelos + dados (para geração de gráficos e bootstrap de coef.)
    lista_modelos <- list()
    
    set.seed(123)  # Para reprodutibilidade
    
    for(s in lista_subst){
      df_sub <- subset(df, substancia == s)
      
      for(mod_info in modelos){
        
        # Ajuste do modelo em TODO o conjunto (para obter p-valores, R2 etc.)
        ajuste_completo <- tryCatch({
          if (input$robusto) {
            lmrob(mod_info$formula, data = df_sub)
          } else {
            lm(mod_info$formula, data = df_sub)
          }
        }, error = function(e) NULL)
        
        if(is.null(ajuste_completo)){
          # Se não foi possível ajustar o modelo, preenchendo com NA
          new_line <- data.frame(
            substancia = s,
            modelo = mod_info$nome,
            p_valor_regressao = NA,
            r2 = NA,
            p_valor_breusch = NA,
            p_valor_shapiro = NA,
            AIC = NA,
            BIC = NA,
            rmse_holdout = NA,
            mad_holdout = NA,
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
        } else {
          p_valor_reg <- NA
          r2_val <- NA
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
            next
          }
          
          # Ajusta modelo (robusto ou não) nos dados de treino
          ajuste_hold <- tryCatch({
            if (input$robusto) {
              lmrob(mod_info$formula, data = dados_treino)
            } else {
              lm(mod_info$formula, data = dados_treino)
            }
          }, error = function(e) NULL)
          
          if(is.null(ajuste_hold)){
            rmse_vec[i] <- NA
            mad_vec[i]  <- NA
            next
          }
          
          # Predição no teste
          pred_teste <- predict(ajuste_hold, newdata = dados_teste)
          obs_teste  <- dados_teste$y
          
          rmse_vec[i] <- sqrt(mean((obs_teste - pred_teste)^2))
          mad_vec[i]  <- mean(abs(obs_teste - pred_teste))
        }
        
        # Média dos valores de RMSE e MAD
        rmse_hold <- mean(rmse_vec, na.rm = TRUE)
        mad_hold  <- mean(mad_vec,  na.rm = TRUE)
        
        new_line <- data.frame(
          substancia = s,
          modelo = mod_info$nome,
          p_valor_regressao = p_valor_reg,
          r2 = r2_val,
          p_valor_breusch = p_valor_bp,
          p_valor_shapiro = p_valor_sw,
          AIC = aic_val,
          BIC = bic_val,
          rmse_holdout = rmse_hold,
          mad_holdout = mad_hold,
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
      formatSignif(columns = c("p_valor_regressao","r2","p_valor_breusch",
                               "p_valor_shapiro","AIC","BIC",
                               "rmse_holdout","mad_holdout"),
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
    
    mod <- pack$model
    df_sub <- pack$data
    form_mod <- pack$formula
    
    # ==============
    # 1) Bootstrap dos coeficientes (usando input$num_boot)
    # ==============
    boot_fn <- function(data, indices){
      d <- data[indices, ]
      if (input$robusto) {
        fit <- lmrob(form_mod, data = d)
      } else {
        fit <- lm(form_mod, data = d)
      }
      coef(fit)
    }
    
    R_boot <- input$num_boot
    
    set.seed(123)
    bs_results <- tryCatch({
      boot(data = df_sub, statistic = boot_fn, R = R_boot)
    }, error = function(e) NULL)
    
    if(is.null(bs_results)){
      tabela_coefs(data.frame())
      output$plot_regressao <- renderPlot({})
      output$plot_residuos <- renderPlot({})
      output$plot_scale_location <- renderPlot({})
      return(NULL)
    }
    
    coefs_orig <- coef(mod)
    ic_lower <- apply(bs_results$t, 2, quantile, probs = 0.025, na.rm = TRUE)
    ic_upper <- apply(bs_results$t, 2, quantile, probs = 0.975, na.rm = TRUE)
    
    df_coefs <- data.frame(
      Coeficiente = names(coefs_orig),
      Estimativa  = unname(coefs_orig),
      IC2.5       = ic_lower,
      IC97.5      = ic_upper,
      row.names   = NULL
    )
    
    tabela_coefs(df_coefs)
    
    # ==============
    # 2) Gera plots
    # ==============
    df_mod <- model.frame(mod)
    
    # Para evitar problemas se existirem Inf/NA, filtramos
    df_mod_filtrado <- df_mod %>%
      filter_all(all_vars(!is.na(.))) %>%
      filter_all(all_vars(!is.infinite(.)))
    
    # Plot Regressão
    output$plot_regressao <- renderPlot({
      if (nrow(df_mod_filtrado) == 0) {
        plot.new()
        title("Nenhum ponto disponível após transformações")
        return(NULL)
      }
      
      var_dep_name <- names(df_mod_filtrado)[1]
      var_ind_name <- names(df_mod_filtrado)[2]
      
      df_plot <- data.frame(
        dep   = df_mod_filtrado[[var_dep_name]],
        indep = df_mod_filtrado[[var_ind_name]]
      )
      
      gg <- ggplot(df_plot, aes(x = indep, y = dep)) +
        geom_point() +
        geom_smooth(method = "lm", formula = y ~ x) +
        labs(
          x = var_ind_name,
          y = var_dep_name,
          title = paste0("Ajuste: ", modelo_sel, " (subst.: ", subst_sel, ")",
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
          x = "Fitted Values",
          y = "Resíduos",
          title = paste("Resíduos vs. Fitted", 
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
          x = "Fitted Values",
          y = expression(sqrt("|Resíduo|")),
          title = paste("Scale-Location Plot",
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
}

shinyApp(ui = ui, server = server)
