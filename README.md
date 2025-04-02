# Painel Shiny para Análises de Regressão com Transformações

## Descrição
Este dashboard desenvolvido em R, com interface Shiny, facilita a realização de análises de regressão para calibrações laboratoriais. O painel permite executar múltiplos modelos com diferentes transformações das variáveis, regressões robustas, validação por holdout e intervalos de confiança baseados em bootstrap.

## Funcionalidades
- **Importação de Dados:** Aceita arquivos Excel contendo as colunas `y`, `x` e `substancia`.
- **Transformações Disponíveis:**
  - Linear simples (`y ~ x`)
  - Transformações não lineares (`y ~ 1/x`, `y ~ 1/x^2`, `1/y ~ x`, `1/y^2 ~ x`)
- **Regressão Robusta:** Opção para realizar análises usando métodos robustos (`lmrob`).
- **Validação de Modelos:** Partição holdout com controle da proporção de dados para treino e teste.
- **Bootstrap:** Cálculo de intervalos de confiança dos coeficientes usando método bootstrap com número ajustável de reamostragens.
- **Diagnóstico de Modelos:** Testes estatísticos e gráficos para avaliação dos modelos:
  - Teste de Shapiro-Wilk (normalidade dos resíduos)
  - Teste de Breusch-Pagan (heterocedasticidade)
  - Gráficos de Resíduos, Ajuste vs. Observado e Scale-Location.
- **Exportação de Resultados:** Resultados podem ser baixados em formatos CSV e Excel.

## Como utilizar
1. Execute o painel em um ambiente com R e Shiny instalados.
2. Carregue seu arquivo Excel através da interface.
3. Configure as opções (bootstrap, robustez e holdout).
4. Clique em "Executar Análises".
5. Avalie os resultados apresentados em tabelas e gráficos.

## Pré-requisitos
- R
- Pacotes R necessários:
  - `shiny`
  - `readxl`
  - `openxlsx`
  - `lmtest`
  - `DT`
  - `ggplot2`
  - `dplyr`
  - `boot`
  - `robustbase`

## Autores
- Carlo Ralph De Musis e Eguiberto Bernardes Fraga Júnior
- Eguiberto

**Última atualização:** 01/04/2025

