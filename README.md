# Análises de Regressão por Mínimos Quadrados Ponderados

Este aplicativo Shiny foi desenvolvido para realizar **análises de regressão linear com transformações nos pesos dos resíduos** por meio do método de **Mínimos Quadrados Ponderados (WLS)**. Ele foi pensado para auxiliar nas calibrações laboratoriais aplicadas na **Perícia Oficial e Identificação Técnica do Estado de Mato Grosso (POLITEC/MT)**.

## Funcionalidades

- **Importação de planilhas Excel** com colunas `y`, `x` e `substancia`;
- **Análise automática de diversos modelos de ponderação**, incluindo:
  - Peso constante (1),
  - 1/x,
  - 1/x²,
  - 1/y,
  - 1/y²;
- **Opção de regressão robusta** (com o pacote `robustbase`);
- **Validação via holdout repetido** com métricas como RMSE, MAD e erro relativo;
- **Testes estatísticos**:
  - p-valor da regressão,
  - R²,
  - Teste de Breusch-Pagan (heterocedasticidade),
  - Teste de Shapiro-Wilk (normalidade dos resíduos);
- **Exportação dos resultados** em formato `.csv` e `.xlsx`;
- **Visualizações gráficas**:
  - Curva de regressão com intervalo de confiança,
  - Resíduos vs valores ajustados,
  - Gráfico escala-localização;
- Interface interativa com **resumo dos coeficientes selecionados**;
- Janela de informações sobre o sistema com autoria e link para repositório.

## Instalação

### Pré-requisitos

Certifique-se de ter o **R** (versão 4.0 ou superior) e o **RStudio** instalados.

### Passo a passo

1. Instale os pacotes necessários (caso ainda não os tenha):

```r
install.packages(c("shiny", "readxl", "openxlsx", "lmtest", "DT", 
                   "ggplot2", "dplyr", "boot", "robustbase"))
```

2. Clone este repositório ou baixe os arquivos:

```bash
git clone https://github.com/demusis/calibracao_laboratorio.git
```

3. Abra o arquivo `.R` principal no RStudio.

4. Execute o app com o botão **"Run App"** ou com:

```r
shiny::runApp()
```

## Estrutura esperada da planilha Excel

A planilha carregada deve conter obrigatoriamente as colunas:

- `x` – variável independente,
- `y` – variável dependente (resposta),
- `substancia` – identificador de cada substância analisada (poderão ser analisadas múltiplas substâncias no mesmo arquivo).

## Autores

- **Carlo Ralph De Musis**  
  Perito Oficial Criminal - POLITEC/MT  
  [GitHub](https://github.com/demusis)

- **Eguiberto Bernardes Fraga Júnior**  
  Perito Oficial Criminal - POLITEC/MT  
  Especialista em Estatística e Modelagem

---

© 2025 - POLITEC/MT. Todos os direitos reservados.
