library(data.table)
library(ggplot2)
library(DataExplorer)

tamanho_da_amostra <- 10000

Base <- fread(input = paste0("data.csv"), data.table = FALSE, dec=".")
Base <- Base[0:tamanho_da_amostra, c('T_degC','Salnty','Depthm')]

# Visualizar Dataframe
View(Base)
DataExplorer::create_report(Base)

# Remover linhas com nulls
Base <- na.omit(Base)

# Gráfico
plot(Base, main="Dataset Scatterplot")

analisador <- function(x, y) {
  print(cor.test(x, y, conf.level=0.95))
  x_str <- deparse(substitute(x))
  y_str <- deparse(substitute(y))
  plot(
    x,
    y,
    main="Modelos sobre Dados",
    xlab=x_str,
    ylab=y_str
  )
  
  # Modelo Liear
  modelo_linear <- lm(y ~ x)
  print(">>>>>>>>>>>> Coeficientes do modelo linear <<<<<<<<<<<<<<<")
  print(coef(modelo_linear))
  abline(modelo_linear)
  print(">>>>>>>>>>>> Outras informações do modelo linear <<<<<<<<<<<<<<<")
  print(summary(modelo_linear))
  
  # Modelo polinomial
  modelo_poly = lm(y ~ poly(x, 3))
  print(">>>>>>>>>>>> Coeficientes do modelo polinomial <<<<<<<<<<<<<<<")
  print(coef(modelo_poly))
  sorted_index <- sort(x, index.return=T)$ix
  lines(
    x[sorted_index],
    predict(modelo_poly)[sorted_index],
    col='red',
    lwd=2
  )
  print(">>>>>>>>>>>> Outras informações do modelo polinomial <<<<<<<<<<<<<<<")
  print(summary(modelo_poly))
  
  # Análise de resíduos
  plot(
    predict(modelo_linear),
    rstandard(modelo_linear),
    main=paste("Análise de Resíduos Modelo Linear para ", x_str),
  )
  plot(
    predict(modelo_poly),
    rstandard(modelo_poly),
    main=paste("Análise de Resíduos Modelo Polinomial para ", x_str),
  )
}

"------------------"
"Analisando Salnty em relação a T_degC"
"------------------"
analisador(x=Base$Salnty, y=Base$T_degC)

"------------------"
"Analisando Depthm em relação a T_degC"
"------------------"
analisador(x=Base$Depthm, y=Base$T_degC)
