# Faz a regressão da variável dada contra a preditora dada,
# suprimindo o intercepto, e devolve o resíduo.
regressOneOnOne <- function(predictor, other, dataframe){
  # Ponto A. Cria uma fórmula como Girth ~ Height -1
  formula <- paste0(other, " ~ ", predictor, " - 1")
  # Usa a fórmula numa regressão e devolve o resíduo.
  resid(lm(formula, dataframe))
}

# Elimina a preditora indicada do data frame fazendo a
# regressão de todas as outras variáveis contra essa preditora
# e devolvendo um data frame com os resíduos
# dessas regressões.
eliminate <- function(predictor, dataframe){
  # Encontra os nomes de todas as colunas, exceto a preditora.
  others <- setdiff(names(dataframe), predictor)
  # Calcula os resíduos de cada uma na regressão contra a preditora dada
  temp <- sapply(others, function(other)regressOneOnOne(predictor, other, dataframe))
  # sapply devolve uma matriz de resíduos; converte em data frame e devolve.
  as.data.frame(temp)
}
