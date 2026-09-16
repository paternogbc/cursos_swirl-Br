makelms <- function(x1, x2, x3){
  # Simula uma variável dependente, y, igual a x1
  # mais um erro com distribuição normal de média 0 e
  # desvio-padrão .3.
  y <- x1 + rnorm(length(x1), sd = .3)
  # Obtém o coeficiente de x1 em 3 modelos lineares
  # aninhados: o primeiro só com a preditora x1,
  # o segundo com x1 e x2, o terceiro com x1, x2 e x3.
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
}

# Processo 1 de geração das preditoras.
rgp1 <- function(){
  print("Processando. Aguarde.")
  # número de observações por simulação
  n <- 100
  # número de simulações
  nosim <- 1000
  # semente, para reprodutibilidade
  set.seed(4321)
  # Ponto A
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  # Ponto B
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

# Processo 2 de geração das preditoras.
rgp2 <- function(){
  print("Processando. Aguarde.")
  # número de observações por simulação
  n <- 100
  # número de simulações
  nosim <- 1000
  # semente, para reprodutibilidade
  set.seed(4321)
  # Ponto C
  x1 <- rnorm(n)
  x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
  x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
  # Ponto D
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}
