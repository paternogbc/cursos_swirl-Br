makelms <- function(){
  # Guarda o coeficiente de modelos lineares com diferentes variáveis independentes
  cf <- c(coef(lm(Fertility ~ Agriculture, swiss))[2], 
          coef(lm(Fertility ~ Agriculture + Catholic,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination,swiss))[2],
          coef(lm(Fertility ~ Agriculture + Catholic + Education + Examination +Infant.Mortality, swiss))[2])
  print(cf)
}

# Processo 1 de geração de preditoras.
rgp1 <- function(){
  print("Processando. Aguarde.")
  # número de observações por simulação
  n <- 100
  # número de simulações
  nosim <- 1000
  # define a semente, para reprodutibilidade
  set.seed(4321)
  # Ponto A:
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  # Ponto B:
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}

# Processo 2 de geração de preditoras.
rgp2 <- function(){
  print("Processando. Aguarde.")
  # número de observações por simulação
  n <- 100
  # número de simulações
  nosim <- 1000
  # define a semente, para reprodutibilidade
  set.seed(4321)
  # Ponto C:
  x1 <- rnorm(n)
  x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
  x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2)
  # Ponto D:
  betas <- sapply(1 : nosim, function(i)makelms(x1, x2, x3))
  round(apply(betas, 1, var), 5)
}
