simbias <- function(seed=8765){
  # A semente padrão garante um histograma bonito. Esse é o único
  # motivo para a lição exigir o argumento padrão, x1c <- simbias().
  # O efeito também aparece com outras sementes.
  set.seed(seed) 
  temp <- rnorm(100)
  # Ponto A
  x1 <- (temp + rnorm(100))/sqrt(2)
  x2 <- (temp + rnorm(100))/sqrt(2)
  x3 <- rnorm(100)
  # Função que simula a regressão de y em 2 variáveis.
  f <- function(k){
    # Ponto B
    y <- x1 + x2 + x3 + .3*rnorm(100)
    # Ponto C
    c(lm(y ~ x1 + x2)$coef[2],
       lm(y ~ x1 + x3)$coef[2])
  }
  # Ponto D
  sapply(1:150, f)
}

# Mostra o efeito de preditoras sem sentido na soma dos quadrados dos resíduos.
bogus <- function(){
  temp <- swiss
  # Acrescenta 41 colunas de preditoras aleatórias a uma cópia dos dados swiss.
  for(n in 1:41){temp[,paste0("random",n)] <- rnorm(nrow(temp))}
  # Define uma função que calcula o deviance da regressão de Fertility
  # em todas as preditoras até a coluna n. A função deviance(model) calcula
  # a soma dos quadrados dos resíduos do modelo dado como argumento.
  f <- function(n){deviance(lm(Fertility ~ ., temp[,1:n]))}
  # Aplica f aos dados de n=6, isto é, só as preditoras legítimas,
  # até n=47, isto é, com todas as preditoras sem sentido.
  rss <- sapply(6:47, f)
  # Mostra o resultado.
  plot(0:41, rss, xlab="Número de preditoras sem sentido", ylab="Soma dos quadrados dos resíduos",
       main="Soma dos quadrados dos resíduos nos dados swiss\ncom preditoras irrelevantes (sem sentido)",
       pch=21, bg='red')
}

# Faz histogramas que mostram o viés nas estimativas do coeficiente de
# uma preditora 1) quando falta uma preditora não correlacionada e
# 2) quando falta uma preditora correlacionada.
x1hist <- function(x1c){
  p1 <- hist(x1c[1,], plot=FALSE)
  p2 <- hist(x1c[2,], plot=FALSE)
  yrange <- c(0, max(p1$counts, p2$counts))
  plot(p1, col=rgb(0,0,1,1/4), xlim=range(x1c), ylim=yrange, xlab="Coeficiente estimado de x1", ylab="Frequência",
        main="Viés causado pela preditora omitida")
  plot(p2, col=rgb(1,0,0,1/4), xlim=range(x1c), ylim=yrange, add=TRUE)
  legend(1.1, 40, c("Sem x3, não correlacionada", "Sem x2, correlacionada"),
         fill=c(rgb(0,0,1,1/4), rgb(1,0,0,1/4)))
}
