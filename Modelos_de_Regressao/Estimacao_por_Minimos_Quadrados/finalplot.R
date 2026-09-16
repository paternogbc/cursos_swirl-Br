#desenha os dados originais de Galton, com pontos maiores para os valores mais frequentes
y <- galton$child
x <- galton$parent
freqData <- as.data.frame(table(galton$child, galton$parent))
names(freqData) <- c("child", "parent", "freq")
plot(as.numeric(as.vector(freqData$parent)), 
     as.numeric(as.vector(freqData$child)), 
     pch = 21, col = "black", bg = "lightblue",
     cex = .07 * freqData$freq, xlab = "parent", ylab = "child")

#linha de regressão original: filhos como resposta, pais como preditora
abline(mean(y) - mean(x) * cor(y, x) * sd(y) / sd(x), #intercepto
       sd(y) / sd(x) * cor(y, x),  #inclinação
       lwd = 3, col = "red")

#nova linha de regressão: pais como resposta, filhos como preditora
abline(mean(y) - mean(x) * sd(y) / sd(x) / cor(y, x), #intercepto
       sd(y) / cor(y, x) / sd(x), #inclinação
       lwd = 3, col = "blue")

#supõe correlação igual a 1, então a inclinação é a razão dos desvios-padrão
abline(mean(y) - mean(x) * sd(y) / sd(x), #intercepto
       sd(y) / sd(x),  #inclinação
       lwd = 2)
points(mean(x), mean(y), cex = 2, pch = 19) #ponto grande na interseção