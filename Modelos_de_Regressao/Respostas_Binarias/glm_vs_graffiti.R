plot(c(3,23,29,55), c(0.5, 0.5, 1.0, 1.0), type='l', lwd=5, col="purple", col.lab="purple", ylim=c(0.25,1),
     xlab="Pontos marcados pelos Ravens", ylab="Probabilidade de vitória dos Ravens", col.main="purple",
     main="Probabilidade de vitória dos Ravens vs pontos: estimativas de máxima\nverossimilhança do GLM comparadas às estimativas grosseiras")
lines(mdl$data$ravenScore, mdl$fitted.values, lwd=5, col="black")
legend('bottomright', c("Estimativas grosseiras", "Estimativas de máxima verossimilhança do GLM"), lwd=5, lty=1,
                        col=c("purple", "black"))
