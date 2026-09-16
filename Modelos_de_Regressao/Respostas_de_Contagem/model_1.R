plot(visits ~ date, hits, 
     main="Visitas por dia ao site do Leek Group e média de visitas\npor dia estimada pela regressão de Poisson",
     xlab="Data", ylab="Visitas", pch=21, bg='green')
lines(hits$date, mdl$fitted.values, lwd=5, col="black")
