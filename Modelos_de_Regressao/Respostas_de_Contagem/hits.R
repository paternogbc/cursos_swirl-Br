plot(visits ~ date, hits, main="Visitas por dia ao site do Leek Group", xlab="Data", ylab="Visitas", pch=21, bg='green')
lines(hits$date, predict(loess(visits ~ julian(date), hits, span=1.5)), lwd=5, col="black")
