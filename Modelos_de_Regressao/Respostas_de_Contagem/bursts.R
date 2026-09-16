plot(visits ~ date, hits, pch=21, bg='lightgreen', main="Picos de popularidade", xlab="Data", ylab="Visitas")
points(simplystats ~ date, hits, pch=21, bg='black')
lines(simplystats ~ date, hits, lwd=3)
legend('topleft', c("Visitas", "Visitas vindas do Simply Statistics"), pch=21, pt.bg=c("lightgreen", "black"), bg="white")
