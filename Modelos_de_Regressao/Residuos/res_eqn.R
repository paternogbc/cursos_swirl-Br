#Estes são os vetores de alterações (ajustes)
sltweak <- c(.01, .02, .03, -.01, -.02, -.03) #um para a inclinação
ictweak <- c(.1, .2, .3, -.1, -.2, -.3)  #um para o intercepto
lhs <- numeric()
rhs <- numeric()
#o lado esquerdo da equação é a soma dos quadrados dos resíduos da linha de regressão alterada
for (n in 1:6) lhs[n] <- sqe(ols.slope+sltweak[n],ols.ic+ictweak[n])
#o lado direito da equação é a soma dos quadrados dos resíduos originais + a soma dos quadrados das duas alterações
for (n in 1:6) rhs[n] <- sqe(ols.slope,ols.ic) + sum(est(sltweak[n],ictweak[n])^2)
