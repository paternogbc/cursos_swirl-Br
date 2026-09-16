# Coloque o código de inicialização neste arquivo. As variáveis criadas
# aqui aparecem no ambiente de trabalho do usuário quando ele começa
# a lição.

data(InsectSprays); 
sA <- InsectSprays$count[InsectSprays$spray=="A"]
sB <- InsectSprays$count[InsectSprays$spray=="B"]
sC <- InsectSprays$count[InsectSprays$spray=="C"]
sD <- InsectSprays$count[InsectSprays$spray=="D"]
sE <- InsectSprays$count[InsectSprays$spray=="E"]
sF <- InsectSprays$count[InsectSprays$spray=="F"]


boxplot(count ~ spray, data = InsectSprays,
        xlab = "Tipo de inseticida", ylab = "Contagem de insetos",
        main = "Dados InsectSprays", varwidth = TRUE, col = "lightgray")


