# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque o código de inicialização neste arquivo. As variáveis criadas
# aqui aparecem no ambiente de trabalho do usuário quando ele começa
# a lição.
galton <- read.csv(file.path(.get_course_path(),
	"Modelos_de_Regressao", "Introducao", "galton.csv"))
gch <- galton$child
gpa <- galton$parent
gpa_nor <- (gpa - mean(gpa))/sd(gpa)
gch_nor <- (gch - mean(gch))/sd(gch)
#verifica se o usuário está no RStudio e se o pacote manipulate está disponível
maniflg <- find.package("manipulate",quiet=TRUE)
if  (isTRUE((Sys.getenv("RSTUDIO") == "1")&&(nchar(maniflg)>0 ))) {
  library(manipulate)
  fname <- file.path(.get_course_path(),
  	"Modelos_de_Regressao","Estimacao_por_Minimos_Quadrados","slopedemo.R")
} else {
  fname <- file.path(.get_course_path(),
  	"Modelos_de_Regressao","Estimacao_por_Minimos_Quadrados","slopedemo_no_mani.R")
}
file.edit(fname)
fname2 <- file.path(.get_course_path(),
	"Modelos_de_Regressao","Estimacao_por_Minimos_Quadrados","finalplot.R")
