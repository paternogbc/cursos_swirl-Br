# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque o código de inicialização neste arquivo. As variáveis criadas
# aqui aparecem no ambiente de trabalho do usuário quando ele começa
# a lição.

data(swiss); 
pairs(swiss, panel = panel.smooth, main = "Dados swiss", col = 3 + (swiss$Catholic > 50))

# Coloque o código de inicialização neste arquivo.
file.copy(from=file.path(.get_course_path(),
	"Modelos_de_Regressao", "Exemplos_de_Regressao_Multipla", "swissLMs.R"), 
          to="swissLMs.R")
file.edit("swissLMs.R")
source("swissLMs.R", local=TRUE)
