# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque neste arquivo o código de inicialização. As variáveis criadas
# aqui vão aparecer no ambiente de trabalho do usuário quando ele
# começar a lição.

hunger <-read.csv(file.path(.get_course_path(),
	"Modelos_de_Regressao","Exemplos_de_Regressao_Multipla_3","hunger.csv"))
