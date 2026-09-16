# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque o código de inicialização neste arquivo.
galton <- read.csv(file.path(.get_course_path(),
	"Modelos_de_Regressao", "Introducao", "galton.csv"))
