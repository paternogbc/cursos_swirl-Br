# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque neste arquivo o código de inicialização.
hits <- read.csv(file.path(.get_course_path(), "Modelos_de_Regressao",
	"Respostas_de_Contagem", "leekGroupData.csv"), as.is=TRUE)
hits[,"date"] <- as.Date(hits[,"date"])
