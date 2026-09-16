# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

galton <- read.csv(file.path(.get_course_path(),
	"Modelos_de_Regressao","Introducao", "galton.csv"))
est <- function(slope, intercept)intercept + slope*galton$parent
sqe <- function(slope, intercept)sum( (est(slope, intercept)-galton$child)^2)
attenu <- datasets::attenu
fname <- paste(.get_course_path(),
	"Modelos_de_Regressao","Residuos","res_eqn.R",sep="/")
