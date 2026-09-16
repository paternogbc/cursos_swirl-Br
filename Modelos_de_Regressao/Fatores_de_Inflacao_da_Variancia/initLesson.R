# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# Coloque neste arquivo o código de inicialização.
 file.copy(from=file.path(.get_course_path(),
 	"Modelos_de_Regressao", "Fatores_de_Inflacao_da_Variancia","vifSims.R"), 
                          to="vifSims.R")
 data(swiss)
 file.edit("vifSims.R")
 source("vifSims.R", local=TRUE)
