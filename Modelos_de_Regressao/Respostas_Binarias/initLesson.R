# Para compatibilidade com o swirl 2.2.21
.get_course_path <- function(){
  tryCatch(swirl:::swirl_courses_dir(),
           error = function(c) {file.path(find.package("swirl"),"Courses")}
  )
}

# dados dos Ravens
ravenData <- read.csv(file.path(.get_course_path(), 
                                 "Modelos_de_Regressao", "Respostas_Binarias", "ravens_data.csv"))
ravenData <- ravenData[order(ravenData$ravenScore), 1:3]
rownames(ravenData) <- NULL
