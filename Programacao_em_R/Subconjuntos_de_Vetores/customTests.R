notify <- function() {
  e <- get("e", parent.frame())
  if(e$val == "Não") return(TRUE)
  
  good <- FALSE
  while(!good) {
    # Get info
    name <- readline_clean("Qual é o teu nome completo? ")
    address <- readline_clean("Qual é o email da pessoa que você gostaria de informar? ")
    
    # Repeat back to them
    message("\nAs informações estão corretas?\n")
    message("Seu nome: ", name, "\n", "Enviar para: ", address)
    
    yn <- select.list(c("Sim", "Não"), graphics = FALSE)
    if(yn == "Sim") good <- TRUE
  }
  
  # Get course and lesson names
  course_name <- attr(e$les, "course_name")
  lesson_name <- attr(e$les, "lesson_name")
  
  subject <- paste(name, "completou com sucesso: ", course_name, "-", lesson_name)
  body = ""
  
  # Send email
  swirl:::email(address, subject, body)
  
  hrule()
  message("Tentei criar um novo email com a seguinte informação:\n")
  message("Para: ", address)
  message("Assunto: ", subject)
  message("Mensagem: <empty>")
  
  message("\nSe isso não funcionou, você pode enviar o email manualmente.")
  hrule()
  
  # Return TRUE to satisfy swirl and return to course menu
  TRUE
}

readline_clean <- function(prompt = "") {
  wrapped <- strwrap(prompt, width = getOption("width") - 2)
  mes <- stringr::str_c("| ", wrapped, collapse = "\n")
  message(mes)
  readline()
}

hrule <- function() {
  message("\n", paste0(rep("#", getOption("width") - 2), collapse = ""), "\n")
}

