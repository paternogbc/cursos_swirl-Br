# Para que o swirl não repita a execução dos comandos de gráfico
AUTO_DETECT_NEWVAR <- FALSE

# Retorna TRUE se o usuário criou um modelo glm específico,
# com um nome específico.
creates_glm_model <- function(correctExpr){
  e <- get("e", parent.frame())
  # Recria o que o usuário fez
  eUsr <- cleanEnv(e$snapshot)
  mdlUsr <- eval(e$expr, eUsr)
  # Acrescenta o resultado, como lista, a e$delta para poder restaurar o progresso
  e$delta <- c(e$delta, as.list(eUsr))
  # Recria o que o usuário deveria ter feito
  eSw <- cleanEnv(e$snapshot)
  mdlSw <- eval(parse(text=correctExpr), eSw)
  # Verifica se o nome do modelo está correto
  if(length(ls(eSw)) > 0){
    nameGood <- sum(ls(eUsr) %in% ls(eSw)) & sum(ls(eSw) %in% ls(eUsr))
    # Se não estiver, aponta o erro de digitação
    if(!nameGood){
      swirl_out(paste0("Parece que você digitou errado o nome do modelo. Eu esperava ", ls(eSw), 
                       ", mas você aparentemente digitou ", ls(eUsr), "."))
      return(FALSE)
    }
  }
  # Verifica se os modelos são efetivamente iguais
  isTRUE(all.equal(as.vector(mdlUsr$coefficients), as.vector(mdlSw$coefficients))) &
    identical(mdlUsr$family$family, mdlSw$family$family) &
    isTRUE(all.equal(mdlUsr$fitted.values, mdlSw$fitted.values))
}

# Retorna TRUE se e$expr corresponde a alguma das expressões dadas
# (como texto) no argumento.
ANY_of_exprs <- function(...){
  e <- get("e", parent.frame())
  any(sapply(c(...), function(expr)omnitest(expr)))
}

# Obtém o estado do swirl
getState <- function(){
  # Enquanto o swirl está rodando, sua função de callback fica no topo da pilha de chamadas.
  # O estado do swirl, chamado e, fica guardado no ambiente dessa função.
  environment(sys.function(1))$e
}

# Obtém o valor que o usuário digitou diretamente ou que foi calculado
# pelo comando que ele digitou.
getVal <- function(){
  getState()$val
}

# Obtém a última expressão que o usuário digitou no console do R.
getExpr <- function(){
  getState()$expr
}
