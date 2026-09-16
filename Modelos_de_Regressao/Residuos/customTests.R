# Retorna TRUE se o usuário criou um modelo lm específico,
# com um nome específico.
creates_lm_model <- function(correctExpr){
  e <- get("e", parent.frame())
  # Faz o que o usuário deveria ter feito
  eSw <- cleanEnv(e$snapshot)
  mdlSw <- eval(parse(text=correctExpr), eSw)
  # Recria o que o usuário fez
  eUsr <- cleanEnv(e$snapshot)
  mdlUsr <- eval(e$expr, eUsr)
  # Se o modelo correto tem nome:
  if(length(ls(eSw))>0){
    # Verifica se o nome do modelo está correto
    nameGood <- sum(ls(eUsr) %in% ls(eSw)) & sum(ls(eSw) %in% ls(eUsr))
    # Se não estiver, aponta o erro de digitação
    if(!nameGood){
      swirl_out(paste0("Parece que você digitou errado o nome do modelo. Eu esperava ", ls(eSw),
                       ", mas você aparentemente digitou ", ls(eUsr), "."))
      return(FALSE)
    } else {
      # Acrescenta o resultado, como lista, a e$delta para poder restaurar o progresso
      e$delta <- c(e$delta, as.list(eUsr))
    }
  }
  # Verifica se os modelos são efetivamente iguais
  isTRUE(all.equal(sort(as.vector(mdlUsr$coefficients)), sort(as.vector(mdlSw$coefficients)))) &
    isTRUE(all.equal(mdlUsr$fitted.values, mdlSw$fitted.values))
}


# Retorna TRUE se e$val é idêntico ao valor que seria
# criado pela expressão correta.
creates_val_identical_to <- function(correctExpr){
  e <- get("e", parent.frame())
  correctVal <- eval(parse(text=correctExpr), cleanEnv(e$snapshot))
  results <- expectThat(e$val,
                        is_identical_to_legacy(correctVal, label=correctExpr),
                        label=deparse(e$expr))
  return(results$passed)
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
