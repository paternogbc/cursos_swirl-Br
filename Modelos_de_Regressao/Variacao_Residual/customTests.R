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
