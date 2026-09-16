# Para que o swirl não repita a execução dos comandos de gráfico
AUTO_DETECT_NEWVAR <- FALSE

# Retorna TRUE se e$expr corresponde a alguma das expressões dadas
# (como texto) no argumento.
ANY_of_exprs <- function(...){
  e <- get("e", parent.frame())
  any(sapply(c(...), function(expr)omnitest(expr)))
}

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

# O código a seguir acrescenta um recurso que restaura os gráficos e
# outros efeitos colaterais das expressões digitadas pelo usuário.
# Ele não preserva a ordem original dos gráficos, porque todos os
# gráficos produzidos pelas questões do tipo figure são restaurados
# depois dos gráficos restaurados por este código.
#
# Uma solução adequada vai exigir pequenas mudanças no núcleo do swirl.
# Este remendo está aqui para nos lembrar da natureza exata do problema.

# Esta função é chamada quando este arquivo é carregado com source.
# Nesse momento, e está cinco quadros acima na pilha de chamadas.
restore_expr <- function(){
  e <- get("e", parent.frame(5))
  if(exists("plotexpr", e, inherits=FALSE)){
    for(expr in e$plotexpr){
      eval(expr)
    }
  }
}

# Chama restore_expr quando este arquivo é carregado com source.
restore_expr()

# Esta função é um teste personalizado, chamado sempre que aparece
# como AnswerTest. Nesse momento, e está um quadro acima na
# pilha de chamadas.
save_expr <- function(){
  e <- get("e", parent.frame())
  if(!exists("plotexpr", e, inherits=FALSE))e$plotexpr <- list()
  n <- length(e$plotexpr)
  e$plotexpr[[n+1]] <- e$expr
  return(TRUE)
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
