# Para que o swirl não repita a execução dos comandos de gráfico
AUTO_DETECT_NEWVAR <- FALSE

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
