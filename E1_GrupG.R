"EXERCICI 1, APARTAT A"
valoracions <- c(1,2,3,4,5,6,7,8,9,10) #VALORS DE LES VALORACIONS
frequencies <- c(2,8,5,13,20,37,50,68,42,19) #FREQÜÈNCIA DE LES VALORACIONS EN ORDRE
vectorvalors <- rep(valoracions, frequencies)
table(vectorvalors) #TAULA DE FREQÜÈNCIES ABSOLUTES
table(vectorvalors)/length(vectorvalors) #TAULA DE FREQÜÈNCIA RELATIVA
cumsum(table(vectorvalors)) #TAULA DE FREQÜÈNCIES ABSOLUTES ACOMULADA
cumsum(table(vectorvalors)/length(vectorvalors))#TAULA DE FREQÜÈNCIES RELATIVES ACOMULADA

"APARTAT B"
mean(vectorvalors) #MITJANA DE LA TAULA 
var(vectorvalors) #VARIÀNCIA CORREGIDA

variancia <- function(x){
  mitjana <- mean(x)
  n <- length(x)
  sumatori <- sum((x - mitjana)**2)
  variacio <- 1/n * sumatori
  
  variacio
}

variancia(vectorvalors) #VARIÀNCIA

