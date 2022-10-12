"EXERCICI 1, APARTAT A"
valoracions <- c(1,2,3,4,5,6,7,8,9,10) #VALORS DE LES VALORACIONS
frequencies <- c(2,8,5,13,20,37,50,68,42,19) #FREQÜÈNCIA DE LES VALORACIONS EN ORDRE
bellavista <- rep(valoracions, frequencies)
table(bellavista) #TAULA DE FREQÜÈNCIES ABSOLUTES
table(bellavista)/length(bellavista) #TAULA DE FREQÜÈNCIA RELATIVA
cumsum(table(bellavista)) #TAULA DE FREQÜÈNCIES ABSOLUTES ACUMULADA
cumsum(table(bellavista)/length(bellavista))#TAULA DE FREQÜÈNCIES RELATIVES ACUMULADA

"APARTAT B"
mean(bellavista) #MITJANA DE LA TAULA 
median(bellavista) #MEDIANA DE LA TAULA
var(bellavista) #VARIÀNCIA CORREGIDA

variancia <- function(x){
  mitjana <- mean(x)
  n <- length(x)
  sumatori <- sum((x - mitjana)**2)
  variacio <- 1/n * sumatori
  
  variacio
}
variancia(bellavista) #VARIÀNCIA

sd(bellavista) #DESVIACIÓ TÍPICA CORREGIDA

desv<-function(x){
  mitjana<-mean(x)
  n<- length(x)
  fdd<-(x-rep(mitjana,n))**2/n
  sum(fdd)
  res<-sqrt(fdd)
}
desv(bellavista) #DESVIACIÓ TÍPICA

"APARTAT C"
fitxer<-read.csv("R/bonambient.csv")
bonambient<-fitxer$valoracions
table(bonambient) #TAULA DE FREQÜÈNCIES ABSOLUTES
table(bonambient)/length(bonambient) #TAULA DE FREQÜÈNCIA RELATIVA
cumsum(table(bonambient)) #TAULA DE FREQÜÈNCIES ABSOLUTES ACUMULADA
cumsum(table(bonambient)/length(bonambient))#TAULA DE FREQÜÈNCIES RELATIVES ACUMULADA

mean(bonambient) #MITJANA 
median(bonambient) #MEDIANA
sd(bonambient) #DESVIACIÓ TÍPICA
var(bonambient) #VARIÀNCIA 


"APARTAT D"
# DIAGRAMES DE BARRES
x1<-table(bellavista)
x2<-table(bonambient)
barplot(x1, main="Hotel Bellavista", sub="Valoracions dels usuaris")
barplot(x2, main="Hotel Bonambient", sub="Valoracions dels usuaris")
# DIAGRAMES DE CAIXES
boxplot(bellavista, main="Hotel Bellavista")
boxplot(bonambient, main="Hotel Bonambient")
