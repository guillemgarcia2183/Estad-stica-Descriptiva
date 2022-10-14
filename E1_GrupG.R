"EXERCICI 1"
"APARTAT A"
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

# FUNCIÓ PER CALCULAR LA VARIÀNCIA
variancia <- function(x){
  mitjana <- mean(x)
  n <- length(x)
  sumatori <- sum((x - mitjana)**2)
  variacio <- 1/n * sumatori
  
  variacio
}
variancia(bellavista) #VARIÀNCIA

sd(bellavista) #DESVIACIÓ TÍPICA CORREGIDA

# FUNCIÓ PER CALCULAR LA DESVIACIÓ TÍPICA
desviacio_tipica<-function(x){
  mitjana<-mean(x)
  n<- length(x)
  fdd<-(x-rep(mitjana,n))**2/n
  sum(fdd)
  res<-sqrt(fdd)
}

desviacio_tipica(bellavista) #DESVIACIÓ TÍPICA

"APARTAT C"
fitxer<-read.csv("R/bonambient.csv")
bonambient<-fitxer$valoracions
table(bonambient) #TAULA DE FREQÜÈNCIES ABSOLUTES
table(bonambient)/length(bonambient) #TAULA DE FREQÜÈNCIA RELATIVA
cumsum(table(bonambient)) #TAULA DE FREQÜÈNCIES ABSOLUTES ACUMULADA
cumsum(table(bonambient)/length(bonambient))#TAULA DE FREQÜÈNCIES RELATIVES ACUMULADA

mean(bonambient) #MITJANA 
median(bonambient) #MEDIANA
desviacio_tipica(bonambient) #DESVIACIÓ TÍPICA
variancia(bonambient) #VARIÀNCIA 


"APARTAT D"
# DIAGRAMES DE BARRES
x1<-table(bellavista)
x2<-table(bonambient)
barplot(x1, main="Hotel Bellavista", sub="Valoracions dels usuaris")
barplot(x2, main="Hotel Bonambient", sub="Valoracions dels usuaris")
# DIAGRAMES DE CAIXES
boxplot(bellavista, main="Hotel Bellavista")
boxplot(bonambient, main="Hotel Bonambient")
"Exercici 2"
"APARTAT A"
library(dplyr) # Per poder utilitzar la funció filter
df<-load("~/R/motos.RData")
# Separem les motos en dues taules segons la transmissió, i n'extreiem la columna preu
cadena<- (motos %>% filter(TRANSMISSIO=="Cadena"))$PREU
corretja<- (motos %>% filter(TRANSMISSIO=="Corretja"))$PREU
# MITJANES
mean(cadena)
mean(corretja)
# MEDIANES
median(cadena)
median(corretja)
# VARIÀNCIES
variancia(cadena)
variancia(corretja)
# DESVIACIÓ TÍPICA
desviacio_tipica(cadena)
desviacio_tipica(corretja)
# PREU MÍNIM
min(cadena)
min(corretja)
# PREU MÀXIM
max(cadena)
max(corretja)
"APARTAT B"
# Coeficient de variació = desviació típica/mitjana
