"GUILLEM GARCIA -> 1636279"
"MARTÍ LLINÉS -> 1637804"

"EXERCICI 1"

"APARTAT A"
library(dplyr)
#OBRIM EL FITXER CSV
fitxer <- read.csv("C:/Users/garci/OneDrive/Escritorio/ESTADÍSTICA/R/SEGONA ENTREGA/enquesta.csv")
"FILTREM LES DADES PER VEURE "
#filter(fitxer, TABAC == 1)
lengths(filter(fitxer, TABAC == 1)) #44 FUMEN, PER TANT 76 NO FUMEN

#filter(fitxer, OCI == 4)
lengths(filter(fitxer, OCI == 4)) #30 FAN ESPORT, 90 NO FAN ESPORT

"APARTAT B"
#filter(fitxer, ALTURA > 160 & SEXE == "d")
lengths(filter(fitxer, SEXE == "d")) #57 DONES 
lengths(filter(fitxer, ALTURA > 160 & SEXE == "d")) #43 DONES FAN MÉS DE 160 CM

#filter(fitxer, SEXE == "d" & (OCI == 1 | OCI == 2)
lengths(filter(fitxer, SEXE == "d" & (OCI == 1 | OCI == 2))) #24 DONES TENEN D'OCI ORDINADOR O TELEVISIÓ

"APARTAT C"
#NOMBRE HOMES -> 120 - 57 = 63
lengths(filter(fitxer, SEXE == "h" & TABAC == 1)) #23 HOMES FUMEN
lengths(filter(fitxer, TABAC == 1 & SEXE == "h" & OCI == 4 )) #6 HOMES FUMEN I FAN ESPORT
lengths(filter(fitxer, TABAC == 1)) #44 FUMEN, PER TANT 76 NO FUMEN


"APARTAT D"
lengths(filter(fitxer, OCI == 4 & TABAC == 0)) #22 PERSONES FAN ESPORT I NO FUMEN
lengths(filter(fitxer, (OCI == 2 | OCI == 3) & TABAC == 1)) #27 PERSONES FUMEN I TENEN OCI ORDINADOR O MÚSICA/LECTURA

"APARTAT E"
lengths(filter(fitxer, PES > 60 & EDAT >= 20)) #72 PERSONES QUE PESEN MÉS 60 KG I TENEN MÉS DE 20 ANYS
lengths(filter(fitxer, PES < 70 & EDAT <= 50))# 65 PERSONES QUE PESEN MENYS DE 70 KG I TENEN COM A MOLT 50 ANYS



"EXERCICI 3"
"APARTAT A"
#REPRESENTACIÓ FUNCIÓ DE DENSITAT
x <- seq(1,3, by = 0.005) 
y <- 1/4 * ((x -1)^3)
plot(x,y, type = "l", main = "FUNCIÓ DE DENSITAT", ylab = "f(x)")

#CÀLCUL DE LA FUNCIÓ DE DISTRIBUCIÓ
xdis <- seq(1,3, by = 0.005)
ydis <- 1/16 * ((x-1)^4)
plot(xdis,ydis, type = "l", main = "FUNCIÓ DE DISTRIBUCIÓ", xlab = "x", ylab = "F(x)")

"APARTAT B"
u <- runif(80000) #SIMULACIÓ AMB N = 80.000
y <- (16*u)^(1/4) + 1 #INVERSA DE LA FUNCIÓ DE DISTRIBUCIÓ
#HISTOGRAMA
hist(y, freq = FALSE, main = "HISTOGRAMA EN N = 80.000", xlab= "F_inversa(u)", ylab = "Densitat")
#DIBUIXEM TAMBÉ LA FUNCIÓ DE DENSITAT SOBRE L'HISTOGRAMA
z <- seq(1,3, by = 0.005) 
t <- 1/4 * ((z-1)^3)
lines(z,t)

"APARTAT C"
mitjana_empirica <- mean(y) #MITJANA

varp <- function(x){
  mitjana <- mean(x)
  n <- length(x)
  sumatori <- sum((x - mitjana)**2)
  variacio <- 1/n * sumatori
  
  variacio
}
varp_empirica <- varp(y) #VARIÀNCIA (NO CORREGIDA)
  
mitjana_empirica
varp_empirica


"APARTAT D"
#REPRESENTEM LA DISTRIBUCIÓ NORMAL (0,1) EN UNA MOSTRA DE N = 80.000
dnormal <- rnorm(80000,0,1)
hist(dnormal, freq=FALSE, main="Histograma de la distribució N(0,1)")
#COMPROVACIONS
mean(dnormal)
var(dnormal)

#APLIQUEM EL TEOREMA CENTRAL DEL LÍMIT EN LA MOSTRA DE L'APARTAT B
n <- 200
mu <- mitjana_empirica
sigma <- sqrt(varp_empirica)
matriu <- matrix(y, nrow=400, ncol = 200)
vector_mitjanes <- apply(matriu,1,mean)

resultat <- (vector_mitjanes - mu)/(sigma/sqrt(n))
hist(resultat, freq=FALSE, main="Histograma aplicant teorema central del límit", ylab="Densitat")
#COMPROVACIONS
mean(resultat)
varp(resultat)

"APARTAT E"
#VEIEM QUIN ÉS EL VALOR T A PARTIR DE LA FUNCIÓ QUANTILA 
valor <- qnorm(0.8, mitjana_empirica, varp_empirica)
valor

#VEIEM QUIN ÉS EL TOTAL DE MOSTRES QUE SON MENOR QUE EL VALOR
sumatori <- 0
for(i in seq_along(y)){
  if (y[[i]] < valor){
    sumatori <- sumatori + 1}
}
sumatori


