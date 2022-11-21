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
lengths(filter(fitxer, PES > 60 & EDAT >= 20)) # 72 PERSONES QUE PESEN MÉS 60 KG I TENEN MÉS DE 20 ANYS
lengths(filter(fitxer, PES < 70 & EDAT <= 50)) # 65 PERSONES QUE PESEN MENYS DE 70 KG I TENEN COM A MOLT 50 ANYS


"EXERCICI 2"
"APARTAT A"

dau <- function(k) {
  if (k%%2)
    return ((1+2*k)/36)
  return ((k-1)/(5*k))
}

"APARTAT B"
x<-c(1,2,3,4,5,6)
prob1<-c(dau(1),dau(2),dau(3),dau(4),dau(5),dau(6))
# Funció de massa de probabilitat
plot (x, prob1,type="h",lwd=2,bty="n",las=1, xlim = c (1 ,6) , ylim = c (0 ,1), col= "red",ylab="Probabilitats",xlab="Valors del dau", main= "Funció de densitat")
# Funció de distribució
acumulacio1 <- cumsum (prob1)
s1 <- stepfun (x , c (0 , acumulacio1))
plot (s1 , verticals = FALSE,col= "red", main="Funció de distribució")
# Comparació entre les dues funcións de distribució
prob2<-rep(1/6,6)
acumulacio2 <- cumsum (prob2)
s2 <- stepfun (x , c (0 , acumulacio2))
plot (s1 , verticals = FALSE, col= "red", main="Funció de distribució")
lines(s2, verticals= FALSE, col="blue")
legend(x="bottomright",legend=c("Dau trucat","Dau normal"),fill = c("red","blue"),title="Tipus de daus")

"APARTAT C"
prob_p<-dau(2)+dau(4)+dau(6)
prob_s<-dau(1)+dau(3)+dau(5)
prob_gt4<-dau(5)+dau(6)
prob_lt5<-dau(1)+dau(2)+dau(3)+dau(4)

"APARTAT D"
# Apliquem la definició d'esperança del cas discret
E<-1*dau(1)+2*dau(2)+3*dau(3)+4*dau(4)+5*dau(5)+6*dau(6) # Esperança teòrica
V<-(1**2)*dau(1)+(2**2)*dau(2)+(3**2)*dau(3)+(4**2)*dau(4)+(5**2)*dau(5)+(6**2)*dau(6)-E**2 # Variancia teòrica

mostra<-sample(c(1,2,3,4,5,6), 300, prob=c(dau(1),dau(2),dau(3),dau(4),dau(5),dau(6)),replace=TRUE) # Creació de la mostra

varp <- function(x){
  mitjana <- mean(x)
  n <- length(x)
  sumatori <- sum((x - mitjana)**2)
  variacio <- 1/n * sumatori
  
  variacio
}
esp<-mean(mostra) # Esperança empírica
var<-varp(mostra) # Variància empírica

"APARTAT E"
p<-dau(1)*(dau(4)+dau(5)+dau(6)) + dau(2)*(dau(3)+dau(4)+dau(5)+dau(6)) + dau(3)*(dau(2)+dau(3)+dau(4)+dau(5)+dau(6))+dau(4)+dau(5)*(dau(1)+dau(2)+dau(3)+dau(4)+dau(5))+dau(6)*(dau(1)+dau(2)+dau(3)+dau(4)) 


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

"EXERCICI 4"
"APARTAT A"
# P(X<295) = P(X<=295)
p1<-pnorm(295,298,3)
# P(290<X<310)=P(X<=310)-P(X<=290)
p2<-pnorm(310,298,3)-pnorm(290,298,3)

"APARTAT B"
# 1P(X>=k)=0.05 --> P(X<=k)=1-0.05
q<-qnorm(0.95,298,3)

"APARTAT C"
# Una ampolla defectuosa depèn de y<-pnorm(295,298,3)
y<-pnorm(295,298,3)
# Y-> 2 ampolles de 6 són defectuoses
# Per tant Y~Binom(6,y). En aquest cas ens interessa P(Y>=2)=1-P(Y<2)=1-P(Y<=1)=1-pbinom(1,6,y)
Y<-1-pbinom(1,6,y)
# X~Binom(30,100,Y)
p_retorn<-dbinom(30,100,Y)

"APARTAT D"
# E(X)=np
E<-100*Y
# Var(X)=np(1-p)
Var<-100*Y*(1-Y)

"APARTAT E"
# Aproximació
p<-1- pnorm(29,24,sqrt(18.44682))  








