"EXERCICI 1, APARTAT A"
valoracions <- c(1,2,3,4,5,6,7,8,9,10) #VALORS DE LES VALORACIONS
frequencies <- c(2,8,5,13,20,37,50,68,42,19) #FREQÜÈNCIA DE LES VALORACIONS EN ORDRE
bellavista <- rep(valoracions, frequencies)

table(bellavista) #TAULA DE FREQÜÈNCIES ABSOLUTES
cumsum(table(bellavista)) #TAULA DE FREQÜÈNCIES ABSOLUTES ACOMULADES

round(table(bellavista)/length(bellavista), digits = 3) #TAULA DE FREQÜÈNCIA RELATIVA
cumsum(round(table(bellavista)/length(bellavista), digits = 3))#TAULA DE FREQÜÈNCIES RELATIVES ACOMULADES

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
  suma<-sum(fdd)
  res<-sqrt(suma)
  res
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
barplot(x1, main="Hotel Bellavista", xlab = "Valoracions", ylab = "Freqüències")
barplot(x2, main="Hotel Bonambient", xlab = "Valoracions", ylab = "Freqüències")
# DIAGRAMES DE CAIXES
boxplot(bellavista, main="Hotel Bellavista", ylab = "Valoracions")
boxplot(bonambient, main="Hotel Bonambient", ylab = "Valoracions")

"Exercici 2, APARTAT A"
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
CV_cad<-desviacio_tipica(cadena)/mean(cadena)
CV_cor<-desviacio_tipica(corretja)/mean(corretja)

"APARTAT C" 
boxplot(cadena, main="Motos amb transmissió per cadena")
boxplot(corretja, main="Motos amb transmissió per corretja")

"APARTAT D" 
x<-motos$CV
y<-motos$PREU
recta<-lm(y~x)
coef(recta)
plot(x,y, xlab="Potència de la moto en CV", ylab="Preu de la moto en €", col = " deepskyblue3 ")
abline(recta)

"APARTAT E"
# Selecció dels 3 primers valors
potencies<-motos$CV[1:3]
preus<-motos$PREU[1:3]
# Valors ajustats
va1<- round(cf[1]+cf[2]*potencies[1],2)
va2<- round(cf[1]+cf[2]*potencies[2],2)
va3<- round(cf[1]+cf[2]*potencies[3],2)
# Residus
r1<- round(va1-preus[1],2)
r2<- round(va2-preus[2],2)
r3<- round(va3-preus[3],2)
# Previsió del preu d'una moto de 150 CV
preu<- round(cf[1] + cf[2]*150,2)


"EXERCICI 3, APARTAT A"
x1 <- anscombe$x1
x2 <- anscombe$x2
x3 <- anscombe$x3
x4 <- anscombe$x4
y1 <- anscombe$y1
y2 <- anscombe$y2
y3 <- anscombe$y3
y4 <- anscombe$y4

#COMPROVACIÓ DE MITJANES X
mean(x1)
mean(x2)
mean(x3)
mean(x4)

#COMPROVACIÓ VARIÀNCIES X
variancia(x1)
variancia(x2)
variancia(x3)
variancia(x4)

#COMPROVACIÓ MITJANES Y
mean(y1)
mean(y2)
mean(y3)
mean(y4)

#COMPROVACIÓ VARIÀNCIES Y
variancia(y1)
variancia(y2)
variancia(y3)
variancia(y4)

"APARTAT B"
par(mfrow = c(2,2))
A <- plot(x1, y1, main = "Primera parella")
B <- plot(x2, y2, main = "Segona Parella")
C <- plot(x3, y3, main = "Tercera parella")
D <- plot(x4, y4, main = "Quarta parella")

"APARTAT C"
#COEFICIENT DE CORRELACIÓ
cor(x1,y1) #PRIMERA PARELLA
cor(x2,y2) #SEGONA PARELLA
cor(x3,y3) #TERCERA PARELLA
cor(x4,y4) #QUARTA PARELLA

#CREAR RECTES DE CADA PARELLA
recta1 <- lm (y1 ~ x1)
recta2 <- lm (y2 ~ x2)
recta3 <- lm (y3 ~ x3)
recta4 <- lm (y4 ~ x4)

#COEFICIENT DE DETERMINACIÓ
summary(recta1)
summary(recta2)
summary(recta3)
summary(recta4)

#CALCULAR LA RECTA DE REGRESSIÓ
coef (recta1)
coef (recta2)
coef (recta3)
coef (recta4)

#REPRESENTAIONS RECTES EN ELS DIAGRAMES DE DISPERSIÓ
par(mfrow = c(2,2))
plot(x1, y1, main = "Primera parella") 
abline (recta1)
plot(x2, y2, main = "Segona Parella")
abline (recta2)
plot(x3, y3, main = "Tercera parella")
abline (recta3)
plot(x4, y4, main = "Quarta parella")
abline (recta4)

"APARTAT E"
#TERCERA PARELLA SENSE CANVIS
par(mfrow = c(1,2))
plot(x3, y3, main = "Tercera parella")
abline (recta3)

#TERCERA PARELLA TREIENT LA TERCERA FILA 
base = anscombe[-3,]
x3_v2 <- base$x3
y3_v2 <- base$y3
recta3_v2 <- lm (y3_v2 ~ x3_v2)
plot(x3_v2, y3_v2, main = "Canviant la tercera fila")
abline(recta3_v2)

"EXERCICI 4, APARTAT A"
fitxer<-read.csv("R/sao-paulo-properties-april-2019.csv")
fitxer2<- (fitxer %>% filter(Negotiation.Type=="rent")) 
saopaulo <- (fitxer2 %>% filter(Property.Type=="apartment")) 

"APARTAT B"
saopaulo %>% arrange(Price)
#TROBAR LA FILA DEL PIS MÉS BARAT
nou_data <- subset(fitxer, preu == 480)
nou_data


"APARTAT C"
preu <- saopaulo$Price
mida <- saopaulo$Size
preu_condomini <- saopaulo$Condo
habitacions <- saopaulo$Rooms
ascensor <- saopaulo$Elevator
districte <- saopaulo$District

     
#ANÀLISI NUMÈRIC DEL PREU
mean(preu) #MITJANA
variancia(preu) #VARIÀNCIA
median(preu) #MEDIANA
desviacio_tipica(preu) #DESVIACIÓ TÍPICA

#GRÀFIQUES 
par(mfrow = c(2,2))
plot(mida,preu, main = 'Relació preu i mida del apartament', xlab = "mida (m**2)", ylab = "preu (R$)")
plot(preu_condomini,preu, main = 'Relació preu i preu de condomini del apartament', xlab = "condomini (R$)", ylab = "preu (R$)" )
plot(habitacions,preu, main = 'Relació preu i habitacions', xlab = "habitacions", ylab = "preu (R$)" )
plot(ascensor,preu, main = 'Relació preu i ascensor', xlab = "ascensor", ylab = "preu (R$)" )


"APARTAT D"
#RECTA DE REGRESSIÓ MIDA/PREU
par(mfrow = c(1,1))
recta1 <- lm (preu ~ mida)
plot(mida,preu, main = 'Relació preu i mida del apartament', xlab = "mida (m**2)", ylab = "preu (R$)")
abline(recta1)


"APARTAT E"
barri1 <- (saopaulo %>% filter(District =="São Rafael/São Paulo"))
barri2 <- (saopaulo %>% filter(District =="Anhanguera/São Paulo"))
barri3 <- (saopaulo %>% filter(District =="Cambuci/São Paulo"))

preu1 <- barri1$Price
preu2 <- barri2$Price
preu3 <- barri3$Price

#CÀLCULS DE CENTRE I DISPERSIÓ BARRI SAO RAFAEL
mitjana1 <- mean(preu1) #MITJANA
var1 <- variancia(preu1) #VARIÀNCIA
med1 <- median(preu1) #MEDIANA
des1 <- desviacio_tipica(preu1) #DESVIACIÓ TÍPICA

#CÀLCULS DE CENTRE I DISPERSIÓ BARRI ANHANGUERA
mitjana2 <- mean(preu2) #MITJANA
var2 <- variancia(preu2) #VARIÀNCIA
med2 <- median(preu2) #MEDIANA
des2 <- desviacio_tipica(preu2) #DESVIACIÓ TÍPICA

#CÀLCULS DE CENTRE I DISPERSIÓ BARRI CAMBUCI
mitjana3 <- mean(preu3) #MITJANA
var3 <- variancia(preu3) #VARIÀNCIA
med3 <- median(preu3) #MEDIANA
des3 <- desviacio_tipica(preu3) #DESVIACIÓ TÍPICA

mitjanes <- c(mitjana1,mitjana2,mitjana3)
variancies <- c(var1,var2,var3)
medianes <- c(med1,med2,med3)
desv <- c(des1,des2,des3)

#GRÀFIQUES
par(mfrow = c(2,2))
barplot(mitjanes, main = 'Comparació  de mitjanes entre barris de Sao Paulo', ylab = "Preu (R$)", names.arg = c("São Rafael", "Anhanguera", "Cambuci"), xlab = "Barris")
barplot(variancies, main = 'Comparació  de variàncies entre barris de Sao Paulo', ylab = "Preu (R$**2)", names.arg = c("São Rafael", "Anhanguera", "Cambuci"), xlab = "Barris")
barplot(medianes, main = 'Comparació  de medianes entre barris de Sao Paulo', ylab = "Preu (R$)", names.arg = c("São Rafael", "Anhanguera", "Cambuci"), xlab = "Barris")
barplot(desv, main = 'Comparació  de desviacions típiques entre barris de Sao Paulo', ylab = "Preu (R$)", names.arg = c("São Rafael", "Anhanguera", "Cambuci"), xlab = "Barris")


