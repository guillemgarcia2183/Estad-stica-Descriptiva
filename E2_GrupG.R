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



