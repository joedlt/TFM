# Para obtener la nacionalidad de los clientes según su numero de tarjeta se obtiene un fichero TAF que se tiene que##
## limpiar y hacer un join  con la tabla ISO de paises para obtener el pais# 

BINES.CECA <- read.csv("I:/I7P/SEGMENTO AUTONOMOS Y COMERCIOS/Seguimiento Segmento Comercios/Informes Territoriales/Varios/TFM/BINES-CECA/BINES-CECA.TXT", header=FALSE, quote="", stringsAsFactors=FALSE)
bin<-BINES.CECA$V1
bin<-substr(bin,1,3)
BINES.CECA$v2<-bin
View(BINES.CECA)

##iso countries##
## se bajo de internet la tabla Iso de paises en un csv#
Countries<-read.csv("I:/I7P/SEGMENTO AUTONOMOS Y COMERCIOS/Seguimiento Segmento Comercios/Informes Territoriales/Varios/TFM/BINES-CECA/Countries.txt", header = TRUE, sep = ";", quote = "|",stringsAsFactors = FALSE,colClasses = "character")
View(Countries)
Countries_visa_nac<-subset(Countries, select = c(ALF,PAIS))
Countries_europay<-subset(Countries, select = c(COD, PAIS))

##Bines nacionales##

BIN_NAC<-BINES.CECA[(BINES.CECA$v2 =="BIN"),]
View(BIN_NAC)
tpvs<-substr(BIN_NAC$V1,10,10)
BIN_NAC$v3<-tpvs
binn<-substr(BIN_NAC$V1,4,9)
BIN_NAC$v4<-binn
View(BIN_NAC)
BIN_NAC$v5<-"ES"
View(BIN_NAC)
BINES_NACIONALES<-BIN_NAC[(BIN_NAC$v3=="T"),]
View(BINES_NACIONALES)
BINES_NACIONALES<-subset(BINES_NACIONALES, select = c(v4,v5))
library(plyr)
BINES_NACIONALES<-rename(BINES_NACIONALES, c("v4"="BIN", "v5"="ALF"))
View(BINES_NACIONALES)
BINES_NAC<-merge(x=BINES_NACIONALES, y=Countries_visa_nac,by = "ALF", all.x=TRUE)
View(BINES_NAC)
BINES_NAC_TOTAL<-subset(BINES_NAC,select =c(BIN,PAIS))
View(BINES_NAC_TOTAL)

##Bines Visa##

BIN_VISA<-BINES.CECA[(BINES.CECA$v2 =="BIV"),]
BINV<-substr(BIN_VISA$V1,4,9)
BIN_VISA$v3<-BINV
PAi<-substr(BIN_VISA$V1,23,24)
BIN_VISA$v4<-PAi
BIN_VISA<-subset(BIN_VISA, select = c(v3,v4))
BIN_VISA<-rename(BIN_VISA,c("v3"="BIN","v4"="ALF"))
BIN_VISA<-subset(BIN_VISA, ALF!="ES")
View(BIN_VISA)
BINES_VISA<-merge(x=BIN_VISA, y=Countries_visa_nac,by = "ALF", all.x=TRUE)
View(BINES_VISA)
BINES_VISA<-na.omit(BINES_VISA)
BINES_VISA_TOTAL<-subset(BINES_VISA, select = c(BIN,PAIS))
View(BINES_VISA_TOTAL)

##Bines Europay##


BIN_EUR<-BINES.CECA[(BINES.CECA$v2 =="BIE"),]
cod_p<-substr(BIN_EUR$V1,48,50)
BIN_EUR$v3<-cod_p
pos<-substr(BIN_EUR$V1,4,4)
BIN_EUR$v4<-pos
BIN_EUR<-BIN_EUR[(BIN_EUR$v4 == "P"),]
library(stringr)
bineu<-substr(BIN_EUR$V1,5,24)
bineu1<-str_extract(bineu,"^[0-9]+")
BIN_EUR$v5<-bineu1
View(BIN_EUR)
BIN_EUR<-subset(BIN_EUR, select = c(v3,v5))
BIN_EUR<-rename(BIN_EUR, c("v3"="COD", "v5"="BIN"))
BIN_EUR<-subset(BIN_EUR, COD!="724")
View(BIN_EUR)
BINES_EUR<- merge(x=BIN_EUR, y=Countries_europay, by="COD",all.x=TRUE)
BINES_EUR<-na.omit(BINES_EUR)
View(BINES_EUR)
BINES_EUR_TOTAL<-subset(BINES_EUR,select = c(BIN,PAIS))
View(BINES_EUR_TOTAL)

#unificar bines y pais#

BINES_TOTALES<-rbind(BINES_EUR_TOTAL,BINES_VISA_TOTAL,BINES_NAC_TOTAL)
View(BINES_TOTALES)
s<-BINES_TOTALES$BIN
# encriptación de los datos de BINES para poder unificarlos con datos sensibles de tarjetas##
# en este caso no pongo el comando usado mas no los valores de encriptación por seguridad de los datos#

s1<-chartr('0123456789', 'XXXXXXXXX', s)
BINES_TOTALES$BIN<-s1
write.table(BINES_TOTALES,"~/ejemploR/BINES_PAIS.txt",sep=";",quote = TRUE, row.names = FALSE, col.names = TRUE)
#El encriptamiento de los bines es secreto por tal motivo lo dejo en este caso con X 
# en las x se pone los caracteres que se cambian por los numeros.


##tabla de ISo moneda##

library(XML)
url<-("http://assemblysys.com/es/codigos-de-monedas-iso-4217/")
monedas<-readHTMLTable(url,which = 1, colClasses = "character", stringAsFactor=FALSE, encoding="UTF-8")
head(monedas, 10)
monedas<-rename(monedas, c("ISO 4217"="ALFCOD", "ISO 4217 numÃ©rico"= "NUM_COD"))
monedas<-subset(monedas, select = c(ALFCOD,NUM_COD,moneda))
View(monedas)
