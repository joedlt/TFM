# limpieza de direcciones de los comercios del banco, ya que se obtiene segun lo que digita libremente el personal de oficina#
# sin ningun tipo de formulario, se hace la limpieza para poder obtener su geolocalizacion#

dircom <- read.csv2 ("~/ejemploR/dircom.txt", sep=";",header = TRUE,quote = "\"", encoding = "UTF-8 BOM", colClasses = "character")
View(dircom)
via<-dircom$des_resto_via
via<-as.character(via)
library(stringr)
via1<-gsub("\\([^()]+\\)", "", via)
via1<-gsub("[[:punct:]]", " ", via1)
via1<-gsub("^[[:space:]]","", via1)
via2<-gsub("[[:space:]][0-9]+|[[:space:]][0-9]+[A-Z]+", "", via1)
via2<-gsub("[[:space:]]$", "", via2)
num_via<-str_extract(via1,"[[:space:]][0-9]+|[[:space:]][0-9]+[A-Z]+")
ult<-str_extract(via2, "[[:space:]][A-Z]+$")
table(ult)
via2<-gsub("[[:space:]](A)$|[[:space:]](BA)$|[[:space:]](BAJ)$|[[:space:]](BAJO)$|[[:space:]](BAJOS)$|[[:space:]](BAIXOS)$|[[:space:]](ESQ)$|[[:space:]](DCH)$|[[:space:]](DCHA)$|[[:space:]](DER)$|[[:space:]](DERECHA)$|[[:space:]](DESP)$|[[:space:]](DRCHA)$|[[:space:]](IZ)$|[[:space:]](IZD)$|[[:space:]](IZDA)$|[[:space:]](IZQ)$|[[:space:]](IZQUIERDA)$|[[:space:]](IZQUIERDO)$|[[:space:]](LOC)$|[[:space:]](LOCA)$|[[:space:]](LOCAL)$|[[:space:]](LOCALES)$|[[:space:]](PTA)$|[[:space:]](PTAL)$|[[:space:]](PTO)$|[[:space:]](PTOS)$|[[:space:]](PLANTA)$|[[:space:]](PLT)$|[[:space:]](PLAZ)$|[[:space:]](PLAZA)$|[[:space:]](PLC)$|[[:space:]](PLT)$|[[:space:]](PLTA)$|[[:space:]](SN)$","",via2)
head(via2, 60)
head(via ,60)
via3<-gsub("[[:space:]](IZQUIER)$|[[:space:]](IZQUIERDA)$|[[:space:]](DERECHA)$|[[:space:]](N)[[:space:]](EDF)|[[:space:]](ESC)|[[:space:]](NUM)|[[:space:]](NUN)$|[[:space:]](S)[[:space:]](N)|[[:space:]][A-Z]$|[[:space:]](L)[0-9]+|[[:space:]](E)[[:space:]]|[[:space:]](EDIF)|[[:space:]](PTO)|[[:space:]](B)[[:space:]]|[[:space:]](B)$|[[:space:]](BJS)|[[:space:]](N)[[:space:]](BJS)|[[:space:]](N)[[:space:]](BJ)|[[:space:]](BJ)|[[:space:]](BAJ)|[[:space:]](BAJOS)|[[:space:]](BJOS)|[[:space:]](BAJO)|[[:space:]](BJO)|[[:space:]](LOCAL)|[[:space:]](LOCAL)$|[[:space:]](LOC)|[[:space:]](S)[[:space:]]||[[:space:]](N)[[:space:]]|[[:space:]](N)$","",via2)
via3<-gsub("^[[:space:]]", "", via3)
pr<-str_extract(via3,"^[A-Z]+[[:space:]]")
table(pr)
via4<-gsub("^(A)[[:space:]]|^(AV)[[:space:]]|^(AVD)[[:space:]]|^(AVDA)[[:space:]]|^(AVDE)[[:space:]]|^(AVE)[[:space:]]|^(AVENIDA)[[:space:]]|^(AVGDA)[[:space:]]|^(C)[[:space:]]|^(CAFET)[[:space:]]|^(CAFETERIA)[[:space:]]|^(CALLE)[[:space:]]|^(CALLEJON)[[:space:]]|^(CAMI)[[:space:]]|^(CARREFOUR)[[:space:]]|^(CARRER)[[:space:]]|^(CARRERA)[[:space:]]|^(CARRETERA)[[:space:]]|^(CC)[[:space:]]|^(CL)[[:space:]]|^(CR)[[:space:]]|^(CRDE)[[:space:]]|^(CRTA)[[:space:]]|^(CRTRA)[[:space:]]|^(CTA)[[:space:]]|^(CTDE)[[:space:]]|^(CT)[[:space:]]|^(CTRA)[[:space:]]|^(CTRO)[[:space:]]|^(EDIF)[[:space:]]|^(ESQ)[[:space:]]|^(PASAJE)[[:space:]]|^(PASEO)[[:space:]]|^(PASSATGE)[[:space:]]|^(PASSEIG)[[:space:]]|^(PGEL)[[:space:]]|^(PJE)[[:space:]]|^(PL)[[:space:]]|^(PLAZA)[[:space:]]|^(PLZ)[[:space:]]|^(PLZA)[[:space:]]|^(POL)[[:space:]]|^(POLIG)[[:space:]]|^(POLIGONO)[[:space:]]|^(PQUE)[[:space:]]|^(PSJE)[[:space:]]|^(PTO)[[:space:]]|^(PZ)[[:space:]]|^(PZA)[[:space:]]|^(RD)[[:space:]]|^(UB)[[:space:]]|^(URBANIZAZION)[[:space:]]|^(V)[[:space:]]|^(XX)[[:space:]]|^(XXCRTA)[[:space:]]|^(XXCTRA)[[:space:]]|","",via3)
via4<-gsub("^[[:space:]]", "", via4)
pr_iva4<-str_extract(via4,"^[A-Z]+[[:space:]]" )
table(pr_iva4)
via4<-gsub("^(A)[[:space:]]|^(AUTOPISTA)[[:space:]]||^(AV)[[:space:]]|^(AVAD)[[:space:]]|^(AVADA)[[:space:]]|^(AVAVDA)[[:space:]]|^(AVD)[[:space:]]|^(AVDA)[[:space:]]|^(AVDEL)[[:space:]]|^(AVEL)[[:space:]]|^(AVENIDAD)[[:space:]]|^(AVING)[[:space:]]|^(AVINGUDA)[[:space:]]|^(C)[[:space:]]|^(CALLEJON)[[:space:]]|^(CAMI)[[:space:]]|^(CARRERA)[[:space:]]|^(CARRETERA)[[:space:]]|^(CLCTRA)[[:space:]]|^(CLDE)[[:space:]]|^(CLR)[[:space:]]|^(CM)[[:space:]]|^(CMCAMINO)[[:space:]]|^(CMDE)[[:space:]]|^(CMNO)[[:space:]]|^(CMO)[[:space:]]|^(CNO)[[:space:]]|^(CR)[[:space:]]|^(CRA)[[:space:]]|^(CT)[[:space:]]|^(CTRA)[[:space:]]|^(CTRO)[[:space:]]|^(LOCAL)[[:space:]]|","", via4)
via5<-gsub("^[[:space:]]","",via4)
pr_via5<-str_extract(via5,"^[A-Z]+[[:space:]]")
table(pr_via5)
via5<-gsub("^(AVR)[[:space:]]|^(CAMINO)[[:space:]]|^(CCALLEJON)[[:space:]]|^(CL)[[:space:]]|^(PASAJE)[[:space:]]|^(PLAZA)[[:space:]]|^(PLZ)[[:space:]]|^(PS)[[:space:]]|^(PSEO)[[:space:]]|^(PSTA)[[:space:]]|^(URBANIZACION)[[:space:]]|","", via5)
via6<-gsub("^[[:space:]]","",via5)
pr_via6<-str_extract(via6, "^[A-Z]+[[:space:]]")
table(pr_via6)
via6<-gsub("^(AVM)[[:space:]]|^(AVRG)[[:space:]]|^(PD)[[:space:]]|^(POLIGONO)[[:space:]]|^(PZA)[[:space:]]|^(PL)[[:space:]]|","",via6)
via7<-gsub("^[[:space:]]", "", via6)
pr_via7<-str_extract(via7,"^[A-Z]+[[:space:]]")
table(pr_via7)
via8<-gsub("^(AV)|^(CL)|^(CT)|^(PLZ)|^(P)[[:space:]]|^(PS)|^(XX)|^(XX)|^(XXC)[[:space:]]|^(RD)|^(CARREFOUR)[[:space:]]^(AV)|^(C)[[:space:]]|","", via7)
pr_via8<-str_extract(via8,"^[A-Z]+[[:space:]]")
table(pr_via8)
via9<-gsub("(CARREFOUR)","",via8)
pr_via9<-str_extract(via9,"^[A-Z]+[[:space:]]")
table(pr_via9)
via9<-gsub("^(CC)","C",via9)
pr_via9<-str_extract(via9,"^[A-Z]+[[:space:]]")
table(pr_via9)
via10<-gsub("^(EMENTES)","CLEMENTES",via9)
via10<-gsub("^(ERROES)","AVERROES", via10)
via10<-gsub("^(ILA)","AVILA",via10)
via10<-gsub("^(INYO)","AVINYO", via10)
head(via10,60)
via10<-gsub("^[[:space:]]","", via10)
via10<-gsub("[[:space:]]$|[[:space:]][[:space:]]$","", via10)
pobn<-gsub("\"\"|\"", "", dircom$X.Nombre_poblacion.)
cp<-gsub("\"\"|\"", "", dircom$X.numcod_postal.)
via_mcp<-paste(via10,pobn,cp)
head(via10,60)
tail(via10,60)
dircom$dir_corr<-via10
dircom$num_via<-num_via
dircom$via_mcp<-via_mcp
num<-gsub("^[[:space:]]", "", dircom$num_via)
cod_p<-gsub("[[:space:]]$", "", dircom$numcod_postal)

# se crea un variable con la infomacion necesaria para crea el KLM que pueda obtenerlas geolocalizaciones pero falta tipo de via#

dir_geo1<-ifelse(is.na(dircom$num_via),paste0(dircom$dir_corr,",",cod_p," ",dircom$Nombre_poblacion),paste0(dircom$dir_corr,",",num,",",cod_p," ",dircom$Nombre_poblacion))
head(dir_geo1, 60)
tail(dir_geo1,60)
dircom$dir_geo1<-dir_geo1
View(dircom)


#tipo de via la busco en las direcciones del INE vias por codigo censal previa limpieza de elementos estraños#
# tablas TRAMOS-NA y VIAS-NAL donde se hace unos tipos de codificacion y joins en Access incluyo sql de las consultas para obtener la siguiente tabla##

codigo_censal<-read.table("I:/I7P/SEGMENTO AUTONOMOS Y COMERCIOS/Seguimiento Segmento Comercios/Informes Territoriales/Varios/TFM/Codigos Censales.txt", header = TRUE, sep = ";",quote ="\"", colClasses = "character")
View(codigo_censal)
library(stringr)
via<-codigo_censal$Nombre.via
via<-as.character(via)
via1<-gsub("\\([^()]+\\)", "", via)
via1<-gsub("\"\"|\"", "", via1)
via2<-str_extract(via, "\\([^()]+\\)")
via2<-gsub("[\\()\\]","",via2)
table(via2)
via22<-gsub("(NO OFICIAL)|(NO OFI)|(NO)|(NO OFIC.)|(N.O.)|(N.O)",NA,via2)
table(via22)
via3<-ifelse(is.na(via22), via1, paste(via22,via1))
table(via3)
poblacion<-gsub("\"\"|\"", "", codigo_censal$X.Nombre.poblacion.)
tipo_via<-gsub("\"\"|\"", "", codigo_censal$X.Tipo.via.)
co_postal<-gsub("\"\"|\"", "", codigo_censal$X.Codigo.Postal.)
codigo_censal$nombre_corr_via<-via3
via_total<-paste(tipo_via, via3, poblacion, co_postal)
via_m<-paste(via3,poblacion,co_postal)
codigo_censal$via_m<-via_m
codigo_censal$via_total_corr<-via_total



# se hace un leftjoin entre las dos tablas para buscar el tipo de via#

dircom_geo<-merge(x=dircom, y=codigo_censal, by.x = c("numcod_postal","dir_corr"), by.y = c("Codigo.Postal", "nombre_corr_via"),all.x = TRUE)
View(dircom_geo)


#incluir el tipo de via#


dir_geo<-ifelse(is.na(dircom_geo$Tipo.via),paste0(dircom_geo$dir_geo1),paste0(dircom_geo$Tipo.via," ",dircom_geo$dir_geo1))
head(dir_geo, 60)
tail(dir_geo,60)

dircom_geo$dir_geo<-dir_geo
dir_geo_klm<-subset(dircom_geo, select = c(cod_comercio, dir_geo,Nombre_poblacion))
View(dir_geo_klm)
dir_geo_Klm_unique<-unique(dir_geo_klm[c("cod_comercio","dir_geo","Nombre_poblacion")])
write.table(dir_geo_Klm_unique,"~/ejemploR/dir_geo.txt",sep=";",quote = TRUE, row.names = FALSE, col.names = TRUE)
table(dir_geo_Klm_unique$Nombre_poblacion)

## Se crea un fichero klm en excell mediante un comando de VBA para despues ser utilizado en google Earth#
# y obtener la geolocalizacion en google Earth mediante extracion de un fichero klm#
# se incluye la poblacion para particionar el fichero para que google Earth no se colapse#


