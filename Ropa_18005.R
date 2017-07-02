library(dplyr)
setwd("~/Master Data Science/TFM/Segmentacion")
Comercios_seg<-read.csv("comercios_segmentacion.txt",sep = ",",quote = "\"",
                        header = TRUE, colClasses = c("Cod_comercio"="character"), stringsAsFactors = FALSE)
summary(Comercios_seg)
str(Comercios_seg)
Comercios_seg$Dia<-as.Date(Comercios_seg$Dia)
Comercios_seg$Frecuencia<-1
summary(Comercios_seg)
View(Comercios_seg)
##se tiene 24 meses de facturación de los comercios por lo tanto se usa 12 meses para trainning##
## y los 12 restantes para test y cross validation##

Fecha_1<-as.Date("2016-03-31")
Fecha_2<-as.Date("2017-04-01")

Comercio<-"100372176"

comercio_1<-Comercios_seg[(Comercios_seg$Cod_comercio==Comercio),]

RFM_comer_1= summarise(group_by(comercio_1[comercio_1$Dia<Fecha_1 & comercio_1$Dia>=Fecha_1-366,], Cod_tarjeta),
                       RECENCIA = as.numeric(min(Fecha_1-Dia, na.rm = TRUE)),
                       FRECUENCIA = sum(Frecuencia, na.rm = TRUE),
                       MONETIZACION =  sum(Venta, na.rm = TRUE)
)

RFM_comer_2= summarise(group_by(comercio_1[comercio_1$Dia<Fecha_2 & comercio_1$Dia>=Fecha_2-365,], Cod_tarjeta),
                       RECENCIA = as.numeric(min(Fecha_2-Dia, na.rm = TRUE)),
                       FRECUENCIA = sum(Frecuencia, na.rm = TRUE),
                       MONETIZACION =  sum(Venta, na.rm = TRUE)
)

##Grafico##

png("Ropa Cod Postal 18005.png",width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(RFM_comer_1$FRECUENCIA,RFM_comer_1$RECENCIA, xlab="FRECUENCIA", ylab="RECENCIA")
frame()
smoothScatter(RFM_comer_1$FRECUENCIA,RFM_comer_1$MONETIZACION, xlab="FRECUENCIA",ylab="MONETIZACION")
smoothScatter(RFM_comer_1$RECENCIA,RFM_comer_1$MONETIZACION, xlab="RECENCIA",ylab="MONETIZACION")
mtext("Densidad de clientes mediante Modelo RFM 12 meses ", outer = TRUE, cex = 2)
dev.off()


RFM_comer_1_norm<-scale(RFM_comer_1[,-1])





## grafico metodo del Codo para determinar el numero de clusters##



misdatos <- RFM_comer_1_norm
elb <- (nrow(misdatos)-1)*sum(apply(misdatos,2,var))
for (i in 2:15) elb[i] <- sum(kmeans(misdatos,
                                     centers=i)$withinss)


png("Ropa Cod Postal 18005 metodo del codo.png",bg="white")
plot(1:15,elb,type = "b",xlab="Num Clusters", ylab="Clusters Suma de Cuadrados", col="blue")
mtext("Cluster metodo del codo ", pch = 20, cex = 2)
dev.off()


#### Metodo que para determinar el numero de clusters mediante la libreria NbClust utiliza 30 tipos de 
###indices para determinar el numero optimo de clusters de un conjunto de datos 
## en caso de error graphics.off() par("mar") par(mar=c(1,1,1,1))
library(NbClust)
nc <- NbClust(RFM_comer_1_norm, min.nc=3, max.nc=9, method="kmeans")

table(nc$Best.n[1,])

dev.off()
png("Ropa Cod Postal 18005 NbClust.png ")
barplot(table(nc$Best.n[1,]),
        xlab="Numero de Clusters", ylab="Numero de Criterios", col = "blue",
        main="Numero de Clusters por Criterio")
mtext("Numero de Clusters ", pch = 20, cex = 2)
dev.off()

## Se calcula los segmentos según el numero del analisis grafico##
##del metodo del codo( Elbow Method), el analisis de NbClust ##
## 


## estima con k=5
N_Cluster<-5
set.seed(1234)
Model_K<-kmeans(RFM_comer_1_norm,N_Cluster,iter.max = 100)


Segments<-Model_K$cluster
table(Segments)
aggregate(RFM_comer_1[,-1], by = list(Segments), mean)

## grafica de segmentos
dev.off()
png(paste("Ropa Cod Postal 18005 Clustering Kmeans para ",N_Cluster," CLUSTERS del Modelo RFM 12M.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(RFM_comer_1$FRECUENCIA,RFM_comer_1$RECENCIA,col=Segments, xlab="FRECUENCIA", ylab="RECENCIA")
plot(c(0,max(RFM_comer_1$RECENCIA)),c(0,max(RFM_comer_1$RECENCIA)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(RFM_comer_1$RECENCIA)),ylim=c(0,max(RFM_comer_1$RECENCIA)))
legend(1,max(RFM_comer_1$RECENCIA)/2-1,legend=c(1:N_Cluster),yjust = 0.5,col=c(1:N_Cluster),pch=15,cex=2)
plot(RFM_comer_1$FRECUENCIA,RFM_comer_1$MONETIZACION,col=Segments, xlab="FRECUENCIA",ylab="MONETIZACION")
plot(RFM_comer_1$RECENCIA,RFM_comer_1$MONETIZACION,col=Segments, xlab="RECENCIA",ylab="MONETIZACION")
mtext(paste("Clusterización kmeans de clientes mediante Modelo RFM 12 meses",sep=""), outer = TRUE, cex = 2)
dev.off()




###Centros segmentos##

Segmentos<-aggregate(RFM_comer_1[,-1], by = list(Segments), mean)
Segmentos$Contador<-table(Segments)
N_Media<-apply(RFM_comer_1[,-1],MARGIN=2,FUN=mean)
N_Std<-apply(RFM_comer_1[,-1],MARGIN=2,FUN=sd)

# comprobacion de los centroides#
Model_K$centers[,3]*N_Std[3]+N_Media[3]
Segmentos$MONETIZACION



## incluir los segmentos en RFM_comer_1

RFM_comer_1$Segmento_1<-Segments


### ahora aplicar el modelo al segundo perido ##

RFM_comer_2$Dst_Cl_1<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[1])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[1])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[1])/N_Std[3])^2
RFM_comer_2$Dst_Cl_2<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[2])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[2])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[2])/N_Std[3])^2
RFM_comer_2$Dst_Cl_3<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[3])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[3])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[3])/N_Std[3])^2
RFM_comer_2$Dst_Cl_4<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[4])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[4])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[4])/N_Std[3])^2
RFM_comer_2$Dst_Cl_5<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[5])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[5])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[5])/N_Std[3])^2



##Aplicamos la distancia minima al segmento para asignar el segmento en el segundo periodo##

RFM_comer_2$minimo<-apply(RFM_comer_2[,5:(4+N_Cluster)],MARGIN=1,FUN=min,na.rm=TRUE)

RFM_comer_2_a<-RFM_comer_2[,5:(4+N_Cluster)]==RFM_comer_2$minimo
RFM_comer_2$Segmento_2<-apply(RFM_comer_2_a,MARGIN=1,FUN=which)

## data frame a exportar para la visualización##

RFM_comercio<-merge(RFM_comer_1[,c("Cod_tarjeta", "Segmento_1")],RFM_comer_2[,c("Cod_tarjeta", "Segmento_2")],all.x=TRUE,all.y=TRUE)
RFM_comercio$Segmento_1[is.na(RFM_comercio$Segmento_1)]=0
RFM_comercio$Segmento_2[is.na(RFM_comercio$Segmento_2)]=0
RFM_comercio$Cod_comercio<-Comercio
RFM_comercio$tipo_cliente<-ifelse(RFM_comercio$Segmento_1==0 & RFM_comercio$Segmento_2!=0 ,"NUEVOS",ifelse(RFM_comercio$Segmento_1!=0 & RFM_comercio$Segmento_2==0,"PERDIDOS","SE MANTIENEN"))
Seg_num<-c(0,1,2,3,4,5)
Des_seg<-c("PERDIDOS","RECIENTE","MAYOR COMPRA MEDIA","POSIBLE ABANDONO","CLIENTE ESPORADICO","CLIENTES MAS COMPRAS")
Seg_Des<-data.frame(Seg_num,Des_seg)
RFM_comercio<-merge(x=RFM_comercio, y=Seg_Des,by.x = "Segmento_2",by.y = "Seg_num",all.x = TRUE)
RFM_comercio<-RFM_comercio[,c(2,3,1,4,5,6)]
View(RFM_comercio)

write.table(RFM_comercio,"~/Master Data Science/TFM/Segmentacion/Ficheros/Segmentos_Ropa_18005.txt",quote = TRUE,sep = ";",col.names = FALSE, row.names = FALSE) 

table(RFM_comercio$Segmento_1,RFM_comercio$Segmento_2)

View(RFM_comercio)
