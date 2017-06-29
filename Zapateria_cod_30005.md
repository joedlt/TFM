# Comercio_Zapatería_30005
Jose  
27 de junio de 2017  

## Segmentación Clientes Zapatería 30005

Se utiliza las operaciones en el TPV de 24 meses para segmentar a los clientes del comercio, tomando como clientes el código de tarjeta.
Se realiza una segmentación de RFM (Recencia, Frecuencia , Monto).
Se utilizaran los primeros 12 meses para efecturar un tranning de la segmentación y los siguientes 12 meses como test.

Primero se cargan los datos y se hace una inspección de los mismos 


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
Comercios_seg<-read.csv("comercios_segmentacion.txt",sep = ",",quote = "\"",
                        header = TRUE, colClasses = c("Cod_comercio"="character"), stringsAsFactors = FALSE)

summary(Comercios_seg)
```

```
##  Cod_comercio       Cod_tarjeta            Dia           
##  Length:33192       Length:33192       Length:33192      
##  Class :character   Class :character   Class :character  
##  Mode  :character   Mode  :character   Mode  :character  
##                                                          
##                                                          
##                                                          
##      Venta        
##  Min.   :-580.00  
##  1st Qu.:  28.00  
##  Median :  57.40  
##  Mean   :  82.37  
##  3rd Qu.: 111.50  
##  Max.   :4266.36
```

```r
str(Comercios_seg)
```

```
## 'data.frame':	33192 obs. of  4 variables:
##  $ Cod_comercio: chr  "000160523" "000160523" "000160523" "000160523" ...
##  $ Cod_tarjeta : chr  "FFGEEZKEEEUSKYKU" "FFGEEZKEEYGGZYKZ" "FFGEEZKEEYGGZYKZ" "FFGEEZKEEYZERYKS" ...
##  $ Dia         : chr  "2017-03-29" "2016-04-02" "2016-08-25" "2016-02-16" ...
##  $ Venta       : num  64 178 69 238 130 69 70 72 118 0 ...
```

```r
Comercios_seg$Dia<-as.Date(Comercios_seg$Dia)
Comercios_seg$Frecuencia<-1
summary(Comercios_seg)
```

```
##  Cod_comercio       Cod_tarjeta             Dia            
##  Length:33192       Length:33192       Min.   :2015-04-01  
##  Class :character   Class :character   1st Qu.:2015-06-19  
##  Mode  :character   Mode  :character   Median :2015-07-18  
##                                        Mean   :2015-10-27  
##                                        3rd Qu.:2016-03-07  
##                                        Max.   :2017-03-31  
##      Venta           Frecuencia
##  Min.   :-580.00   Min.   :1   
##  1st Qu.:  28.00   1st Qu.:1   
##  Median :  57.40   Median :1   
##  Mean   :  82.37   Mean   :1   
##  3rd Qu.: 111.50   3rd Qu.:1   
##  Max.   :4266.36   Max.   :1
```
Dividimos la muestra entre los dos periodos e igualmente filtramos solo el comercio de analisis en este caso una zapateria 30005, ya que el fichero completo contiene los comercios escogidos para hacer la demo


```r
Fecha_1<-as.Date("2016-03-31")
Fecha_2<-as.Date("2017-04-01")

comercio_1<-Comercios_seg[(Comercios_seg$Cod_comercio=="000160523"),]
```
Se crean las variables para el RFM 


```r
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
```
Se realiza un gráfico de inspección de la relación de las varibles de RFM entre ellas
<img src="C:/Users/Jose/Documents/Master Data Science/TFM/Segmentacion/Zapateria Cod Postal 30005.png">
Ahora se normalizan las variables


```r
RFM_comer_1_norm<-scale(RFM_comer_1[,-1])
```
Ahora para segmentar se realizara un modelo K means, pero dado que en este modelo se ha de especificar el numero de clusters que se quiere, se realizara el Elbow method para ver el numero de clusters óptimos.


```r
misdatos <- RFM_comer_1_norm
elb <- (nrow(misdatos)-1)*sum(apply(misdatos,2,var))
for (i in 2:15) elb[i] <- sum(kmeans(misdatos,
                                     centers=i)$withinss)
```
La gráfica no muestra un punto óptimo diferenciado, existen dos puntos de inflexión como lo son el 3 y el 6.
<img src="C:/Users/Jose/Documents/Master Data Science/TFM/Segmentacion/Zapateria cod 30005 metodo del codo.png">







Para tener otra perspectiva se utiliza la libreria (NbClust) que efectúa 30 tipos de índices para determinar el número óptimo de clusters de un conjunto de datos


```r
library(NbClust)
nc <- NbClust(RFM_comer_1_norm, min.nc=3, max.nc=9, method="kmeans")
```

![](Zapateria_cod_30005_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```
## *** : The Hubert index is a graphical method of determining the number of clusters.
##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
##                 significant increase of the value of the measure i.e the significant peak in Hubert
##                 index second differences plot. 
## 
```

![](Zapateria_cod_30005_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```
## *** : The D index is a graphical method of determining the number of clusters. 
##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
##                 second differences plot) that corresponds to a significant increase of the value of
##                 the measure. 
##  
## ******************************************************************* 
## * Among all indices:                                                
## * 7 proposed 3 as the best number of clusters 
## * 2 proposed 4 as the best number of clusters 
## * 4 proposed 5 as the best number of clusters 
## * 7 proposed 6 as the best number of clusters 
## * 2 proposed 8 as the best number of clusters 
## * 2 proposed 9 as the best number of clusters 
## 
##                    ***** Conclusion *****                            
##  
## * According to the majority rule, the best number of clusters is  3 
##  
##  
## *******************************************************************
```

```r
table(nc$Best.n[1,])
```

```
## 
## 0 3 4 5 6 8 9 
## 2 7 2 4 7 2 2
```
Observando el gráfico de barras se ve claramente que el óptimo esta entre 3 y 6

<img src="C:/Users/Jose/Documents/Master Data Science/TFM/Segmentacion/Zapateria Cod Postal 30005 NbClust.png">

Así que se ejecutan los dos modelos el k=3 y k=6 y mediante análisis gráfico y el de los centroides se optará por el k que más sentido tenga.


```r
N_Cluster<-3
set.seed(1234)
Model_K<-kmeans(RFM_comer_1_norm,N_Cluster,iter.max = 100)

Segments<-Model_K$cluster
table(Segments)
```

```
## Segments
##   1   2   3 
## 195 944 803
```

```r
aggregate(RFM_comer_1[,-1], by = list(Segments), mean)
```

```
##   Group.1  RECENCIA FRECUENCIA MONETIZACION
## 1       1 128.75385   2.435897     502.3385
## 2       2 284.20445   1.093220     176.5784
## 3       3  98.78705   1.089664     203.4919
```
El gráfico

<img src="C:/Users/Jose/Documents/Master Data Science/TFM/Segmentacion/Zapateria Cod Postal 30005 Clustering Kmeans para 3 CLUSTERS del Modelo RFM 12M.png">

Ahora el k=6


```r
N_Cluster<-6
set.seed(1234)
Model_K<-kmeans(RFM_comer_1_norm,N_Cluster,iter.max = 100)


Segments<-Model_K$cluster
table(Segments)
```

```
## Segments
##   1   2   3   4   5   6 
##  13  88  91 850 719 181
```

```r
aggregate(RFM_comer_1[,-1], by = list(Segments), mean)
```

```
##   Group.1  RECENCIA FRECUENCIA MONETIZACION
## 1       1  95.84615   5.076923   1162.30769
## 2       2 281.97727   1.977273     37.59091
## 3       3  88.24176   2.274725     53.75824
## 4       4 284.10000   1.000000    188.12941
## 5       5  99.09597   1.000000    215.91099
## 6       6 143.07735   2.022099    494.97238
```
El gráfico

<img src="C:/Users/Jose/Documents/Master Data Science/TFM/Segmentacion/Zapateria Cod Postal 30005 Clustering Kmeans para 6 CLUSTERS del Modelo RFM 12M.png">

Se puede ver que tanto los centroides del modelo  como el gráfico de scatter plot de k=6 es el que más sentido tiene para este analisis y por lo tanto será el utilizado en la segmentación.

Ahora creamos las variables con los centroides de la segmentación


```r
Segmentos<-aggregate(RFM_comer_1[,-1], by = list(Segments), mean)
Segmentos$Contador<-table(Segments)
N_Media<-apply(RFM_comer_1[,-1],MARGIN=2,FUN=mean)
N_Std<-apply(RFM_comer_1[,-1],MARGIN=2,FUN=sd)
```
Se realiza la comprobación de los centroides


```r
Model_K$centers[,3]*N_Std[3]+N_Media[3]
```

```
##          1          2          3          4          5          6 
## 1162.30769   37.59091   53.75824  188.12941  215.91099  494.97238
```

```r
Segmentos$MONETIZACION
```

```
## [1] 1162.30769   37.59091   53.75824  188.12941  215.91099  494.97238
```
Y luego se incluyen los segmentos en el primer período.


```r
RFM_comer_1$Segmento_1<-Segments
```
Aplicamos el modelo al segundo período


```r
RFM_comer_2$Dst_Cl_1<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[1])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[1])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[1])/N_Std[3])^2
RFM_comer_2$Dst_Cl_2<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[2])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[2])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[2])/N_Std[3])^2
RFM_comer_2$Dst_Cl_3<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[3])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[3])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[3])/N_Std[3])^2
RFM_comer_2$Dst_Cl_4<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[4])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[4])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[4])/N_Std[3])^2
RFM_comer_2$Dst_Cl_5<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[5])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[5])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[5])/N_Std[3])^2
RFM_comer_2$Dst_Cl_6<-((RFM_comer_2$RECENCIA-Segmentos$RECENCIA[6])/N_Std[1])^2+((RFM_comer_2$FRECUENCIA-Segmentos$FRECUENCIA[6])/N_Std[2])^2+((RFM_comer_2$MONETIZACION-Segmentos$MONETIZACION[6])/N_Std[3])^2
```
Se aplica la distancia mínima para asignar el segmento en el segundo período.


```r
RFM_comer_2$minimo<-apply(RFM_comer_2[,5:(4+N_Cluster)],MARGIN=1,FUN=min,na.rm=TRUE)

RFM_comer_2_a<-RFM_comer_2[,5:(4+N_Cluster)]==RFM_comer_2$minimo
RFM_comer_2$Segmento_2<-apply(RFM_comer_2_a,MARGIN=1,FUN=which)
```
Se crea un dataframe para ser exportado e incluirlo para la visualización


```r
RFM_comercio<-merge(RFM_comer_1[,c("Cod_tarjeta", "Segmento_1")],RFM_comer_2[,c("Cod_tarjeta", "Segmento_2")],all.x=TRUE,all.y=TRUE)
RFM_comercio$Segmento_1[is.na(RFM_comercio$Segmento_1)]=0
RFM_comercio$Segmento_2[is.na(RFM_comercio$Segmento_2)]=0
RFM_comercio$Cod_comercio<-"000160523"
RFM_comercio$tipo_cliente<-ifelse(RFM_comercio$Segmento_1==0 & RFM_comercio$Segmento_2!=0 ,"NUEVOS",ifelse(RFM_comercio$Segmento_1!=0 & RFM_comercio$Segmento_2==0,"PERDIDOS","SE MANTIENEN"))
RFM_comercio$Des_seg<-ifelse(RFM_comercio$Segmento_2==1,"CLIENTES MAS COMPRAS",ifelse(RFM_comercio$Segmento_2==0,"CLIENTES PERDIDOS",ifelse
                                                                                      (RFM_comercio$Segmento_2==2,"POSIBLE ABANDONO",ifelse(RFM_comercio$Segmento_2==3,"ANTIGUOS",ifelse(RFM_comercio$Segmento_2==4,"HISTORICO",ifelse(RFM_comercio$Segmento_2==5,"RECIENTES","MAYOR COMPRA MEDIA"))))))
```
Y por último se guarda el Dataframe en un fichero txt para luego ser usado en el cuadro de visualización
El proceso se repetirá para los otros comercios que se escogieron para la Demo.
