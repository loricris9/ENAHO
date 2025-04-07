
#========================================================================================================================================================
#========================================================================================================================================================
#=================================== C?DIGO PARA DETERMINAR LAS CARACTERISTICAS DEL LOS NIVELES EDUCATIVOS ==============================================
#========================================================================================================================================================
#========================================================================================================================================================


#========================================================================================================================================================
#========================================================= CARGAR LIBRERIAS Y LA DATA A UTILIZAR ========================================================
#========================================================================================================================================================

########### INSTALACI?N DE PAQUETES

install.packages("packagename")
install.packages("reshape2")

########### LIMPIAR
rm(list=ls())

####LIBRARY
library(haven)
library(data.table)
library(tidyverse)
library(nortest)
library(ggplot2)
library(Hmisc)
library(dplyr)
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)
update.packages()

########### PEGAR LA RUTA DONDE SE GUARD? EL ARCHIVO
load("C:/Users/HP/OneDrive/Escritorio/LORI PRUDENCIO/TESIS/Modelo/dt_enaho.Rdata")
ruta <- "./DATOS/"
#load(file = paste0(ruta,"dt_enaho.Rdata")) #DATATABLE
dt_enaho<-rename(dt_enaho, Sex = p207, Estrsocial = estrsocial, Hac = hacinamiento ,Etn = p558c, Ocu = ocu500, Inghog = inghog2d, Mieperho = mieperho, Edad = p208a, Habhog = p104a, Rathac = ratio_hab )

#========================================================================================================================================================
#================================================================== AN?LISIS DEL DEFAULT ================================================================
#========================================================================================================================================================


#TRASFORMAR A DATA FRAME
dt_enaho <- as.data.frame(dt_enaho)

#VERIFICAR SI TIENE VALORES NULL
table(dt_enaho$p301a, useNA = "alw")
str(dt_enaho$p301a)
dt_enaho<-as.data.table(dt_enaho)

# FILTRAR A LOS MISSING Y A LOS QUE NO IRAN EN EL AN?LSISIS
dt_enaho<-dt_enaho[!is.na(p301a),]
dt_enaho<-dt_enaho[p301a >= 3 & p301a <= 11 ,]
table(dt_enaho$p301a)


# CONSTRUCCI?N DE TARGET EN 3 CATEGORIAS (PRIMARIA, SECUNDARIA Y SUPERIOR)

dt_enaho$Nivedu <- ifelse(dt_enaho$p301a <= 4 , 1,
                          ifelse(dt_enaho$p301a <= 6 ,2,3))


# VERIFICAR SI CRUZ? LA INFORMACI?N
table(dt_enaho$p301a,dt_enaho$Nivedu)
prop.table(table(dt_enaho$Nivedu))
dt_enaho$Nivedu<- as.factor(dt_enaho$Nivedu)



#========================================================================================================================================================
#================================================= CONSTRUCCI?N DE LA DATA DE DESARROLO  ================================================================
#========================================================================================================================================================


str(dt_enaho$ubigeo)
table(dt_enaho$ubigeo)

# Probando con solo callao
#dt_enaho <- filter(dt_enaho, ubigeo =='070104' | ubigeo =='070105' | ubigeo =='070106')

# Porbando com callao y lima metropolitana
dt_enaho <- filter(dt_enaho, ubigeo == 070104| ubigeo ==070105| ubigeo ==070106|
                     ubigeo ==150101|
                     ubigeo ==150104|
                     ubigeo ==150105|
                     ubigeo ==150108|
                     ubigeo ==150109|
                     ubigeo ==150111|
                     ubigeo ==150113|
                     ubigeo ==150114|
                     ubigeo ==150115|
                     ubigeo ==150116|
                     ubigeo ==150120|
                     ubigeo ==150121|
                     ubigeo ==150122|
                     ubigeo ==150128|
                     ubigeo ==150130|
                     ubigeo ==150131|
                     ubigeo ==150132|
                     ubigeo ==150134|
                     ubigeo ==150136|
                     ubigeo ==150140|
                     ubigeo ==150141|
                     ubigeo ==150102|
                     ubigeo ==150106|
                     ubigeo ==150110|
                     ubigeo ==150112|
                     ubigeo ==150117|
                     ubigeo ==150125|
                     ubigeo ==150135|
                     ubigeo ==150139|
                     ubigeo ==150119|
                     ubigeo ==150123|
                     ubigeo ==150124|
                     ubigeo ==150126|
                     ubigeo ==150127|
                     ubigeo ==150129|
                     ubigeo ==150133|
                     ubigeo ==150138|
                     ubigeo ==150142|
                     ubigeo ==150143|
                     ubigeo ==150103|
                     ubigeo ==150107|
                     ubigeo ==150118|
                     ubigeo ==150137
)


# Verificando que filtr? bien
table(dt_enaho$ubigeo)

str(dt_enaho)
colnames(dt_enaho)

# Seleccionando solo las variables a utilizar
dt_enaho <- as.data.frame(dt_enaho)
dt_enaho <- select(dt_enaho, c(1,2,23,22,21,20,9,10,11,15,16,17,24,25,26))


# An?lisis general para verificar los missing totales
data_univ<-data.frame(colnames(dt_enaho))
data_univ<-mutate(data_univ,TOTAL=0,VALIDOS=0,DESVIACION=0,FILL_RATE=0)

for(i in 1:length(dt_enaho))
{
  data_univ$TOTAL[i]=length(dt_enaho[[i]])
  data_univ$VALIDOS[i]=sum(is.na(dt_enaho[[i]])==FALSE)
  data_univ$DESVIACION[i]=sd(dt_enaho[[i]],na.rm = TRUE)
  data_univ$FILL_RATE[i]=(table(is.na(dt_enaho[[i]]))[1]/ 
                            (table(is.na(dt_enaho[[i]]))[1]+
                               table(is.na(dt_enaho[[i]]))[2]))
  print(i)
}

# completamos los missing del fill rate
data_univ$FILL_RATE<-ifelse(is.na(data_univ$FILL_RATE),1,data_univ$FILL_RATE)


########### Selecci?n de variables cuantitativas para an?lisis univariado

colnames(dt_enaho)
base_MOD_cuanti_selec <- select(dt_enaho, c(5,6))



### Create the function para calcular la moda.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


### CREANDO LA TABLA DE ALMACENAMIENTO PARA INDICADORES CUANTITATIVOS
library(dplyr)
library(nortest)
library(base)

data_univ3<-data.frame(colnames(base_MOD_cuanti_selec))
data_univ3<-mutate(data_univ3,TOTAL=0,MISS=0,MISS_PORC=0,MISS_DEFAULT=0,MEDIA=0,MEDIANA=0,MODA=0,DESVEST=0,MIN=0,MAX=0,
                   P5=0,P25=0,P75=0,P90=0,FILL_RATE=0,NORM_P=0.000)

for(i in 1:length(base_MOD_cuanti_selec))
{
  data_univ3$TOTAL[i]=length(base_MOD_cuanti_selec[[i]])
  data_univ3$MISS[i]=sum(is.na(base_MOD_cuanti_selec[[i]])==TRUE)
  data_univ3$MISS_PORC[i]=sum(is.na(base_MOD_cuanti_selec[[i]])==TRUE)/length(base_MOD_cuanti_selec[[i]])
  data_univ3$MISS_DEFAULT[i]=sum((is.na(base_MOD_cuanti_selec[[1]])==TRUE & base_MOD_cuanti_selec$PISE==1)==TRUE)
  data_univ3$MEDIA[i]=mean(base_MOD_cuanti_selec[[i]],na.rm = TRUE)
  data_univ3$MEDIANA[i]=median(base_MOD_cuanti_selec[[i]],na.rm = TRUE)
  data_univ3$MODA[i]=getmode(base_MOD_cuanti_selec[[i]])
  data_univ3$DESVEST[i]=sd(base_MOD_cuanti_selec[[i]],na.rm = TRUE)
  data_univ3$MIN[i]=min(base_MOD_cuanti_selec[[i]],na.rm = TRUE)
  data_univ3$MAX[i]=max(base_MOD_cuanti_selec[[i]],na.rm = TRUE)
  data_univ3$P5[i]=quantile(base_MOD_cuanti_selec[[i]],na.rm = TRUE,.05)
  data_univ3$P25[i]=quantile(base_MOD_cuanti_selec[[i]],na.rm = TRUE,.25)
  data_univ3$P75[i]=quantile(base_MOD_cuanti_selec[[i]],na.rm = TRUE,.75)
  data_univ3$P90[i]=quantile(base_MOD_cuanti_selec[[i]],na.rm = TRUE,.90)
  data_univ3$FILL_RATE[i]=(table(is.na(base_MOD_cuanti_selec[[i]]))[1]/
                             (table(is.na(base_MOD_cuanti_selec[[i]]))[1]+
                                table(is.na(base_MOD_cuanti_selec[[i]]))[2])*100)
  data_univ3$NORM_P[i]=lillie.test(base_MOD_cuanti_selec[[i]])$p.value
  print(i)
}

head(data_univ3)

write.csv(data_univ3,file="data_univ_cuantivf.csv")

getwd()


# Grafica general de los cuantitativos
hists<-as.matrix(colnames(base_MOD_cuanti_selec))
#Aqui podemos tomar el histograma del ingreso
pdf("matriz_datos_cuantivf.pdf") 
for(i in 1:length(base_MOD_cuanti_selec))
{
  hist(base_MOD_cuanti_selec[[i]],
       main=hists[i],
       xlab="Valores de variable",
       border="black",
       col="Gray")
  print(i)
}
dev.off()



########### Selecci?n de variables cualitativas para an?lisis univariado

colnames(dt_enaho)
base_MOD_cualis <- select(dt_enaho, c(3,7,9,10,11,12,14))



###########Creando tablas de almacenamineto de informaci?n
data_univ_cuali<-data.frame(colnames(base_MOD_cualis))
data_univ_cuali<-mutate(data_univ_cuali,TOTAL=0,DIST_CAT=0,MISS=0,MISS_PORC=0,MODA=0)

for(i in 1:length(base_MOD_cualis))
{
  data_univ_cuali$TOTAL[i]=length(base_MOD_cualis[[i]])
  data_univ_cuali$DIST_CAT[i]=uniqueN(base_MOD_cualis[[i]],na.rm = TRUE)
  data_univ_cuali$MISS[i]=sum(is.na(base_MOD_cualis[[i]])==TRUE)
  data_univ_cuali$MISS_PORC[i]=sum(is.na(base_MOD_cualis[[i]])==TRUE)/length(base_MOD_cualis[[i]])
  data_univ_cuali$MODA[i]=getmode(base_MOD_cualis[[i]])
  print(i)
}

# guardando archivos de an?lisis
write.csv(data_univ_cuali,file="univ_cualivf.csv")




### Imputaci?n de las variables cuanti
for (i in seq_along(base_MOD_cuanti_selec)) {
  base_MOD_cuanti_selec[[i]] <- replace(base_MOD_cuanti_selec[[i]], is.na(base_MOD_cuanti_selec[[i]]), median(base_MOD_cuanti_selec[[i]],na.rm = TRUE))
  print(i)
}




## Correlaciones de las variables cuanti

library(corrplot)
corr <- cor(base_MOD_cuanti_selec) ## aplicar la correlaci?n sperman 
corr
#correlaci?n despreciable: r < |0.1|
# correlaci?n baja: |0.1| < r <= |0.3|
#correlaci?n mediana : |0.3| < r <= |0.5|
# correlaci?n fuerte o alta: r > |0.5|

# gr?fica inicial de correlaciones
corrplot(corr, type="upper", order="hclust", tl.col="black", addCoef.col = "black", tl.srt=45)

#========================================================================================================================================================
#================================================= AN?LISIS BIVARIADO - CHI CUADRADO  ===================================================================
#========================================================================================================================================================


#La prueba de independencia Chi cuadrado permite determinar si existe asociaci?n entre dos variables discretas. Sus supuestos de aplicaci?n son:

#La muestra es un subconjunto aleatorio de la poblaci?n.
#Los valores esperados son mayores o iguales a 5.
#Las variables discretas poseen menos de 20 niveles cada una.
#La prueba de independencia Chi cuadrado permite evaluar la siguiente hip?tesis nula y alternativa:

#H0: ??2=0. No existe asociaci?n entre las variables.
#H1: ??2>0. Hay asociaci?n entre las variables.



########### BIVARIADO PARA VARIABLES CUALITATIVAS


# chi cuadrado para sexo / p207
tabla <- table(dt_enaho$Nivedu,dt_enaho$Sex)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para nivel socioecon?mico / estrsocial
tabla <- table(dt_enaho$Nivedu,dt_enaho$Estrsocial)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para nivel socioecon?mico / estrsocial
tabla <- table(dt_enaho$Nivedu,dt_enaho$Estrsocial)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi




# chi cuadrado para estado civil / p209
tabla <- table(dt_enaho$Nivedu,dt_enaho$p209)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para ocupaciones / ocu500
tabla <- table(dt_enaho$Nivedu,dt_enaho$Ocu)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para informalidad / ocupinf
tabla <- table(dt_enaho$Nivedu,dt_enaho$ocupinf)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para Ednicidad / p558c
tabla <- table(dt_enaho$Nivedu,dt_enaho$Etn)

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



# chi cuadrado para hacinamiento / hacinamiento
tabla <- table(dt_enaho$Nivedu,dt_enaho$Hac)
tabla
chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi



########### BIVARIADO PARA VARIABLES CUANTITATIVAS

### tenemos que categorizar las variables num?ricas para aplicar el chi cuadrado (es recomendable)


### ingresos
quantile(dt_enaho$Inghog, prob=c(0,0.25,0.5,0.75,1))
#0%        25%        50%        75%       100% 
#0.00   13989.88   25674.97   44745.00 1015785.00 

dt_enaho$Inghog_cat <- ifelse(dt_enaho$Inghog <= 13989.88 , 1,
                                ifelse(dt_enaho$Inghog <= 25674.97 ,2,
                                       ifelse(dt_enaho$Inghog <= 44745.00 ,3,
                                              ifelse(dt_enaho$Inghog <= 1015785.00  ,4,5 ))))


# por quintiles porque el nivel socioecon?mico tien 5 categor?as
tabla <- table(dt_enaho$Nivedu,dt_enaho$Inghog_cat)
tabla
chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada

chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi

# gr?fica 
grafico <- plot(x = dt_enaho$Nivedu, y = dt_enaho$Inghog, xlab ="Nivel educativo", ylab="Ingresos")

dt_enaho$Nivedu <- as.factor(dt_enaho$Nivedu)



### miembros total de hogar 
quantile(dt_enaho$Mieperho, prob=c(0,0.25,0.5,0.75,1))
#0%  25%  50%  75% 100% 
#1    3    4    6   21 

dt_enaho$Mieperho_cat <- ifelse(dt_enaho$Mieperho <= 3 , 1,
                                ifelse(dt_enaho$Mieperho <= 4 ,2,
                                       ifelse(dt_enaho$Mieperho <= 6 ,3,
                                              ifelse(dt_enaho$Mieperho <= 21  ,4,5 ))))


tabla <- table(dt_enaho$Nivedu,dt_enaho$Mieperho_cat)
tabla

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada
chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi

# gr?fica 
grafico <- plot(x = dt_enaho$Nivedu, y = dt_enaho$Mieperho, xlab="Nivel educativo", ylab="N?mero de miembros del hogar")
grafico




## miembros habitaciones exclusivas para dormir
quantile(base_MOD_cuanti_selec$Habhog, prob=c(0,0.25,0.5,0.75,1))
#0%  25%  50%  75% 100% 
#0    1    2    3   13 

dt_enaho$Habhog_cat <- ifelse(dt_enaho$Habhog <= 1 , 1,
                             ifelse(dt_enaho$Habhog <= 2 ,2,
                                    ifelse(dt_enaho$Habhog <= 3 ,3,
                                           ifelse(dt_enaho$Habhog <= 13  ,4,5 ))))


tabla <- table(dt_enaho$Nivedu,dt_enaho$Habhog_cat)
tabla

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada
chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi


# gr?fica
grafico <- plot(x = dt_enaho$Nivedu, y = dt_enaho$Habhog)
grafico




### edad
quantile(base_MOD_cuanti_selec$Edad, prob=c(0,0.25,0.5,0.75,1))
#0%  25%  50%  75% 100% 
#0   14   30   50   98 

dt_enaho$Edad_cat <- ifelse(dt_enaho$Edad <= 14 , 1,
                             ifelse(dt_enaho$Edad <= 30 ,2,
                                    ifelse(dt_enaho$Edad <= 50 ,3,
                                           ifelse(dt_enaho$Edad <= 98 ,4,5 ))))


tabla <- table(dt_enaho$Nivedu,dt_enaho$Edad_cat)
tabla

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada
chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi

# gr?fica
grafico <- plot(x = dt_enaho$Nivedu, y = dt_enaho$Edad, xlab="Nivel educativo", ylab="Edad(A?os)")
grafico




### ratio habitacion
quantile(base_MOD_cuanti_selec$Rathab, prob=c(0,0.25,0.5,0.75,1))
#0%        25%        50%        75%       100% 
#0.0000000  0.3333333  0.5000000  0.6666667 12.0000000 

dt_enaho$Rathab_cat <- ifelse(dt_enaho$Rathab <= 0.3333333 , 1,
                                 ifelse(dt_enaho$Rathab <= 0.5000000 ,2,
                                        ifelse(dt_enaho$Rathab <= 0.6666667 ,3,
                                               ifelse(dt_enaho$Rathab <= 12.0000000 ,4,5 ))))



tabla <- table(dt_enaho$Nivedu,dt_enaho$Rathab_cat)
tabla

chisq.test(tabla)$expected # Ver valores esperados en la tabla cruzada
chi <-chisq.test(tabla) #Guardamos los resultados del test en un objeto
chi

# gr?fica
grafico <- plot(x = dt_enaho$Nivedu, y = dt_enaho$Rathab)
grafico










#========================================================================================================================================================
#============================================================== GENERANDO EL MODELO   ===================================================================
#========================================================================================================================================================

# nombres de las variables
colnames(dt_enaho)

# Seleccionando base de desarrollo 
desarrollo <- select(dt_enaho, Estrsocial, Inghog, Mieperho,Habhog,Sex,Ocu,Etn,Nivedu)
#modelo con solo variables deseadas


# analizando las variables que saldran del modelo
data_univ<-data.frame(colnames(desarrollo))
data_univ<-mutate(data_univ,TOTAL=0,VALIDOS=0,DESVIACION=0,FILL_RATE=0)

for(i in 1:length(desarrollo))
{
  data_univ$TOTAL[i]=length(desarrollo[[i]])
  data_univ$VALIDOS[i]=sum(is.na(desarrollo[[i]])==FALSE)
  data_univ$DESVIACION[i]=sd(desarrollo[[i]],na.rm = TRUE)
  data_univ$FILL_RATE[i]=(table(is.na(desarrollo[[i]]))[1]/ 
                            (table(is.na(desarrollo[[i]]))[1]+
                               table(is.na(desarrollo[[i]]))[2]))
  print(i)
}



# SELECCIONAMOS LAS VARIABLES QUE SE USAR?N EN EL MODELO
desarrollo <- select(dt_enaho, Estrsocial, Mieperho,Habhog,Sex,Ocu,Etn,Nivedu,Inghog)
desarrollo <- filter(desarrollo,is.na(desarrollo$Nivedu)==FALSE)

# IMPUTACI?N DE VALORES PERDIDOS.:
desarrollo$Sex<-ifelse(is.na(desarrollo$Sex)==TRUE,2,desarrollo$Sex)
#desarrollo$p209<-ifelse(is.na(desarrollo$p209)==TRUE,6,desarrollo$p209)
desarrollo$Ocu<-ifelse(is.na(desarrollo$Ocu)==TRUE,1,desarrollo$Ocu)
desarrollo$Etn<-ifelse(is.na(desarrollo$Etn)==TRUE,6,desarrollo$Etn)
#desarrollo$Hac<-ifelse(is.na(desarrollo$Hac)==TRUE,1,desarrollo$Hac)
#desarrollo$Edad<-ifelse(is.na(desarrollo$Edad)==TRUE,34,desarrollo$Edad)
desarrollo$Habhog<-ifelse(is.na(desarrollo$Habhog)==TRUE,3,desarrollo$Habhog)
#desarrollo$Rathac<-ifelse(is.na(desarrollo$Rathac)==TRUE,0.6,desarrollo$Rathac)


# GR?FICA DE DISTRIBUCI?N DE FRECUENCIAS
#hist(desarrollo$Edad)


# DIAGRAMA DE CAJA PARA LA EDAD Y TARGET
#grafico<-ggplot(data = desarrollo, aes(x = Nivedu, y = Edad, colour = p209)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")


#grafico



# DIAGRAMA DE CAJA PARA LA ratio_hab Y TARGET
grafico<-ggplot(data = desarrollo, aes(x = Nivedu, y = Rathab, colour = p209)) +
  geom_boxplot() +
  theme_bw() +
  theme(legend.position = "bottom")

grafico


########### GENERANDO EL MODELO

# Modelo multinomial
library(nnet)
modelo <-multinom(Nivedu~., data = desarrollo)
#para probar modelos
#1
modelo1 <-multinom(Nivedu~Ocu+Estrsocial+Sex+Etn+Mieperho+Habhog+Inghog, data = desarrollo)
summary(modelo1)
#2
modelo2 <-multinom(Nivedu~Ocu+Inghog+Sex+Etn+Mieperho, data = desarrollo)
summary(modelo2)
#3 mod final
modelo3 <-multinom(Nivedu~Ocu+Estrsocial+Sex+Etn+Mieperho+Habhog, data = desarrollo)
summary(modelo3)
## MIDIENDO LA SIGNIFICANCIA DEL MODELO
z <- summary(modelo)$coefficients/summary(modelo)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
install.packages("VGAM")
library(VGAM)
#usando funcion que ya muestra el estadistico y el p value
#mod1 prueba
modelo1 <- vglm(Nivedu~Ocu+Estrsocial+Sex+Etn+Mieperho+Habhog+Inghog, multinomial(refLevel = 1), data = desarrollo)
summary(modelo1)
#mod2 prueba
modelo2 <- vglm(Nivedu~Ocu+Inghog+Sex+Etn+Mieperho, multinomial(refLevel = 1), data = desarrollo)
summary(modelo2)
#mod final
modelo3 <- vglm(Nivedu~Ocu+Estrsocial+Sex+Etn+Mieperho+Habhog, multinomial(refLevel = 1), data = desarrollo)
summary(modelo3)
#el valor de la prediccion se concentra sobre la columna q tiene mayor %(para interpretar los odd ratios)
exp(coef(modelo1, matrix=TRUE))
head(round(fitted(modelo1), 2))





                                                                                