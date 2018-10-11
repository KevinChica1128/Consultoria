#Script Consultoria

#Cargamos la base de datos
library(readxl)
PROPUESTA_FUNDEMERCA_1_ <- read_excel("C:/Users/KEVINSTEVEN/Desktop/UNIVALLE/CONSULTORIA/BUCANERO/PROPUESTA FUNDEMERCA (1).xlsx")
View(PROPUESTA_FUNDEMERCA_1_)
datos<-as.data.frame(PROPUESTA_FUNDEMERCA_1_[,c(-1,-2)])
row.names(datos)<-PROPUESTA_FUNDEMERCA_1_$Producor


for (i in 1:length(datos[,1])) {
  minimos[i]<-c(summary(t(datos[i,]))[1])
  Q1[i]<-c(summary(t(datos[i,]))[2])
  medianas[i]<-c(summary(t(datos[i,]))[3])
  medias[i]<-c(summary(t(datos[i,]))[4])
  Q3[i]<-c(summary(t(datos[i,]))[5])
  maximos[i]<-c(summary(t(datos[i,]))[6])
  desviaciones[i]<-c(sd(t(datos[i,]),na.rm = T))
  descriptivas<-rbind(minimos,Q1,medianas,medias,Q3,maximos,desviaciones)
}
descriptivas<-as.data.frame(descriptivas)
row.names(descriptivas)<-PROPUESTA_FUNDEMERCA_1_$Producor


