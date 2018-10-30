#Script Consultoria

#Cargamos la base de datos
library(readxl)
PROPUESTA_FUNDEMERCA_1_ <- read_excel("C:/Users/KEVINSTEVEN/Desktop/UNIVALLE/CONSULTORIA/BUCANERO/PROPUESTA FUNDEMERCA (2.5).xlsx")
View(PROPUESTA_FUNDEMERCA_1_)
datos<-as.data.frame(PROPUESTA_FUNDEMERCA_1_[,c(-1,-2)])
row.names(datos)<-PROPUESTA_FUNDEMERCA_1_$Producor

minimos<-c()
Q1<-c()
medianas<-c()
medias<-c()
Q3<-c()
maximos<-c()
desviaciones<-c()
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
colnames(descriptivas)<-PROPUESTA_FUNDEMERCA_1_$Producor

#Distribución mortalidades por productor
install.packages("rriskDistributions")
library(rriskDistributions)
res1<-fit.cont(datos[20,][!is.na(datos[20,])]) #Ver distribución de las mortalidades de los individuos

#######intervalos de confianza para la proporcion pobacional de pollos muertos#####
datos1=t(datos)#trasponemos la matriz para poder operar
##generalizando
#creamos un ciclo para el calculo de la media
X_b=c()
for(i in 1:length(datos[,1])){
  X_b=c(X_b,mean(datos1[,i],na.rm = T))
}
#ciclo para el calculo de los intervalos
alfa=0.05 #confianza
L1=c()
L2=c()
intervalos=data.frame(matrix(NA,48,4))#matriz para amacenar los intervalos
colnames(intervalos)=c("cod","inferior","superior","proporcion")
row.names(intervalos)=PROPUESTA_FUNDEMERCA_1_$Producor
intervalos[,1]=1:48
intervalos[,4]=X_b
for(i in 1:length(datos[,1])){
  L1=c(L1,X_b-(qnorm(1-alfa/2)*sqrt((X_b*(1-X_b))/length(na.omit(datos1[,i])))))
  L2=c(L2,X_b+(qnorm(1-alfa/2)*sqrt((X_b*(1-X_b))/length(na.omit(datos1[,i])))))
  intervalos[i,2]=L1[i] ; intervalos[i,3]=L2[i]
}
install.packages("ggplot2")
library("ggplot2")
install.packages("Rmisc")
library("Rmisc")

df=data.frame(intervalos)
graf1=ggplot(df,aes(cod,proporcion, ymin=inferior, ymax=superior))+
  ylim(-0.05,0.2)+
  geom_pointrange() +
  geom_errorbar(aes(ymin=inferior, ymax=superior), width=0.9)

x11()
graf1

#Intervalos de credibilidad con Poisson:
library("HDInterval")
library("TeachingDemos")

#Cargamos la base de datos de los pollos muertos:
pollosmuertos<- read_excel("PROPUESTA FUNDEMERCA (2.5).xlsx")
pollosmuertos<-as.data.frame(pollosmuertos[,c(-1,-2)])
row.names(pollosmuertos)<-PROPUESTA_FUNDEMERCA_1_$Producor

#Para el ciclo 50 del productor ALBEIRO HERNANDEZ
L1<-rgamma(10000,pollosmuertos[1,1]+(1/2), rate=1) #Valores simulados de la aposteriori
emp.hpd (L1, conf = 0.95 )   #Intervalo con el 95% de confianza 

#Con los cuantiles
qgamma(0.025,pollosmuertos[1,1]+(1/2), rate=1)
qgamma(0.975,pollosmuertos[1,1]+(1/2), rate=1)

#Generalización para todos los ciclos y todos los productores con función:
Intervalos<-data.frame(matrix(NA,48,51))#matriz para almacenar los intervalos
colnames(Intervalos)=c("cod",rep(c("Inferior","Superior"),25))
row.names(Intervalos)=PROPUESTA_FUNDEMERCA_1_$Producor
Intervalos[,1]=1:48
for (j in 1:length(datos[,1])) {
  for (i in seq(2,51,2)) {
    if(anyNA(pollosmuertos[j,i-(i/2)])=="TRUE"){
      is.na<-Intervalos[j,i]
      is.na<-Intervalos[j,i+1]
    }else{
      L<-rgamma(10000,pollosmuertos[j,i-(i/2)]+(1/2),rate = 1)
      Intervalos[j,i]=emp.hpd(L,0.95)[1]
      Intervalos[j,i+1]=emp.hpd(L,0.95)[2]
      }
  }
}

#Generalización para todos los ciclos y todos los productores con cuantiles:
IntervalosC<-data.frame(matrix(NA,48,51))#matriz para almacenar los intervalos
colnames(IntervalosC)=c("cod",rep(c("Inferior","Superior"),25))
row.names(IntervalosC)=PROPUESTA_FUNDEMERCA_1_$Producor
IntervalosC[,1]=1:48
for (j in 1:length(datos[,1])) {
  for (i in seq(2,51,2)) {
    if(anyNA(pollosmuertos[j,i-(i/2)])=="TRUE"){
      is.na<-IntervalosC[j,i]
      is.na<-IntervalosC[j,i+1]
    }else{
      L<-rgamma(10000,pollosmuertos[j,i-(i/2)]+(1/2),rate = 1)
      IntervalosC[j,i]=qgamma(0.025,pollosmuertos[j,i-(i/2)]+(1/2), rate=1)
      IntervalosC[j,i+1]=qgamma(0.975,pollosmuertos[j,i-(i/2)]+(1/2), rate=1)
    }
  }
}

#La distribución predictiva posterior es una binomial negativa:
#La probabilidad de que el productor ALBEIRO HERNANDEZ tenga mas pollos muertos que en el ciclo 50 es:
pnbinom(100,(1/2)+100,0.5,lower.tail = F)

#Generalización para las probabilidades predictivas para cada ciclo por productor:
PredictivasP<-data.frame(matrix(NA,48,26))#matriz para almacenar los intervalos
colnames(PredictivasP)=c("cod",colnames(pollosmuertos))
row.names(PredictivasP)=PROPUESTA_FUNDEMERCA_1_$Producor
PredictivasP[,1]=1:48
for (j in 1:length(datos[,1])) {
  for (i in 2:26) {
    if(anyNA(pollosmuertos[j,i-1])=="TRUE"){
      is.na<-PredictivasP[j,i]
    }else{
      PredictivasP[j,i]=pnbinom(pollosmuertos[j,i-1],(1/2)+pollosmuertos[j,i-1],0.5,lower.tail = F)
    }
  }
}
