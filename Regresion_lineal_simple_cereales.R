setwd("~/datasets tp final")
cereales<-read.table("cereales.txt",header=T)
View(cereales)
attach(cereales)

# ANÁLISIS DESCRIPTIVO DE CADA VARIABLE:

#Para la variable X correspondiente a los niveles de Fibra de los cereales:
sort(Fibra)
summary(Fibra) # resumen de 5 numeros
sd(Fibra) # desviación estándar muestral
mean(Fibra,trim=0.1) # media 0.1-podada
mean(Fibra,trim=0.2) # media 0.2-podada
IQR(Fibra) #distancia intercuartil

# Realizamos un histograma: 
par(mfrow=c(1,1)) 
hist(Fibra,freq = FALSE, breaks = seq(0,15,by=1.5), col = "pink", main="gramos de fibra", xlab="fibra")
#Superponemos la curva normal:
curve(dnorm(x, mean(Fibra),sd(Fibra)),add=T,col="red")

# Realizamos un diagrama de caja:
boxplot(Fibra, main="boxplot (fibra)", ylab="Fibra", col="pink")

# Para la variable Y correspondiente a Calorias por porción de los cereales:
sort(Calorias)
summary(Calorias) # resumen de 5 numeros
sd(Calorias) # desviación estándar muestral
mean(Calorias,trim=0.1) # media 0.1-podada
mean(Calorias,trim=0.2) # media 0.2-podada
IQR(Calorias) # distancia intercuartil

# Realizamos un histograma: 
par(mfrow=c(1,1)) 
hist(Calorias,freq = FALSE, breaks = seq(50,160,by=10), col = "orange", main="Calorias por porción", xlab="Calorias")
#Superponemos la curva normal:
curve(dnorm(x, mean(Calorias),sd(Calorias)),add=T,col="red")

# Realizamos un diagrama de caja:
boxplot(Calorias, main="Boxplot (Calorias)", ylab="Calorias por porción", col="orange")

# ajustamos una recta para ver si los datos son razonablemente normales:
qqnorm(Calorias)
qqline(Calorias, col="red")

####################################################################################################################################################

# EL SIGUIENTE PASO ES COMENZAR A PLANTEAR EL MODELO LINEAL:

#Graficamos PARA FIBRA-CALORIAS la recta del ajuste por cuadrados minimos junto con los puntos:
regresionF <- lm(Calorias ~ Fibra)
summary(regresionF)


# SSE:
SSE=((15.72)^2)*(length(Fibra)-2)
SSE

#la varianza del error, se estima por SSE/n-2 
scuadrado=SSE/34
scuadrado

#Analisis de residuos:
plot(Fibra, Calorias, xlab='Fibra', ylab='Calorias')
abline(regresionF, col="maroon")
plot(Fibra, resid(regresionF), ylab="Residuos")
qqline(y=0)
plot(Calorias,resid(regresionF), ylab="Residuos")
qqline(y=0)
plot(fitted(regresionF), resid(regresionF), xlab="valores ajustados", ylab="Residuos")
qqnorm(resid(regresionF))
qqline(resid(regresionF), col="blue")
bp<-boxplot(resid(regresionF), ylab="Residuos", main="boxplot de residuos")
bp$out
hist(resid(regresionF), freq=F, xlab="Residuos", col="bisque3")

# Observamos la presencia de outliers, y una dependencia de los residuos y los valores ajustados
# con una homocedasticidad baja (los residuos no tienen variabilidad constante entorno al 0).
# en este caso no se presenta normalidad de los residuos. La gráfica de valores ajustados muestra que los puntos
# se agrupan en la parte inferios derecha de la gráfica. La gráfica de calorias-residuos muestra una posible 
# relación curvilinea.

# debemos entonces quitar datos atipicos para ver si los valores del modelo mejoran ( buscamos un coef.de correlacion mas alto,
# una homocedasticidad mas alta y menor dependencia de los residuos con los valores ajustados)

# TRANSFORMAMOS LA VARIABLE RESPUESTA:
#Se transforma la variable respuesta y se quitan los datos atípicos y de alta influencia:
#punto de alta influencia en 3, se observan los casos para cuando está incluido y no el punto:
regresionFF <- lm(100*(Calorias)^(-1) ~ Fibra,cereales[-c(27,28,9),])
summary(regresionFF)
plot(Fibra[-c(27,28,9)], 100*(Calorias[-c(27,28,9)])^(-1), xlab='Fibra', ylab='Calorias')
abline(regresionFF, col="red")

regresionFF <- lm(100*(Calorias)^(-1) ~ Fibra,cereales[-c(27,28,9,3,4,33,35),])  
summary(regresionFF)
plot(Fibra[-c(27,28,9,3,4,33,35)], 100*(Calorias[-c(27,28,9,3,4,33,35)])^(-1), xlab='Fibra', ylab='Calorias')
abline(regresionFF, col="green")

#El dato 3 fue eliminado por ser un punto de alta influencia. Observar que los valores para las rectas
# ajustadas cambian bastante al quitarlo.

sort(fibraSinOut)
summary(Fibra[-c(27,28,9,3,4,33,35)])
mean(Fibra[-c(27,28,9,3,4,33,35)])
sd(Fibra[-c(27,28,9,3,4,33,35)])
IQR(Fibra[-c(27,28,9,3,4,33,35)])
mean(Fibra[-c(27,28,9,3,4,33,35)],trim=0.1)
mean(Fibra[-c(27,28,9,3,4,33,35)],trim=0.2)
m=length(Fibra[-c(27,28,9,3,4,33,35)]) 
m #29

#Analisis de residuos:
plot(Fibra[-c(27,28,9,3,4,33,35)], resid(regresionFF), ylab="Residuos", xlab="Fibra")
qqline(y=0)
plot(100*(Calorias[-c(27,28,9,3,4,33,35)])^(-1),resid(regresionFF), ylab="Residuos", xlab="Calorías")
qqline(y=0)
plot(fitted(regresionFF), resid(regresionFF), xlab="valores ajustados", ylab="Residuos")
qqline(y=0)
qqnorm(resid(regresionFF))
qqline(resid(regresionFF), col="blue")
bp<-boxplot(resid(regresionFF), ylab="residuos", col="bisque2")
bp$out
hist(resid(regresionFF), freq=F, xlab="residuos", col="bisque2")

                       ########################################################
                       #   Y= 0.872564 + 0.056897x recta de regresión final   #
                       ########################################################    

# La VARIANZA DEL ERROR ahora es:
SSE=((0.06619)^2)*(length(Fibra[-c(27,28,9,3,4,33,35)])-2)  # s=0.06619 raíz de la varianza s²
SSE # 0.1182901

fibraSinOut<-Fibra[-c(27,28,9,3,4,33,35)]

Sxx=(var(fibraSinOut))*(length(fibraSinOut)-1)
Sxx
#Sxy=((cov(Fibra[-c(27,28,9,3,4,33,35)],Calorias[-c(27,28,9,3,4,33,35)]))*(length(Fibra[-c(27,28,9,3,4,33,35)]-1)))
#Sxy

##calculamos c00:
C00= function(x){
  resultado1<-0
  resultado2<-0
  for(i in x){
    resultado1<- resultado1 + (i)^2
    resultado2<- resultado2 + (i-mean(x))^2
  }
  return(resultado1/(length(x)*resultado2))
}
C00(fibraSinOut)
#otra forma:
sumita=function(x){
  res<-0
  for(i in x){
    res<- res + (i)^2
  }
  return(res)
}
c00=((sumita(fibraSinOut))/(length(fibraSinOut)*Sxx))
c00

##calculamos c11:
C11= function(x){
  resultado<-0
  for(i in x){
    resultado<- resultado + (i - mean(x))^2
  }
  return(1/(resultado))
}
C11(fibraSinOut)
#otra forma:
c11=1/Sxx
c11

#---------------------------------------------------------------------------------------------------------------

# INTERVALOS DE CONFIANZA DE NIVEL 95% PARA LOS PARÁMETROS DE LA RELACIÓN fibra-calorias:

#PARA B0:
liminF= 0.872564 - qt(1-0.95/2, length(fibraSinOut)-2)* 0.06619*sqrt(C00(fibraSinOut))
liminF
limsuP= 0.872564 + qt(1-0.95/2, length(fibraSinOut)-2)* 0.06619*sqrt(C00(fibraSinOut))
limsuP

#PARA B1:
liminF= 0.056897 - qt(1-0.95/2, length(fibraSinOut)-2)* 0.06619*sqrt(C11(fibraSinOut))
liminF
limsuP= 0.056897 + qt(1-0.95/2, length(fibraSinOut)-2)* 0.06619*sqrt(C11(fibraSinOut))
limsuP
  
# PARA LA VARIANZA DEL ERROR:
liminf=((length(fibraSinOut)-2)*(0.066^2))/qchisq(1-(0.95/2),length(fibraSinOut)-2)
liminf
limsup=((length(fibraSinOut)-2)*(0.066^2))/qchisq(0.95/2,length(fibraSinOut)-2)
limsup

#-----------------------------------------------------------------------------------------------------------------------------------------

# TEST DE HIPÓTESIS PARA LA ORDENADA AL ORIGEN B0, DE NIVEL DE SIGNIFICACIÓN 0.05:

qt(0.975, 27)
#con 0 (no sirve)
#t=((0.872564-0)/(0.06619*sqrt(C00(fibraSinOut))))
#t
#pt(51.25,27)
#C00(firaSinOut)

#con 0.8
t=((0.872564-0.8)/(0.06619*sqrt(C00(fibraSinOut))))
t
pt(4.26,27)

#------------------------------------------------------------------------------------------------------------------------------------------

#TEST DE HIPÓTESIS PARA LA PENDIENTE B1, DE NIVEL DE SIGNIFICACIÓN 0.05:

qt((0.95),27)

#con 0 (no sirve)
#t=((0.056897-0)/(0.06619*sqrt( 0.003060712))) ###### arreglar###
#t
#pt(-15.53,length(FibraF)-2)
#h=1-5.514011e-14
#h
#c11(fibraSinOut)

#mayor que 0.05:
t=((0.056897-0.05)/(0.06619*sqrt( 0.003060712))) 
t
pt(1.88,length(fibraSinOut)-2)

#--------------------------------------------------------------------------------------------------------------------------------

fibraSinOut <- Fibra[-c(27,28,9,3,4,33,35)]

# IC PARA LA ESPERANZA DE Y:

EY= function(x){
  return(0.872564 + (0.056897*x))
}

#cuando x* es el promedio de x:
limInf<-EY(mean(fibraSinOut))- qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
limInf
limSup=EY(mean(fibraSinOut))+ qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
limSup

L=2*qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
L #longitud
                                
summary(fibraSinOut)

#cuando x* es el el primer cuartil:
summary(fibraSinOut) #x*=1.000

limInf<-EY(1.00)- qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((1.000-mean(fibraSinOut))^2)/Sxx)
limInf
limSup=EY(1.00)+ qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((1.000-mean(fibraSinOut))^2)/Sxx)
limSup

L=2*qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((1.000-mean(fibraSinOut))^2)/Sxx)
L #longitud

#cuando x* es el tercer cuartil: 
#x*=4.000

limInf<-EY(4.000)- qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((4.000-mean(fibraSinOut))^2)/Sxx)
limInf
limSup=EY(4.000)+ qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((4.000-mean(fibraSinOut))^2)/Sxx)
limSup

L=2*qt(1-(0.05/2),27)*0.06619*sqrt(1/length(fibraSinOut)+((4.000-mean(fibraSinOut))^2)/Sxx)
L #longitud


#-----------------------------------------------------------------------------------------------------------------------------------

#INTERVALOS DE PREDICCIÓN PARA LA VARIABLE RESPUESTA Y: 

#cuando x* es el PRIMER cuartil:
#x*=1.000
limInfP<-EY(1.000)- qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((1.000-mean(fibraSinOut))^2)/Sxx)
limInfP
limSupP=EY(1.000)+ qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((1.000-mean(fibraSinOut))^2)/Sxx)
limSupP

L=2*qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((1.000-mean(fibraSinOut))^2)/Sxx)
L #longitud

#CUANDO X* ES EL PROMEDIO DE X:

limInfP<-EY(mean(fibraSinOut))- qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
limInfP
limSupP=EY(mean(fibraSinOut))+ qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
limSupP

L=qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((mean(fibraSinOut)-mean(fibraSinOut))^2)/Sxx)
L #longitud
2*
#cuando x* es el tercer cuartil:
#x*=4.000
limInfP<-EY(4.000)- qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((4.000-mean(fibraSinOut))^2)/Sxx)
limInfP
limSupP=EY(4.000)+ qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((4.000-mean(fibraSinOut))^2)/Sxx)
limSupP

L=2*qt(1-(0.05/2),27)*0.06619*sqrt(1+(1/length(fibraSinOut))+((4.000-mean(fibraSinOut))^2)/Sxx)
L #longitud







 
 
