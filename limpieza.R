# Antonio Noguerón Bárcenas A01423759
# Annette Pamela Ruiz Abreu A01423595
# Paulina Galindo Rodríguez A01424818

# Primer Análisis de Datos

# Librerías

library(dplyr)
library(tidyr)
library(readr)
library(skimr)
library(readxl)
library(ggplot2)
library(corrplot)
library(GGally)
library(broom)
library(psych)
library(lmtest)
library(nortest)


# Importar datos de situación problema
historico <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Histórico de defectos")
muestreo <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Datos de muestreo", skip=1)
indice <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Índice")

# Exploración de los datos de muestreo
head(muestreo)
str(muestreo)
resumen <- summary(muestreo)
colnames <- names(muestreo)
num_datos <- nrow(muestreo)

#Cambio de nombre a las variabkes
attach(muestreo)
#        NUEVO                 # ANTERIOR
datos<- rename(muestreo, PresionBomba = A,
                TempPlastico3Bomba  = B,
                TempPlastico4Mezcladora = C,
                TempTornilloUsillo = D,
                RpmTornilloUsillo = E,
                TempBarril = F,
                VelocidadExtrusion = G,
                TempEnfriadores = H,
                TipoMateria = I,
                PorcentajeDefectos = Y
               )

names(datos)
attach(datos)


# Graficar cada variable respecto al porcentaje de error

#plot(A, Y, main="Gráfica presión v.s. porcentaje de defectos", xlab="Presión", ylab="Defectos %")
#plot(B, Y, main="Gráfica temp plástico 3 v.s. porcentaje de defectos", xlab="Temp plástico 3", ylab="Defectos %")
#plot(C, Y, main="Gráfica temp plástico 4 v.s. porcentaje de defectos", xlab="Temp plástico 4", ylab="Defectos %")
#plot(D, Y, main="Gráfica temp tornillo v.s. porcentaje de defectos", xlab="Temp tornillo", ylab="Defectos %")
#plot(E, Y, main="Gráfica RPM v.s. porcentaje de defectos", xlab="RPM", ylab="Defectos %")
#plot(F, Y, main="Gráfica temp barril v.s. porcentaje de defectos", xlab="Temp barril", ylab="Defectos %")
#plot(G, Y, main="Gráfica velocidad v.s. porcentaje de defectos", xlab="Velocidad", ylab="Defectos %")
#plot(H, Y, main="Gráfica temp enfriadores v.s. porcentaje de defectos", xlab="Temp enfriadores", ylab="Defectos %")
#plot(I, Y, main="Gráfica materia prima v.s. porcentaje de defectos", xlab="Materia prima", ylab="Defectos %")

# Gráficas múltiples
#multi.hist(x = muestreo, dcol = c("blue", "red"),
#           dlty = c("dotted","solid"), lwd = c(2,1),
#           main = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "Y"))

# Correlograma
#corrplot(cor(datos))

# Gráficas ggpairs
#ggpairs(datos)
#ggpairs(datos, upper = list(coninuous = "smooth"),
#        lower = list(continuous = "blank"),
#        diag = list(continuous = "densityDiag"))


#Al ver el correlograma, vimos que la velocidad de extrusión
# y la Presión de la bomba están relacionadas.
#plot(PresionBomba, VelocidadExtrusion, main="Gráfica Presión de la Bomba \n v.s. Velocidad de extrusión", xlab="Presión de la Bomba [Bar]", ylab="Velocidad Extrusión [m/s]")
#plot(TempPlastico4Mezcladora, PorcentajeDefectos, main="Gráfica de Temperatura de Plástico 4 (mezcladora) \n v.s. Porcentaje de Defectos", xlab="Temperatura Plástico 4 [°C]", ylab="Defectos [%]")
#plot(TempPlastico3Bomba, PorcentajeDefectos, main="Gráfica de Temperatura de Plástico 3 (bomba) \n v.s. Porcentaje de Defectos", xlab="Temperatura Plástico 3 [°C]", ylab="Defectos [%]")
#plot(TempBarril, PorcentajeDefectos, main="Gráfica de Temperatura del Barril \n v.s. Porcentaje de Defectos", xlab="Temperatura del Barril [°C]", ylab="Defectos [%]")



#Regresión lineal simple con una de las variables con más correlación
regresionSimple <- lm(Y ~ C, data=muestreo)
summary(regresionSimple)

# Regresión lineal inicial

regresion <- lm(Y ~ A + B + C + D + E + F + G + H + I, data=muestreo)
summary(regresion)

# Regresión lineal ajustada 1

regresion1 <- lm(Y ~ A + B + C + D + E + F, data=muestreo)
summary(regresion1)
glance(regresion1)

# Regresión lineal ajustada 2
regresion2 <- lm(Y ~ B + C + D + E + F + H, data=muestreo)
summary(regresion2)
glance(regresion2)

#Regresión con las variables que tiene un valor p < 0.05
regresion3 <- lm(Y ~ B + C + D + E + F, data=muestreo)
summary(regresion3)
glance(regresion3)

#plot(TempPlastico4Mezcladora, PorcentajeDefectos)
#abline(regresionSimple, col = "red")

# Modelo de predicción

datos_nuevos <- data.frame(A=73,B=213,C=220,D=195,E=110,F=130,G=4.5,H=70,I=2)
predict(regresion1, datos_nuevos)


#dwtest(bestLm, data = muestreo)
#bptest(bestLm)

#hist(bestLm$residuals)


#lillie.test(bestLm$residuals)

step(regresion)
bestLm = lm(formula = Y ~ A + B + C + D + E + F, data = muestreo)
summary(mejor_modelo_step)
#anova(mejor_modelo_step)

#Dejando las demás variables con el valor de sus medias, haremos una predicción del porcentaje de defectos con presión de la bomba:

#baja
datos_nuevos <- data.frame(A=20,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=45,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=75,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)


#Dejando las demás variables con el valor de sus medias, haremos una predicción del porcentaje de defectos con B:
#min = 167
#max 256


#baja
datos_nuevos <- data.frame(A=36.98,B=170,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=36.98,B=255,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#C
#min 184
#max 252

#baja
datos_nuevos <- data.frame(A=36.98,B=221.7,C=185,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=36.98,B=221.7,C=250, D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)


#D
#min 186
#max 264

#baja
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=190,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5, D=261,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

# A menor D

#E
#min 74
#mean 98.51
#max 129

#baja
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=78,F=146.7)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5, D=220.3,E=125,F=146.7)
predict(bestLm, datos_nuevos)

#A mayor E

#F
#min 108
#mean 146.7
#max 189

#baja
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=112)
predict(bestLm, datos_nuevos)

#media
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5,D=220.3,E=98.51,F=146.7)
predict(bestLm, datos_nuevos)

#alta
datos_nuevos <- data.frame(A=36.98,B=221.7,C=214.5, D=220.3,E=98.51,F=180)
predict(bestLm, datos_nuevos)

#A mayor F



datos_nuevos <- data.frame(A=68,B=239.2,C=249.3, D=186,E=151.2,F=178)
predict(bestLm, datos_nuevos)



