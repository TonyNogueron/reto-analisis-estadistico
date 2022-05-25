# Antonio Noguerón
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
library(psych)


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


# Graficar cada variable respecto al porcentaje de error

plot(muestreo$A, muestreo$Y, main="Gráfica presión v.s. porcentaje de defectos", xlab="Presión", ylab="Defectos %")
plot(muestreo$B, muestreo$Y, main="Gráfica temp plástico 3 v.s. porcentaje de defectos", xlab="Temp plástico 3", ylab="Defectos %")
plot(muestreo$C, muestreo$Y, main="Gráfica temp plástico 4 v.s. porcentaje de defectos", xlab="Temp plástico 4", ylab="Defectos %")
plot(muestreo$D, muestreo$Y, main="Gráfica temp tornillo v.s. porcentaje de defectos", xlab="Temp tornillo", ylab="Defectos %")
plot(muestreo$E, muestreo$Y, main="Gráfica RPM v.s. porcentaje de defectos", xlab="RPM", ylab="Defectos %")
plot(muestreo$F, muestreo$Y, main="Gráfica temp barril v.s. porcentaje de defectos", xlab="Temp barril", ylab="Defectos %")
plot(muestreo$G, muestreo$Y, main="Gráfica velocidad v.s. porcentaje de defectos", xlab="Velocidad", ylab="Defectos %")
plot(muestreo$H, muestreo$Y, main="Gráfica temp enfriadores v.s. porcentaje de defectos", xlab="Temp enfriadores", ylab="Defectos %")
plot(muestreo$I, muestreo$Y, main="Gráfica materia prima v.s. porcentaje de defectos", xlab="Materia prima", ylab="Defectos %")

# Gráficas múltiples
multi.hist(x = muestreo, dcol = c("blue", "red"),
           dlty = c("dotted","solid"), lwd = c(2,1),
           main = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "Y"))

# Gráfica correlación
corrplot(cor(muestreo))

# Gráficas ggpairs
ggpairs(muestreo)
ggpairs(muestreo, upper = list(coninuous = "smooth"),
        lower = list(continuous = "blank"),
        diag = list(continuous = "densityDiag"))

# Regresión lineal inicial

regresion <- lm(Y ~ A + B + C + D + E + F + G + H + I, data=muestreo)
summary(regresion)

# Regresión lineal ajustada 1

regresion1 <- lm(Y ~ A + B + C + D + E + F, data=muestreo)
summary(regresion1)

# Regresión lineal ajustada 2
regresion2 <- lm(Y ~ B + C + D + E + F + H, data=muestreo)
summary(regresion2)

# Modelo de predicción

datos_nuevos <- data.frame(A=73,B=213,C=220,D=195,E=110,F=130,G=4.5,H=70,I=2)
predict(regresion1, datos_nuevos)