# Antonio Noguerón Bárcenas A01423759

# Actividad Regresión lineal simple y validación por Puntos Extras

# Librerías
library(readxl)
library(GGally)
library(broom)
library(psych)


# Importar datos de situación problema
historico <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Histórico de defectos")
muestreo <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Datos de muestreo", skip=1)
indice <- read_excel("Datos_Situacion_Problema.xlsx", sheet="Índice")

attach(muestreo)

#Regresión lineal simple con una de las variables con más correlación

regresionSimple = function(X,Y, nombreX, nombreY){
  validoConI = FALSE
  validoSinI = FALSE
  print("")
  print(paste(c("        Modelo de regresión lineal simple del ", nombreY, " contra ", nombreX), collapse = ""))
  #Probabilidad de equivocarse Alpha, se busca que sea baja. 5%
  alpha = 0.05
  
  modelo_conIntercepto = lm(Y ~ X)
  resumenConI = summary(modelo_conIntercepto)
  
  modelo_sinIntercepto = lm(Y ~ X - 1)
  resumenSinI = summary(modelo_sinIntercepto)
  
  print("")
  print("-------------La ecuación inicial de nuestro modelo lineal simple con Intercepto es --------------")
  coeff_x_conI = modelo_conIntercepto$coefficients[2]
  coeff_I = modelo_conIntercepto$coefficients[1]
  ecuacionConI = paste(c(nombreY," = ", round(coeff_I, 4), " + ", round(coeff_x_conI,4), "*", nombreX), collapse = "")
  print(ecuacionConI)
  print("")
  print("----- Valores P con intercepto ------")
  pValue_x_conI = resumenConI$coefficients["X", "Pr(>|t|)"]
  pValue_Intercept = resumenConI$coefficients["(Intercept)", "Pr(>|t|)"] 
  print(paste(c("El valor-p de nuestra variable independiente ", nombreX, " es: ", pValue_x_conI), collapse = ""))
  print(paste(c("El valor-p de nuestra intersección es: ", pValue_Intercept), collapse = ""))
  
  print("")
  print("Se realiza la validación estadística de los coeficientes con las hipótesis y un valor alpha de 0.05")
  print("")
  
  if (pValue_x_conI >= alpha) {
    print("El Valor-P de la variable independiente es mayor que alpha, no se rechaza la hipótesis nula.")
    print("     B1 = 0, No existe modelo para esta variable.")
  } else if (pValue_Intercept >= alpha) {
    print("El Valor-P del intercepto es mayor que alpha, no se rechaza la hipótesis nula.")
    print("     B0 = 0, el intercepto es 0, se vuelve a realizar el modelo sin intercepto.")
  } else{
    print("Ambos valores-p son menores que alpha, la ecuación inicial del modelo se mantiene.")
    validoConI = TRUE
  }
  
  rCuadradaConI = resumenConI$r.squared
  errorEstandarConI = resumenConI$sigma
  
  print("")
  print(paste(c("El error estándar del modelo con Intercepto es de: ", errorEstandarConI), collapse = ""))
  print("")
  print("")
  print(paste(c("La R² del modelo con Intercepto es de: ", rCuadradaConI), collapse = ""))
  print("")
  
  print("")
  print("-------------La ecuación inicial de nuestro modelo lineal simple sin Intercepto es --------------")
  coeff_x_sinI = modelo_sinIntercepto$coefficients[1]
  ecuacionSinI = paste(c(nombreY," = ", round(coeff_x_sinI,4), "*", nombreX), collapse = "")
  print(ecuacionSinI)
  print("")
  print("----- Valores P sin intercepto ------")
  pValue_x_sinI = resumenSinI$coefficients["X", "Pr(>|t|)"]
  print(paste(c("El valor-p de nuestra variable independiente ", nombreX, " es: ", pValue_x_sinI), collapse = ""))
  
  print("")
  print("Se realiza la validación estadística de los coeficientes con las hipótesis y un valor alpha de 0.05")
  print("")
  
  
  if (pValue_x_sinI >= alpha) {
    print("El Valor-P de la variable independiente es mayor que alpha, no se rechaza la hipótesis nula.")
    print("  B1 = 0, No existe modelo para esta variable.")
  } else {
    print("El valor-p de la variable independiente es menor que alpha, se rechaza la hipótesis nula, B1 != 0.")
    validoSinI = TRUE
  }
  
  rCuadradaSinI = resumenSinI$r.squared
  errorEstandarSinI = resumenSinI$sigma
  
  print("")
  print(paste(c("El error estándar del modelo sin Intercepto es de: ", errorEstandarSinI), collapse = ""))
  print("")
  print("")
  print(paste(c("La R² del modelo sin Intercepto es de: ", rCuadradaSinI), collapse = ""))
  print("")
  
  if (validoConI == TRUE & validoSinI == TRUE){
    print("")
    print("Ya que ambos modelos son válidos, buscaremos el mejor usando el coeficiente de determinación R²")
    if (rCuadradaConI > rCuadradaSinI){
      print("El modelo con Intercepto tiene mayor R²")
      print("Éste es el modelo válido")
      modelo = modelo_conIntercepto
      resumen = summary(modelo)
      ecuacion = ecuacionConI
      validoSinI = FALSE
    } else {
      print("El modelo sin Intercepto tiene mayor R²")
      print("Éste es el modelo válido")
      modelo = modelo_sinIntercepto
      resumen = summary(modelo)
      ecuacion = ecuacionSinI
      validoConI = FALSE
    }
  } else if (validoConI == TRUE){
      print("")
      print("El modelo con Intercepto es el modelo válido")
      modelo = modelo_conIntercepto
      resumen = summary(modelo)
      ecuacion = ecuacionConI
  } else if (validoSinI == TRUE){
      print("")
      print("El modelo sin Intercepto es el modelo válido")
      modelo = modelo_sinIntercepto
      resumen = summary(modelo)
      ecuacion = ecuacionSinI
  } else {
      print("")
      print("Ningún modelo es válido")
      ecuacion = "No hubo un modelo válido que represente los datos de las variables"
  }
  print("")
  print("__________________________________________________________________________________")
  print("")
  print(ecuacion)
  print("")
  if (validoConI == TRUE){
    print(paste(c("Esto quiere decir que por cada unidad que incremente ", nombreX, ", la variable ", nombreY, " aumenta un promedio de ", round(coeff_x_conI,4), " más un valor de ", round(coeff_I,4), " que no depende de ninguna variable."),collapse = ""))
  } else {
    print(paste(c("Esto quiere decir que por cada unidad que incremente ", nombreX, ", la variable ", nombreY, " aumenta un promedio de ", round(coeff_x_sinI,4), "."),collapse = "")) 
  }
  print("")
  intervaloConfianza <- confint(modelo, level = 0.95)
  print("---- Intervalo de Confianza ----")
  print(paste(c("El coeficiente B1 está entre ", intervaloConfianza[1], " y ", intervaloConfianza[2], " con un 95% de confianza."),collapse = ""))
  print(paste(c("La R² del modelo es de: ", resumen$r.squared), collapse = ""))
  print(paste(c("Lo que significa que solo el ", round((resumen$r.squared)*100,1), "% de la variabilidad de la variable respuesta está siendo explicado por el modelo"),collapse = ""))
  print("")
  print("")
  print(glance(modelo))
  plot(modelo)
  return(modelo)
  }

#regresionSimple(A,Y, "Presión de la Bomba", "Porcentaje de Defectos")
#regresionSimple(B,Y, "Temperatura del plástico 3", "Porcentaje de Defectos")
#regresionSimple(C,Y, "Temperatura del plástico 4", "Porcentaje de Defectos")
modeloD = regresionSimple(D,Y, "Temperatura del tornillo", "Porcentaje de Defectos")
#regresionSimple(E,Y, "RPM", "Porcentaje de Defectos")
#regresionSimple(F,Y, "Temperatura del Barril", "Porcentaje de Defectos")
#regresionSimple(G,Y, "Velocidad de Extrusión", "Porcentaje de Defectos")
#regresionSimple(H,Y, "Temperatura de enfriadores", "Porcentaje de Defectos")
anova(modeloD)
summary(modeloD)

library(lmtest)

dwtest(modeloD, data = inteligencia)
#bptest(modeloD)

hist(modeloD$residuals)

library(nortest)
lillie.test(modeloD$residuals)


