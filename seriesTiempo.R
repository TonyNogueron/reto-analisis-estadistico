library(readxl)
historicoA <- read_excel("Datos_Situacion_Problema.xlsx", 
                                       sheet = "Producto A", col_types = c("numeric", 
                                                                           "numeric", "numeric"), skip = 2)

historicoB <- read_excel("Datos_Situacion_Problema.xlsx", 
                         sheet = "Producto B", col_types = c("numeric", 
                                                             "numeric", "numeric"), skip = 2)

historicoC <- read_excel("Datos_Situacion_Problema.xlsx", 
                         sheet = "Producto C", col_types = c("numeric", 
                                                             "numeric", "numeric"), skip = 2)

str(historicoA)
str(historicoB)
str(historicoC)

productoA = ts(historicoA$`defectuosas`, start = 1, frequency = 12); #productoA
productoB = ts(historicoB$`defectuosas`, start = 1, frequency = 12); #productoB
productoC = ts(historicoC$`defectuosas`, start = 1, frequency = 12); #productoC

boxplot(productoA~ cycle(productoA))
boxplot(productoB~ cycle(productoA))
boxplot(productoC~ cycle(productoA))
cycle(productoA)
cycle(productoB)
cycle(productoC)

# DESCOMPOSICIÓN DE UNA SERIE
productoA.ts.desc = decompose(productoA); plot(productoA.ts.desc,xlab = "Año")
productoB.ts.desc = decompose(productoB); plot(productoB.ts.desc,xlab = "Año")
productoC.ts.desc = decompose(productoC); plot(productoC.ts.desc,xlab = "Año")

productoA.des = productoA-productoA.ts.desc$seasonal
plot(productoA.des,main="Año")
productoB.des = productoB-productoB.ts.desc$seasonal
plot(productoB.des,main="Año")
productoC.des = productoC-productoC.ts.desc$seasonal
plot(productoC.des,main="Año")

# TRANSFORMACIONES BÁSICAS DE UNA SERIE
# 1. Estabilización de la varianza
plot(log(productoA))
plot(log(productoB))
plot(log(productoC))
#2. Eliminación de tendencia
x = log(productoA); difA.x = diff(x); plot(difA.x)
x = log(productoB); difB.x = diff(x); plot(difB.x)
x = log(productoC); difC.x = diff(x); plot(difC.x)
# 3. Eliminación de estacionalidad
difA2.difA.x = diff(difA.x,lag=12); plot(difA2.difA.x)
difB2.difB.x = diff(difB.x,lag=12); plot(difB2.difB.x)
difC2.difC.x = diff(difC.x,lag=12); plot(difC2.difC.x)

# FUNCIONES DE AUTOCOVARIANZAS Y DE AUTOCORRELACIONES
yA = difA2.difA.x; acf(yA)
yB = difB2.difB.x; acf(yB)
yC = difC2.difC.x; acf(yC)
# Valores numéricos
acf(yA,plot=FALSE)$acf
acf(yB,plot=FALSE)$acf
acf(yC,plot=FALSE)$acf

# RUIDO BLANCO
ruido_blanco = rnorm(1000,0,1)
plot.ts(ruido_blanco, main="Ejemplo de Ruido Blanco", xlab="Tiempo", ylab="Valores",col="6")
# Prueba de hipótesis para verificar si es ruido blanco
Box.test(ruido_blanco)

