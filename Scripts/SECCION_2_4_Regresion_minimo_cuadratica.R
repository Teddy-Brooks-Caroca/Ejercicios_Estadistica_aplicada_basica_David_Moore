# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# SECCIÓN 2.4 "Regresión mínimo cuadrática"

# **************************************************
# PREGUNTA 2.30 - Recta de regresión del consumo de gas
# **************************************************

# El ejemplo 2.9 da la ecuación de la recta de regresión del consumo de gas y
# con relación a los grados-día x de los datos de la tabla 2.2 como:
# ŷ = 3,0949 + 0,94966x

# (a) Utiliza la función de regresión de la calculadora para hallar la ecuación de
# la recta de regresión mínimo-cuadrática.

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

ejercicio_2_30 <- read.csv("ejercicio_2_30_grados.csv")

str(ejercicio_2_30)

head(ejercicio_2_30)

regresion <- lm(Gas.m3. ~ Grados.día,data = ejercicio_2_30)

summary(regresion)

"
Call:
lm(formula = Gas.m3. ~ Grados.día, data = ejercicio_2_30)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.9743 -0.4785  0.2235  0.3858  1.8707 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3.09485    0.39062   7.923 1.53e-06 ***
Grados.día   0.94996    0.02498  38.035 1.56e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9539 on 14 degrees of freedom
Multiple R-squared:  0.9904,	Adjusted R-squared:  0.9897 
F-statistic:  1447 on 1 and 14 DF,  p-value: 1.563e-15
"
# (b) Utiliza tu calculadora para hallar la media y la desviación típica de x e y,
# y su correlación r. Halla la pendiente b y la ordenada en el origen a de la recta de
# regresión a partir de esos valores, utilizando las ecuaciones del recuadro Ecuación
# de la recta de regresión mínimo-cuadrática. Comprueba que en (a) y en (b) obtienes la
# ecuación del ejemplo 2.9. (Los resultados pueden ser algo distintos debido a los
# errores de redondeo.)

media_gas <- mean(ejercicio_2_30$Gas.m3.)
media_grados <- mean(ejercicio_2_30$Grados.día)

desv_gas <- sd(ejercicio_2_30$Gas.m3.)
desv_grados <- sd(ejercicio_2_30$Grados.día)

z_gas <- (ejercicio_2_30$Gas.m3. - media_gas) / desv_gas
z_grados <- (ejercicio_2_30$Grados.día - media_grados) / desv_grados

n_muestras <- nrow(ejercicio_2_30)

r <- (1 / (n_muestras - 1)) * sum(z_gas * z_grados)

b <- r * (desv_gas / desv_grados)

a <- media_gas - (b*media_grados)

resultados <- data.frame(
  Variable = c("Gas(m3)","Grados - día"),
  Media = c(media_gas,media_grados),
  Desviacion = c(desv_gas,desv_grados)
)  

resultados_mininmo_cuadratica = data.frame(
  Medida = c("Correlación (r)", "Pendiente (b)","Intercepto (a)"),
  Valores = c(r,b,a)
)

print(resultados)
print(resultados_mininmo_cuadratica)

"
      Variable   Media Desviacion
1      Gas(m3) 14.8625   9.412748
2 Grados - día 12.3875   9.860958

==========================================

           Medida   Valores
1 Correlación (r) 0.9951961
2   Pendiente (b) 0.9499615
3  Intercepto (a) 3.0948517
"
# **************************************************
# PREGUNTA 2.31 - Lluvia ácida
# **************************************************

# Unos investigadores determinaron, durante 150 semanas consecutivas, la acidez 
# de la lluvia en una zona rural de Colorado, EE UU. La acidez se determina 
# mediante el pH. Valores de pH bajos indican una acidez alta. Los investigadores 
# observaron una relación lineal entre el pH y el paso del tiempo e indicaron 
# que la recta de regresión mínimo-cuadrática es:
# pH = 5,43 − (0,0053 × semanas)

# (a) Dibuja esta recta. ¿La asociación es positiva o negativa? Explica de una
# manera sencilla el significado de esta asociación.

intercepto <- 5.43
pendiente <-  -0.0053
semanas <- 0:150 

pH <- intercepto + pendiente * semanas

plot(semanas, 
     pH,
     main = "Evolución del pH de la lluvia en el tiempo",
     xlab = "Semanas",
     ylab = "pH",
     type = "l", 
     lwd = 2, 
     col = "blue",
     ylim = c(4.5, 5.5))
points(c(0, 150), c(5.43, 5.43 - 0.0053*150), pch = 16, col = "red")
grid()

"La asociación es negativa, lo que sugiere que conforme pasan las semanas el nivel de ph 
disminuye, es decir que aumenta la acidez"

# (b) De acuerdo con la recta de regresión, ¿cuál era el pH al comienzo del
# estudio (semana = 1)? ¿Y al final (semana = 150)?

semana_1 <- intercepto - pendiente * 1
semana_150 <- intercepto - pendiente * 150

comparativa_ph_semana <- data.frame(
  Semana = c("Semana 1","Semana 150"),
  pH =c(semana_1,semana_150)
)

print(comparativa_ph_semana)

"
  Semana      pH
  Semana 1    5.4247
  Semana 150  4.6350
"

# (c) ¿Cuál es la pendiente de la recta de regresión? Explica claramente qué indica 
# la pendiente respecto del cambio del pH del agua de lluvia en esta zona rural.

"
b = -0.0053

el pH disminuye 0,0053 unidades por semana (como media)."

# **************************************************
# PREGUNTA 2.32 - Manatís en peligro
# **************************************************

# El ejercicio 2.4 proporciona datos sobre el número de lanchas registradas en 
# Florida y el número de manatís muertos por las lanchas motoras entre 1977 y 1990. 
# La recta de regresión para predecir los manatís muertos a partir del número de 
# lanchas motoras registradas es:
# muertos = −41,4 + (0,125 × lanchas)

# (a) Dibuja un diagrama de dispersión y añádele la recta de regresión. Predice
# el número de manatís que matarán las lanchas en un año en que se registraron
# 716.000 lanchas.

ejercicio_2_32 <- read.csv("ejercicio_2_04_manaties.csv")

head(ejercicio_2_32)

str(ejercicio_2_32)

intercepto <- -41.4
pendiente <- 0.125
lanchas <- 716

mortandad_manaties <- intercepto + pendiente * lanchas

print(mortandad_manaties)

plot(ejercicio_2_32$Licencias_miles,
     ejercicio_2_32$Manatis_muertos,
     main = "Relación entre licencias expedidas y manatíes muertos",
     xlab = "Licencias expedidas (x 1000)",
     ylab = "Manatíes muertos",
     col = "blue",
     pch = 16)
abline(intercepto,
       pendiente,
       col = "red",
       lwd = 2,
       lty = "dashed")
grid()

"En un año en que se autoricen 716000 licencias, puede haber una mortandad de 
48 manatíes muertos"

# (b) He aquí nuevos datos sobre los manatís muertos durante cuatro años más:

# Año    Licencias expedidas (1.000)   Manatís muertos
# 1991   716                          53
# 1992   716                          38
# 1993   716                          35
# 1994   735                          49

# Añade estos puntos al diagrama de dispersión. Durante estos cuatro años,
# Florida tomó fuertes medidas para proteger a los manatís. ¿Observas alguna evi
# dencia de que estas medidas tuvieron éxito?

nuevo_registro <- data.frame(
  Año = c(1991,1992,1993,1994),
  Licencias_miles = c(716,716,716,735),
  Manatis_muertos = c(53,38,35,49)
)

print(nuevo_registro)

plot(ejercicio_2_32$Licencias_miles,
     ejercicio_2_32$Manatis_muertos,
     main = "Relación entre licencias expedidas y manatíes muertos",
     xlab = "Licencias expedidas (x 1000)",
     ylab = "Manatíes muertos",
     col = "blue",
     pch = 16,
     xlim = c(420,750),
     ylim = c(0,60))
points(nuevo_registro$Licencias_miles,
       nuevo_registro$Manatis_muertos,
       col = "green",
       pch = 15,
       cex = 1.2)
abline(intercepto,
       pendiente,
       col = "red",
       lwd = 2,
       lty = "dashed")
grid()

"Han tenido un exito mesurado, pues aunque la cantidad de manatis muerto disminuyó, todavía
se puede apreciar que la cantidad de muertes es proporcional a la cantidad de licencias
expedidas."

# (c) En el apartado (a) predijiste el número de manatís muertos en un año
# con 716.000 lanchas registradas. En realidad, el número de lanchas registradas
# se mantuvo en 716.000 durante los siguientes tres años. Compara las medias de
# manatís muertos en estos años con tu predicción en (a). ¿Qué nivel de exactitud
# has alcanzado?

datos_completos <- rbind(ejercicio_2_32, nuevo_registro)

View(datos_completos)

licencias_716 <- datos_completos[datos_completos$Licencias_miles == 716, ]

print(licencias_716)

promedio__manatis_716 <- mean(licencias_716$Manatis_muertos)

print(promedio__manatis_716)

comparativa <- data.frame(
  Medida = c("promedio","predicción"),
  Valores = c(promedio__manatis_716,mortandad_manaties)
)

print(comparativa)

"
 Año  Licencias_miles Manatis_muertos
 1991             716              53
 1992             716              38
 1993             716              35

======================================

Medida     Valores
promedio        42
predicción      48

"