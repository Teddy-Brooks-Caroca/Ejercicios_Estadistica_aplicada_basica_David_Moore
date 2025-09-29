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

a <- media_gas - (b * media_grados)

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

# **************************************************
# PREGUNTA 2.33 - El profesor Moore y la natación
# **************************************************

# He aquí los tiempos (en minutos) que tarda el profesor Moore en nadar 1.800 metros 
# y su ritmo cardíaco después de bracear (en pulsaciones por minuto) en 23 sesiones de natación.

# Minutos:   34,12 35,72 34,72 34,05 34,13 35,72 36,17 35,57 35,37 35,57 35,43 
#            36,05 34,85 34,70 34,75 33,93 34,60 34,00 34,35 35,62 35,68 35,28 35,97

# Pulsaciones: 152 124 140 152 146 128 136 144 148 144 136 124 148 144 140 156 
#              136 148 148 132 124 132 139

# (a) Un diagrama de dispersión muestra una relación lineal negativa relativa
# mente fuerte. Utiliza tu calculadora o un programa informático para comprobar
# que la recta de regresión mínimo-cuadrática es
# pulsaciones = 479,9 − (9,695 × minutos)

ejercicio_2_33 <- read.csv("ejercicio_2_33_natacion.csv")

str(ejericicio_2_33)

View(ejercicio_2_33)

intercepto = 479.9
pendiente = -9.695 

correlacion <- cor(ejercicio_2_33$Minutos,ejercicio_2_33$Pulsaciones)
regresion <- lm(Pulsaciones ~ Minutos, data = ejercicio_2_33)

summary(regresion)

resultados_programa <- data.frame(
  Medida = c("Correlacion", "Intercepto","Pendiente"),
  Valor_calculado = c(correlacion,coef(regresion)[1],coef(regresion)[2]),
  Valor_otorgado = c("No dado",intercepto,pendiente)
)

print(resultados_programa)

plot(ejercicio_2_33$Minutos, ejercicio_2_33$Pulsaciones,
     main = "Tiempo de natación vs Ritmo cardíaco",
     xlab = "Minutos (1800 metros)",
     ylab = "Pulsaciones por minuto",
     col = "blue", pch = 16)
abline(a = 479.9, b = -9.695, col = "red", lwd = 2)
abline(regresion, col = "green", lty = 2, lwd = 2)

"   
Medida       Valor_calculado  Valor_otorgado
Correlacion      -0.7459841          No dado
Intercepto      479.9341457            479.9
Pendiente        -9.6949034           -9.695
"

# (b) Al siguiente día el profesor tardó 34,30 minutos. Predice su ritmo cardíaco.
# En realidad su pulso fue 152. ¿Cómo de exacta es tu predicción?


dia_siguiente <- intercepto + (pendiente * 34.30)

valor_real <- 152
valor_predicho <- dia_siguiente

error_absoluto <- abs(valor_real - valor_predicho)
error_relativo <- (error_absoluto / valor_real) * 100
exactitud <- 100 - error_relativo

comparativa <- data.frame(
  Medidas = c("Valor real", "Valor predicho", "Error absoluto", "Error relativo", "Exactitud"),
  Valores = c(valor_real, 
              round(valor_predicho, 1),
              round(error_absoluto, 1),
              paste0(round(error_relativo, 1), "%"),
              paste0(round(exactitud, 1), "%"))
)

print(comparativa)

"
 Medidas        Valores
 Valor real     152.0
 Valor predicho 147.4
 Error absoluto   4.6
 Error relativo   3.1%
 Exactitud       96.9%
"

# (c) Supón que sólo conociéramos que las pulsaciones fueron 152. Ahora quieres 
# predecir el tiempo que el profesor estuvo nadando. Halla la recta de regresión
# mínimo-cuadrática apropiada para la ocasión. ¿Cuál es tu predicción? ¿Es muy
# exacta?

pulsaciones <- 152 

predecir <- (intercepto - pulsaciones) /abs(pendiente)

tiempo_real <- 34.30
error <- abs(tiempo_real - predecir)
error_porcentual <- (error / tiempo_real) * 100
exactitud <- 100 - error_porcentual

comparativa <- data.frame(
  Medidas = c("Tiempo real", "Tiempo predicho", "Error absoluto", "Exactitud"),
  Valores = c(paste0(round(tiempo_real, 2), " min"),
              paste0(round(predecir, 2), " min"),
              paste0(round(error, 2), " min"),
              paste0(round(exactitud, 1), "%"))
)

print(comparativa)

"
Medidas         Valores
Tiempo real     34.30 min
Tiempo predicho 33.82 min
Error absoluto   0.48 min
Exactitud       98.6%
"
# (d) Explica de forma clara, a alguien que no sepa estadística, por qué las dos
#     rectas de regresión son distintas.

"Las dos rectas de regresión son diferentes porque cada una está diseñada para hacer 
una predicción específica: una predice las pulsaciones cuando conoces el tiempo de natación, 
y la otra predice el tiempo de natación cuando conoces las pulsaciones. Como cada recta se ajusta 
para minimizar los errores en su propia predicción, terminan siendo líneas distintas, cada 
una optimizada para su propósito particular."

# **************************************************
# PREGUNTA 2.34 - Predicción del comportamiento de mercados de valores
# **************************************************

# Algunas personas creen que el comportamiento de un mercado de valores en enero 
# permite predecir el comportamiento del mercado durante el resto del año. 
# Toma como variable explicativa x el porcentaje de cambio en el índice del mercado 
# de valores en enero y como variable respuesta y la variación del índice a lo largo de todo el año.

# Cálculos a partir de datos del periodo 1960-1997 dan:
# x̄ = 1,75%    ȳ = 9,07%
# sx = 5,36%   sy = 15,35%
# r = 0,596

# (a) ¿Qué porcentaje de la variación observada en los cambios anuales del
# índice se explica a partir de la relación lineal con el cambio del índice en enero?

r2 <- 0.596 * 0.596

print(paste0(round(r2, 2)*100, "%"))

"El 36% de la variación observada en los cambios anuales del índice se explica por 
la relación lineal con el cambio en enero."

# (b) ¿Cuál es la ecuación de la recta mínimo-cuadrática para la predicción del
# cambio en todo el año a partir del cambio en enero?

b <- 0.596 * (15.35 / 5.36)

a <- 9.07 - (b * 1.75)

print(b)
print(a)

"
ŷ = 6.08305 + 1.706828 * x
"

# (c) En enero el cambio medio es x̄ = 1,75%. Utiliza tu recta de regresión para
# predecir el cambio del índice en un año para el cual en enero sube un 1,75%. ¿Por
# qué podías haber conocido este resultado (hasta donde te permite el error de re
# dondeo) sin necesidad de hacer ningún cálculo?

y_sombrero <- a + b * 1.75

print(y_sombrero)

"
ŷ = 9.07

Por que se supone que el comportamiento de Enero predice el comportamiento de todo el año, 
por lo tanto la regresión minimo cuadratica es el promedio de la variación del indice 
a lo largo de todo el año"

# **************************************************
# PREGUNTA 2.35 - Castores y larvas de coleóptero
# **************************************************

# Un estudio parece mostrar que los castores pueden ser beneficiosos para una 
# determinada especie de coleóptero. Los investigadores establecieron 23 parcelas 
# circulares, cada una de ellas de 4 metros de diámetro, en una zona en la que los 
# castores provocaban la caída de álamos al alimentarse de su corteza. En cada parcela, 
# los investigadores determinaron el número de tocones resultantes de los árboles 
# derribados por los castores y el número de larvas del coleóptero.

# Datos:
# Tocones: 2 2 1 3 3 4 3 1 2 5 1 3 2 1 2 2 1 1 4 1 2 1 4
# Larvas:  10 30 12 24 36 40 43 11 27 56 18 40 25 8 21 14 16 6 54 9 13 14 50

# (a) Haz un diagrama de dispersión que muestre cómo el número de toco
# nes debidos a los castores influye sobre el de larvas. ¿Qué muestra tu diagrama?
# (Los ecólogos creen que los brotes que surgen de los tocones resultan más apete
# cibles para las larvas ya que son más tiernos que los de los árboles mayores.)

resultados_parcelas <- data.frame(
  Tocones = c(2,2,1,3,3,4,3,1,2,5,1,3,2,1,2,2,1,1,4,1,2,1,4),
  Larvas = c(10,30,12,24,36,40,43,11,27,56,18,40,25,8,21,14,16,6,54,9,13,14,50)
)

print(resultados_parcelas)

plot(resultados_parcelas$Tocones,
     resultados_parcelas$Larvas,
     main = "Castores y larvas de coleóptero",
     xlab = "Tocones",
     ylab = "Larvas",
     col = "blue",
     pch = 15)
grid()

"El diagrama muestra una relación lineal positiva entre número de tocones y número de larvas, 
aunque con dispersión y con más observaciones en valores bajos de tocones (1-3), que es lo típico 
en distribuciones naturales.

Por lo tanto, el diagrama de dispersión apoya la hipótesis de los ecólogos, mostrando una clara 
tendencia creciente entre el número de tocones y la abundancia de larvas.
"

# (b) Halla la recta de regresión mínimo-cuadrática y dibújala en tu diagrama.

media_tocones <- mean(resultados_parcelas$Tocones)
media_larvas <- mean(resultados_parcelas$Larvas)

desv_tocones <- sd(resultados_parcelas$Tocones)
desv_larvas <- sd(resultados_parcelas$Larvas)

z_tocones <- (resultados_parcelas$Tocones - media_tocones) / desv_tocones
z_larvas <- (resultados_parcelas$Larvas - media_larvas) / desv_larvas

n_muestras <- nrow(resultados_parcelas)

r <- (1 / (n_muestras - 1)) * sum(z_tocones * z_larvas)

b <- r * (desv_larvas / desv_tocones)

a <- media_larvas - (b * media_tocones)

resultados <- data.frame(
  Variable = c("Tocones","Larvas"),
  Media = c(media_tocones,media_larvas),
  Desviacion = c(desv_tocones,desv_larvas)
)  

resultados_mininmo_cuadratica = data.frame(
  Medida = c("Correlación (r)", "Pendiente (b)","Intercepto (a)"),
  Valores = c(r,b,a)
)

print(resultados)
print(resultados_mininmo_cuadratica)

plot(resultados_parcelas$Tocones,
     resultados_parcelas$Larvas,
     main = "Castores y larvas de coleóptero",
     xlab = "Tocones",
     ylab = "Larvas",
     col = "blue",
     pch = 15)
abline(a = a, b = b, col = "red", lwd = 2)
grid()


"
Variable     Media  Desviacion
Tocones   2.217391    1.204406
Larvas   25.086957   15.637696

=================================

          Medida      Valores
 Correlación (r)   0.91604789
   Pendiente (b)  11.8937330
  Intercepto (a)  -1.2861035
"

# (c) ¿Qué porcentaje de la variación observada en el número de larvas se puede
# explicar por la dependencia lineal con el número de tocones?

r2 = r * r

print(paste0(round(r2, 2)*100, "%"))

" Hay un 84% de la variación observada en el número de larvas se explica por la 
relación lineal con el número de tocones."