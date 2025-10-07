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

# **************************************************
# PREGUNTA 2.36 - Consumo de gasolina y velocidad
# **************************************************

# El ejercicio 2.6 proporciona datos sobre el consumo de gasolina y de un automóvil 
# a distintas velocidades x. El consumo de carburante se ha medido en litros de 
# gasolina por 100 kilómetros y la velocidad en kilómetros por hora. 
# Con la ayuda de un programa estadístico hemos obtenido la recta de regresión 
# mínimo-cuadrática y también los residuos.

# La recta de regresión es:
# ŷ = 11,058 − 0,01466x

# Los residuos, en el mismo orden que las observaciones, son:
# 10,09  2,24  −0,62  −2,47  −3,33  −4,28  −3,73  −2,94
# −2,17  −1,32  −0,42  0,57  1,64  2,76  3,97

# (a) Dibuja un diagrama de dispersión con las observaciones y traza la recta
# de regresión en tu diagrama.

ejercicio_2_36 <- read.csv("ejercicio_2_06_consumo_coche.csv")

residuos <- c(10.09,2.24,-0.62, -2.47,-3.33,-4.28,-3.73,-2.94,-2.17,-1.32,-0.42,0.57,1.64,2.76,3.97)

a = 11.058

b = -0.01466

y_pred = a + b *ejercicio_2_36$Velocidad_km_h

plot(ejercicio_2_36$Velocidad_km_h,
     ejercicio_2_36$Consumo_litros_100km,
     main = "Consumo de vehiculo por velocidad",
     xlab = "Velocidad (x 100 km/h)",
     ylab = "Consumo (litros x 100 km)",
     pch = 16,
     col = "blue")
abline(a = a, b = b,col ="red",lwd = 2)
segments(ejercicio_2_36$Velocidad_km_h, 
         ejercicio_2_36$Consumo_litros_100km,
         ejercicio_2_36$Velocidad_km_h, 
         y_pred,
         col = "green", lwd = 1)
grid()

# (b) ¿Utilizarías la recta de regresión para predecir y a partir de x? Justifica tu
# respuesta.

"No utilizaría esta recta de regresión para predecir el consumo a partir de la velocidad 
porque los residuos muestran un patrón sistemático claro: comienzan siendo positivos grandes, 
luego se vuelven negativos en el rango medio de velocidades, y finalmente vuelven a ser 
positivos al final. Este comportamiento curvilíneo indica que la relación real entre 
velocidad y consumo no es lineal, sino que probablemente sigue una forma cuadrática o curvilínea. 
Usar un modelo lineal simple produciría predicciones sesgadas, especialmente a velocidades bajas 
y altas, donde los residuos son más grandes. Sería más apropiado ajustar un modelo no lineal que 
capture mejor la verdadera relación entre estas variables."

# (c) Comprueba que la suma de los residuos es 0 (o muy cercana a 0, teniendo
# en cuenta los errores de redondeo).

suma_residuos <- sum(residuos)

print(suma_residuos)

" 
residuos = -0.01
"

# (d) Dibuja un diagrama de residuos con relación a los valores de x. Traza una
# recta horizontal a la altura del valor 0 del eje de las ordenadas. Comprueba que
# la distribución de los residuos a lo largo de esta recta es similar a la distribución
# de los puntos a lo largo de la recta de regresión del diagrama de dispersión en (a).

plot(ejercicio_2_36$Velocidad_km_h,
     residuos,
     main = "Valores residuales",
     xlab = "Velocidad (x 100 km/h",
     ylab = "Residuos",
     col = "blue",
     pch = 16)
abline(h = 0, col = "red", lwd = 2)
grid()

"Se ve el mismo patrón curvilíneo que  en el diagrama de dispersión original, 
pero ahora centrado alrededor de y = 0, confirmando que la relación no es lineal 
sino curvilínea."

# **************************************************
# PREGUNTA 2.37 - ¿Cuántas calorías?
# **************************************************

# La tabla 2.5 proporciona datos sobre el contenido real en calorías de diez alimentos 
# y la media de los contenidos estimados por un numeroso grupo de personas. 
# El ejercicio 2.23 explora la influencia de dos observaciones atípicas sobre la correlación.

# Datos de la tabla 2.5:
# Alimento                                 Calorías estimadas  Calorías reales
# ---------------------------------------------------------------------------
# 225 g de leche entera                    196                159
# 142 g de espaguetis con salsa de tomate  394                163
# 142 g de macarrones con queso            350                269
# Una rebanada de pan de trigo             117                61
# Una rebanada de pan blanco               136                76
# 57 g de caramelos                        364                260
# Una galleta salada                       74                 12
# Una manzana de tamaño medio              107                80
# Una patata de tamaño medio               160                88
# Una porción de pastel de crema           419                160

# (a) Dibuja un diagrama de dispersión adecuado para predecir la estimación
# de las calorías a partir de los valores reales. Señala los puntos correspondientes a
# los espaguetis y a los pasteles en tu diagrama. Estos dos puntos quedan fuera de
# la relación lineal de los ocho puntos restantes.

ejercicio_2_37 <- read.csv("ejercicio_2_23_calorias.csv")

puntos_fuera <- ejercicio_2_37$Alimento %in% 
  c("142 g de espaguetis con salsa de tomate","Una porción de pastel de crema")


plot(ejercicio_2_37$Calorias_reales,
     ejercicio_2_37$Calorias_estimadas,
     main = "Estimacion calorica en alimentos",
     xlab = "Calorias reales",
     ylab = "Calorias estimadas",
     col = "blue",
     pch = 16)
text(ejercicio_2_37$Calorias_reales,
     ejercicio_2_37$Calorias_estimadas,
     labels = ejercicio_2_37$Alimento,
     pos = 3, cex = 0.7, col = "darkblue")
points(ejercicio_2_37$Calorias_reales[puntos_fuera],
       ejercicio_2_37$Calorias_estimadas[puntos_fuera],
       col = "red", pch = 17, cex = 1.5)
grid()

# (b) Utiliza tu calculadora para hallar la recta de regresión de las calorías esti
# madas con relación a las calorías reales. Hazlo dos veces, primero, con todos los
# puntos y luego, dejando fuera los espaguetis y los pasteles.

calorias_filtradas <- ejercicio_2_37[!(ejercicio_2_37$Alimento %in%
                                         c("142 g de espaguetis con salsa de tomate",
                                           "Una porción de pastel de crema")), ]

modelo_todas_observaciones <- lm(Calorias_estimadas ~ Calorias_reales, data = ejercicio_2_37)

summary(modelo_todas_observaciones)
modelo_filtrado_observaciones <- lm(Calorias_estimadas ~ Calorias_reales, data = calorias_filtradas)

comparativa_valores <- data.frame(
  Dato = c("Todas las observaciones", "Sin observaciones atípicas"),
  Intercepto = c(coef(modelo_todas_observaciones)[1],coef(modelo_filtrado_observaciones)[1]),
  Pendiente = c(coef(modelo_todas_observaciones)[2],coef(modelo_filtrado_observaciones)[2])
)

print(comparativa_valores)

"
 Dato                       Intercepto Pendiente
 Todas las observaciones      58.58785  1.303555
 Sin observaciones atípicas   43.88138  1.147213
"

# (c) Dibuja las dos rectas de regresión en tu diagrama (una de trazo continuo
# y la otra con trazo discontinuo). Los espaguetis y los pasteles, tomados conjunta
# mente, ¿son observaciones influyentes? Justifica tu respuesta.

plot(ejercicio_2_37$Calorias_reales,
     ejercicio_2_37$Calorias_estimadas,
     main = "Estimacion calorica en alimentos",
     xlab = "Calorias reales",
     ylab = "Calorias estimadas",
     col = "blue",
     pch = 16)
text(ejercicio_2_37$Calorias_reales,
     ejercicio_2_37$Calorias_estimadas,
     labels = ejercicio_2_37$Alimento,
     pos = 3, cex = 0.7, col = "darkblue")
points(ejercicio_2_37$Calorias_reales[puntos_fuera],
       ejercicio_2_37$Calorias_estimadas[puntos_fuera],
       col = "red", pch = 17, cex = 1.5)
abline(a = 58.58785,b = 1.303555,col ="black",lwd = 2)
abline(a = 43.88138,b = 1.147213,col = "darkgreen",lwd = 2,lty = 4 )
grid()

"Sí, los espaguetis y pasteles son observaciones influyentes porque al eliminarlos, 
la recta de regresión cambia significativamente tanto en pendiente como en intercepto."

# **************************************************
# PREGUNTA 2.38 - ¿Influyentes o no?
# **************************************************

# Hemos visto que el niño 18 de los datos Gesell de la tabla 2.7 es una observación 
# influyente. Ahora vamos a examinar el efecto del niño 19, que también es una 
# observación atípica en la figura 2.14.

# (a) Halla la recta de regresión mínimo-cuadrática de la puntuación en la prueba 
# Gesell respecto a la edad a la cual un niño empieza a hablar, dejando fuera al
# niño 19. El ejemplo 2.12 da la recta de regresión con todos los niños. Dibuja ambas
# rectas en el mismo gráfico (no es necesario que lo hagas sobre un diagrama de dis
# persión; tan sólo dibuja las rectas). ¿Calificarías al niño 19 como muy influyente?
# ¿Por qué?

ejercicio_2_38 <- read.csv("ejercicio_2_38_GESELL.csv")

str(ejercicio_2_38)

View(ejercicio_2_38)

modelo_todos_los_ninos <- lm(Puntuacion ~ Edad,data = ejercicio_2_38)

modelo_sin_nino_19 <- lm(Puntuacion ~ Edad,data = ejercicio_2_38[ejercicio_2_38$Nino != 19, ])

comparativa <- data.frame(
  Modelo = c("Todos los niños", "Sin niño 19"),
  Intercepto = c(coef(modelo_todos_los_ninos)[1],coef(modelo_sin_nino_19)[1]),
  Pendiente = c(coef(modelo_todos_los_ninos)[2],coef(modelo_sin_nino_19)[2])
)

print(comparativa)

punto_nino_19 <-  ejercicio_2_38$Nino == 19

plot(ejercicio_2_38$Edad,
     ejercicio_2_38$Puntuacion,
     main = "Comparativa desempeño prueba Gesell",
     xlab = "Edad (en meses)",
     ylab = "Puntuación",
     col = "blue",
     pch = 16)
points(ejercicio_2_38$Edad[punto_nino_19],
       ejercicio_2_38$Puntuacion[punto_nino_19],
       col = "red",
       pch = 15)
abline(a = 109.8738, b = -1.126989, col = "black", lwd = 2)
abline(a = 109.3047, b = -1.193311, col = "darkgreen", lwd = 2, lty = 4)
grid()

"
Modelo            Intercepto  Pendiente
Todos los niños   109.8738   -1.126989
Sin niño 19       109.3047   -1.193311

No, ya que los cambios observados son menores y no alteran sustancialmente la interpretación 
del modelo.
"
# (b) La exclusión del niño 19, ¿qué efecto tiene sobre el valor r² de esta regre
# sión? Explica por qué cambia r² al excluir al niño 19.

cor_todos <- cor(ejercicio_2_38$Edad, ejercicio_2_38$Puntuacion)
cor_sin_19 <- cor(ejercicio_2_38$Edad[!ejercicio_2_38$Nino == 19],ejercicio_2_38$Puntuacion[!ejercicio_2_38$Nino == 19])

r2_todos <- cor_todos * cor_todos

r2_sin_19 <- cor_sin_19 * cor_sin_19

resultados <- data.frame(
  Modelo = c("Todos los niños", "Sin niño 19"),
  r2 = c(r2_todos,r2_sin_19)
)

print(resultados)

"
Modelo           r2
Todos los niños  0.4099713
Sin niño 19      0.5716310

El niño 19 es un punto atípico que no sigue bien el patrón lineal general. Al estar más 
alejado de la recta de regresión, aumenta la suma de cuadrados de los residuos (SSE), lo que reduce 
R2.
"

# **************************************************
# PREGUNTA 2.39 - Repaso sobre relación lineal (Ahorros de Antonio)
# **************************************************

# Antonio guarda sus ahorros en un colchón. Empezó con 500 € que le dio su madre 
# y cada año fue añadiendo 100 €. Sus ahorros totales después de x años vienen 
# dados por la ecuación: y = 500 + 100x

# (a) Representa gráficamente esta ecuación. (Escoge dos valores de x, tales como
# 0 y 10. Calcula los valores correspondientes de y a partir de la ecuación. Dibuja
# estos dos puntos en el gráfico y dibuja la recta uniéndolos.)

intercepto <- 500
pendiente <- 100
annio <- 0:10

ahorros_Antonio <- intercepto + pendiente * annio

tercer_ahorro <- intercepto + pendiente * 3
octavo_ahorro <- intercepto + pendiente * 8

plot(annio,
     ahorros_Antonio,
     main = "Ahorros de Antonio",
     xlab = "Años",
     ylab = "Ahorros (en Euros)",
     pch = 16,
     col = "blue")
points(3,tercer_ahorro,pch = 15,col = "red")
points(8,octavo_ahorro,pch = 15,col = "darkgreen")
abline(a = intercepto, b = pendiente, col = "black",lwd = 2,lty= 4)
grid()

# (b) Después de 20 años, ¿cuánto tendrá Antonio en su colchón?

vigesimo_ahorro <- intercepto + pendiente * 20

print(vigesimo_ahorro)

"Antonio tendrá 2500€ después de 20 años"

# (c) Si Antonio hubiera añadido cada año 200 € a sus 500 € iniciales, en vez
# de 100, ¿cuál sería la ecuación que describiría sus ahorros después de x años?

nueva_pendiente <- 200

tercer_ahorro_nuevo <- intercepto + nueva_pendiente * 3
octavo_ahorro_nuevo <- intercepto + nueva_pendiente * 8
vigesimo_ahorro_nuevo <- intercepto + nueva_pendiente * 20

comparativa <- data.frame(
  Annio_de_Ahorro = c("Tercer año","Octavo año","Vigésimo año"),
  Ahorro_100_euros = c(tercer_ahorro,octavo_ahorro,vigesimo_ahorro),
  Ahorro_200_euros = c(tercer_ahorro_nuevo,octavo_ahorro_nuevo,vigesimo_ahorro_nuevo)
)

print(comparativa)

"
  Annio_de_Ahorro Ahorro_100_euros Ahorro_200_euros
1      Tercer año              800             1100
2      Octavo año             1300             2100
3    Vigésimo año             2500             4500
"

# **************************************************
# PREGUNTA 2.40 - Repaso sobre relación lineal (Crecimiento de rata)
# **************************************************

# En el periodo posterior a su nacimiento, una rata blanca macho gana exactamente 
# 40 gramos (g) por semana.

# (a) Si la rata pesaba 100 gramos al nacer, da una ecuación para predecir su
# peso después de x semanas. ¿Cuál es la pendiente de esta recta?
 
intercepto <- 100
pendiente <- 40
semana <- 0:5

y_sombrero <- intercepto + pendiente * semana

resultados <- data.frame(semana,y_sombrero)

print(resultados)

"
semana  y_sombrero
    0        100
    1        140
    2        180
    3        220
    4        260
    5        300
"

# (b) Dibuja un gráfico de esta recta para valores de x entre el nacimiento y las
# 10 semanas de edad.

semana <- 0:10

y_sombrero <- intercepto + pendiente * semana

plot(semana,
     y_sombrero,
     main = "Crecimiento de rata blanca macho",
     xlab = "Semanas",
     ylab = "Peso (en gramos)",
     col = "blue",
     pch = 16)
grid()

# (c) ¿Utilizarías esta recta para predecir el peso de la rata a los 2 años? Haz la
# predicción y medita sobre si el resultado es razonable.

semana_104 <- intercepto + pendiente * 104

print(semana_104)

"En la semana 104 pesarian 4.26 kilos. Esta predicción no es realista porque el modelo lineal 
solo describe el crecimiento en las primeras semanas de vida; en la realidad, las ratas dejan 
de crecer indefinidamente y alcanzan un peso máximo mucho menor. Por eso, aunque la cuenta es 
correcta, no sería razonable usar esta recta para estimar el peso en la adultez."

# **************************************************
# PREGUNTA 2.41 - Coeficiente de inteligencia y notas escolares
# **************************************************

# La figura 2.5 muestra las notas escolares medias y los coeficientes de inteligencia 
# de 78 estudiantes de primero de bachillerato.

# Estadísticas:
# Coeficientes de inteligencia: x̄ = 108,9, sx = 13,17
# Notas medias escolares: ȳ = 7,447, sy = 2,10
# Correlación: r = 0,6337

# (a) Halla la ecuación de la recta de regresión mínimo-cuadrática que permita
# predecir las notas escolares a partir de los coeficientes de inteligencia.

b <- 0.6337 * (2.10 / 13.17)

a <- 7.447 - (b * 108.9)

valores_recta_minimo_cuadratica <- data.frame(
  Medidas = c("Correlacion (c)","Intercepto (a)","Pendiente (b)"),
  Valores = c(0.6337,a,b)
)

print(valores_recta_minimo_cuadratica)

"
          Medidas    Valores
1 Correlacion (c)  0.6337000
2  Intercepto (a) -3.5568613
3   Pendiente (b)  0.1010456
"

# (b) ¿Qué porcentaje de la variación observada en las notas escolares se puede 
# explicar por la relación lineal entre las notas escolares y los coeficientes de
# inteligencia?

r <- 0.6337 

r2 <- r * r

print(paste0(round(r2, 2)*100, "%"))

"Hay un 40% de variación observada en las notas que se puede explicar entre las notas
escolares y los coeficientes de inteligencia"

# (c) Un estudiante tiene un coeficiente de inteligencia de 103 y una nota media
# escolar de sólo 0,53. ¿Cuál es la predicción de la nota media escolar de un estu
# diante con un coeficiente de inteligencia de 103? ¿Cuál es el valor residual de este
# estudiante?

coef_estudiante <- 103

nota_estudiante <- 0.53

recta_estudiante <- a + b * coef_estudiante

residual_estudiante <- nota_estudiante - recta_estudiante

valores_estudiante <- data.frame(
  Medida = c("Coeficiente","Nota media","Nota predicha","Residuo"),
  Valores = c(103,0.53,recta_estudiante,residual_estudiante)
)

print(valores_estudiante)

"
         Medida    Valores
1   Coeficiente 103.000000
2    Nota media   0.530000
3 Nota predicha   6.850831
4       Residuo  -6.320831
"

# **************************************************
# PREGUNTA 2.42 - Llévame a ver un partido de baloncesto 
# **************************************************

# ¿Qué relación existe entre el precio de los bocadillos de salchicha y el de los 
# refrescos de cola en los estadios de baloncesto de EE UU?

# TABLA DE PRECIOS EN ESTADIOS DE BALONCESTO:

# Estadio        Bocadillo  Refrescos  Estadio      Bocadillo  Refrescos  Estadio      Bocadillo  Refrescos
#                                                                                     de cola
# --------------------------------------------------------------------------------------------------------
# Angels         2,50       1,75       Giants       2,75       2,17       Rangers      2,00       2,00
# Astros         2,00       2,00       Indians      2,00       2,00       RedSox       2,25       2,25
# Braves         2,50       1,79       Marlins      2,25       1,80       Rockies      2,25       2,29
# Brewers        2,00       2,00       Mets         2,50       2,50       Royals       1,75       1,99
# Cardinals      3,50       2,00       Padres       1,75       2,25       Tigers       2,00       2,00
# Dodgers        2,75       2,00       Phillies     2,75       2,20       Twins        2,50       1,75
# Expos          1,75       2,00       Pirates      1,75       1,75       White Sox    2,00       2,22

# (a) Dibuja un diagrama de dispersión que sea adecuado para predecir el precio 
# del refresco de cola a partir del precio del bocadillo. Describe la relación que
# observas. ¿Hay observaciones atípicas?

ejercicio_2_42 <- read.csv("ejercicio_2_42_estadios.csv")

str(ejercicio_2_42)

View(ejercicio_2_42)

plot(ejercicio_2_42$Bocadillo,
     ejercicio_2_42$Refrescos,
     main = "Prediccion precios de refresco por bocadillos",
     xlab = "Bocadillos (en US$)",
     ylab = "Refrescos (en US$)",
     pch = 16,
     col = "blue")
text(ejercicio_2_42$Bocadillo,
     ejercicio_2_42$Refrescos,
     labels = ejercicio_2_42$Estadio,
     pos = 3, cex = 0.7, col = "darkblue")
grid()

"Tenemos dos estadios: Cardinals con bocadillos a US$3.5 y Mets con resfrescos a US$2.5"

# (b) Halla la correlación entre el precio de los bocadillos y el precio de los
#     refrescos de cola. ¿Qué porcentaje de la variación del precio del refresco se explica
#     a partir de la relación lineal?

r <- cor(ejercicio_2_42$Bocadillo,ejercicio_2_42$Refrescos)

r2 <- r * r

porcentaje <- round(r2 * 100,1)

print(paste0(porcentaje, "%"))

"Solo el 0.2% de la variación en el precio de los refrescos se explica por la relación lineal 
con el precio de los bocadillos. Esto indica que prácticamente no hay relación lineal entre estos 
precios en los estadios."

# (c) Halla la ecuación de la recta de regresión mínimo-cuadrática para predecir 
# el precio del refresco a partir del precio del bocadillo. Dibuja la recta en tu
# diagrama de dispersión. A partir de tus resultados en (b), explica por qué no es
# sorprendente que la recta sea casi horizontal (pendiente próxima a cero).

media_bocadillos <- mean(ejercicio_2_42$Bocadillo)
media_refrescos <- mean(ejercicio_2_42$Refrescos)

desv_bocadillos <- sd(ejercicio_2_42$Bocadillo)
desv_refrescos <- sd(ejercicio_2_42$Refrescos)

z_bocadillos <- (ejercicio_2_42$Bocadillo - media_bocadillos) / desv_bocadillos
z_refrescos <- (ejercicio_2_42$Refrescos - media_refrescos) / desv_refrescos

n_muestras <- nrow(ejercicio_2_42)

r <- (1 / (n_muestras - 1)) * sum(z_bocadillos * z_refrescos)

b <- r *(desv_refrescos / desv_bocadillos)

a <- media_refrescos - (b * media_bocadillos)

resultados <- data.frame(
  Medidas = c("Correlación (r)","Intercepto (a)","Pendiente (b)"),
  Valores = c(r,a,b)
)

print(resultados)

"
         Medidas    Valores
1 Correlación (r) 0.04112220
2 Intercepto (a) 1.99124054
3  Pendiente (b) 0.01881997

La pendiente es casi cero (0.0188) porque la correlación entre las variables es 
prácticamente nula (r = 0.041). Cuando no hay relación lineal entre dos variables, 
la recta de regresión se vuelve casi horizontal, indicando que el precio de los bocadillos 
no ayuda a predecir el precio de los refrescos.
"
# (d) Señala la observación que potencialmente es más influyente. ¿A qué estadio corresponde? 
# Halla la recta de regresión mínimo-cuadrática sin esta observación y dibújala en tu diagrama 
# de dispersión. Esta observación, ¿es realmente una observación influyente?

precio_maximo <- max(ejercicio_2_42$Bocadillo)

estadio_caro <- ejercicio_2_42$Estadio[ejercicio_2_42$Bocadillo == precio_maximo]

datos_sin_caro <- ejercicio_2_42[ejercicio_2_42$Bocadillo != precio_maximo, ]

r_nuevo <- cor(datos_sin_caro$Bocadillo, datos_sin_caro$Refrescos)
b_nuevo <- r_nuevo * (sd(datos_sin_caro$Refrescos) / sd(datos_sin_caro$Bocadillo))
a_nuevo <- mean(datos_sin_caro$Refrescos) - (b_nuevo * mean(datos_sin_caro$Bocadillo))

comparacion <- data.frame(
  Modelo = c("Con todos", "Sin estadio caro"),
  Intercepto = c(round(a, 4), round(a_nuevo, 4)),
  Pendiente = c(round(b, 4), round(b_nuevo, 4)),
  Cambio = c("--", round(abs(b_nuevo - b), 4))
)

print(comparacion)

"
Estadio mas caro es el Cardinals

==================================================

            Modelo Intercepto Pendiente Cambio
1        Con todos     1.9912    0.0188     --
2 Sin estadio caro     1.9238    0.0508 0.0319
"

# **************************************************
# PREGUNTA 2.43 - Análisis de agua (Nitratos y absorbancia)
# **************************************************

# Las empresas suministradoras de agua la analizan regularmente para detectar la 
# posible presencia de contaminantes. La determinaciónde éstos se hace de forma 
# indirecta, por ejemplo, por colorimetría, que consiste en añadir un reactivo que 
# da color al reaccionar con el contaminante a determinar. Posteriormente se hace 
# pasar un haz de luz por la solución coloreada y se determina su “absorbancia”. 
# Para calibrar este método de análisis, los laboratorios disponen de patrones con 
# concentraciones conocidas del producto a determinar. Suele existir una relación 
# lineal entre la concentración del producto a determinar y su absorbancia una vez 
# ha tenido lugar la reacción anteriormente comentada. He aquí unaserie de datos 
# sobre la absorbancia y la concentración de nitratos. Los nitratos se expresan en 
# miligramos por litro de agua.

# Datos sobre la absorbancia y la concentración de nitratos (mg/L):
# Nitratos: 50 50 100 200 400 800 1200 1600 2000 2000
# Absorbancia: 7,0 7,5 12,8 24,0 47,0 93,0 138,0 183,0 230,0 226,0

# (a) Teóricamente estos datos deben mantener una relación lineal. Si el coeficiente 
# de correlación no es al menos 0,997, hay que suponer que algo fue mal y hay que repetir 
# el proceso de calibración. Representa gráficamente los datos y halla su correlación. 
# ¿Se debe repetir la calibración?

analisis_agua <- data.frame(
  Nitratos = c(50,50,100,200,400,800,1200,1600,2000,2000),
  Absorbancia = c(7.0,7.5,12.8,24.0,47.0,93.0,138.0,183.0,230.0,226.0)
)

print(analisis_agua)

r = cor(analisis_agua$Nitratos,analisis_agua$Absorbancia)

print(r)

plot(analisis_agua$Nitratos,
     analisis_agua$Absorbancia,
     main = "Análisis de agua",
     xlab = "Nitratos (mg/l)",
     ylab = "Absorbancia",
     pch = 16,
     col = "blue")
grid()

"
r = 0.9999392

No se debe repetir la calibración"

# (b) Determina la ecuación de la recta de regresión mínimo-cuadrática que nos
# permita predecir la absorbancia a partir de la concentración de nitratos. Si el la
# boratorio analiza un patrón con 500 miligramos de nitratos por litro, ¿qué valor
# de absorbancia obtendrás? Basándote en tu dibujo y en la correlación, ¿crees que
# la estimación de la absorbancia será muy exacta?

media_nitratos <- mean(analisis_agua$Nitratos)
media_absorbancia <- mean(analisis_agua$Absorbancia)

desv_nitratos <- sd(analisis_agua$Nitratos)
desv_absorbancia <- sd(analisis_agua$Absorbancia)

z_nitratos <- (analisis_agua$Nitratos - media_nitratos) / desv_nitratos
z_absorbancia <- (analisis_agua$Absorbancia - media_absorbancia) / desv_absorbancia

n_muestras <- nrow(analisis_agua)

b <- r * (desv_absorbancia / desv_nitratos)

a <- media_absorbancia - (b * media_nitratos)

valor_500 <- a + b * 500

print(valor_500)

resultados_agua <- data.frame(
  Medidas = c("Correlación (r)", "Intercepto (a)", "Pendiente (b)"),
  Valores = c(r,a,b)
)

print(resultados_agua)

"
          Medidas   Valores
1 Correlación (r) 0.9999392
2  Intercepto (a) 1.6570874
3   Pendiente (b) 0.1133011

=============================================

En 500 mg/l tendrá una absorbancia de 58.30763, basándome en lo requerimientos de la
consigna se podría decir que la estimacion es casí exacta.
"

# **************************************************
# PREGUNTA 2.44 - Crecimiento de una niña (Sara)
# **************************************************

# Los padres de Sara están preocupados porque creen que es baja para su edad.
# Edad (meses): 36 48 51 54 57 60
# Altura (cm): 86 90 91 93 94 95

# (a) Dibuja un diagrama de dispersión con estos datos. Fíjate en la fuerte relación lineal.

crecimiento_Sara <- data.frame(
  edad = c(36,48,51,54,57,60),
  altura = c(86,90,91,93,94,95)
)

print(crecimiento_Sara)

plot(crecimiento_Sara$edad,
     crecimiento_Sara$altura,
     main = "Crecimiento de Sara",
     xlab = "Edad (en meses)",
     ylab = "Altura (en centímetros)",
     pch = 18,
     col = "red")
grid()

# (b) Usando la calculadora, halla la ecuación de la recta de regresión mínimo
# cuadrática de la altura en relación con la edad.

regresion <- lm(altura ~ edad, data = crecimiento_Sara)

summary(regresion)

"
Residuals:
  1     2     3     4     5     6 
0.25 -0.35 -0.50  0.35  0.20  0.05 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 71.95000    1.05297   68.33 2.75e-07 ***
  edad         0.38333    0.02041   18.78 4.73e-05 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.3873 on 4 degrees of freedom
Multiple R-squared:  0.9888,	Adjusted R-squared:  0.986 
F-statistic: 352.7 on 1 and 4 DF,  p-value: 4.734e-05
"

# (c) Predice la altura de Sara a los 40 y a los 60 meses. Utiliza tus resultados
# para dibujar la recta de regresión en tu diagrama de dispersión.

a <- coef(regresion)[1]

b <- coef(regresion)[2]

altura_40_meses <- a + b * 40

altura_60_meses <- a + b * 60

resultados_sara <- data.frame(
  Meses = c("40 meses","60 meses"),
  Altura = c(altura_40_meses,altura_60_meses)
)
print(resultados_sara)

plot(crecimiento_Sara$edad,
     crecimiento_Sara$altura,
     main = "Crecimiento de Sara",
     xlab = "Edad (en meses)",
     ylab = "Altura (en centímetros)",
     pch = 18,
     col = "red")
points(40,altura_40_meses,col= "blue",pch = 16)
points(60,altura_60_meses,col = "blue", pch = 17)
abline(a = a, b = b, lwd = 2, lty = 2)
grid()

"
     Meses   Altura
1 40 meses 87.28333
2 60 meses 94.95000
"

# (d) ¿Cuál es el ritmo de crecimiento de Sara en centímetros por mes? Las niñas 
# con crecimiento normal ganan unos 6 cm de altura entre los 4 (48 meses) y los
# 5 años (60 meses). En este último caso, ¿qué valor toma el ritmo de crecimiento
# expresado en centímetros por mes? ¿Crece Sara más despacio de lo normal?

crecimiento_normal <- 6

ritmo_de_sara <- b

ritmo_de_crecimiento <- crecimiento_normal / 12

comparacion_crecimiento <- data.frame(
  Tipo = c("Sara", "Normal"),
  Ritmo_cm_mes = c(round(ritmo_de_sara,2),round(ritmo_de_crecimiento,2)),
  Diferencia = c("---",round(ritmo_de_sara - ritmo_de_crecimiento,2))
)

print(comparacion_crecimiento)

"
Tipo   Ritmo_cm_mes Diferencia
Sara           0.38        ---
Normal         0.50      -0.12
"

# **************************************************
# PREGUNTA 2.45 - Invertir en y fuera de EE UU
# **************************************************

# Unos inversores quieren saber qué relación existe entre los rendimientos de las 
# inversiones en EE UU y las inversiones fuera de EEUU. 
# La tabla 2.8 proporciona datos sobre los rendimientos totales de los valores 
# bursátiles en EE UU y fuera de EE UU, durante un periodo de 26 años.

# TABLA 2.8 - Rendimientos anuales en y fuera de EE UU (1971-1997):

# Año   Rendimiento_fuera_EEUU  Rendimiento_en_EEUU
# -------------------------------------------------
# 1971   29,6                   14,6
# 1972   36,3                   18,9
# 1973   -14,9                  -14,8
# 1974   -23,2                  -26,4
# 1975   35,4                   37,2
# 1976   2,5                    23,6
# 1977   18,1                   -7,4
# 1978   32,6                   6,4
# 1979   4,8                    18,2
# 1980   22,6                   32,3
# 1981   -2,3                   -5,0
# 1982   -1,9                   21,5
# 1983   23,7                   22,4
# 1984   7,4                    6,1
# 1985   56,2                   31,6
# 1986   69,4                   18,6
# 1987   24,6                   5,1
# 1988   28,5                   16,8
# 1989   10,6                   31,5
# 1990   -23,0                  -3,1
# 1991   12,8                   30,4
# 1992   -12,1                  7,6
# 1993   32,9                   10,1
# 1994   6,2                    1,3
# 1995   11,2                   37,6
# 1996   6,4                    23,0
# 1997   2,1                    33,4

# (a) Haz un diagrama de dispersión adecuado para predecir los rendimientos
# de los valores bursátiles fuera de EE UU a partir de los rendimientos en EE UU.

ejercicio_2_45 <- read.csv("ejercicio_2_45_rendimientos.csv")

str(ejercicio_2_45)

View(ejercicio_2_45)

plot(ejercicio_2_45$Rendimiento_en_EEUU,
     ejercicio_2_45$Rendimiento_fuera_EEUU,
     main = "Rendimientos bursatiles",
     xlab = "Rendimiento bursatil en EE.UU",
     ylab = "Rendimiento bursatil fuera de EE.UU",
     col = "blue",
     pch = 16)
grid()

# (b) Halla la correlación y r². Describe con palabras la relación entre los rendi
# mientos en y fuera de EE UU. Utiliza r y r² para hacer más precisa tu descripción.

r <- cor(ejercicio_2_45$Rendimiento_en_EEUU,ejercicio_2_45$Rendimiento_fuera_EEUU)

r2 <- r * r

resultados <- data.frame(
  Medidas = c("r", "r2"),
  Resultados = c(r, r2)
)

print(resultados)

"
  Medidas Resultados
1       r  0.4641252
2      r2  0.2154122

==============================

El coeficiente de correlación entre los rendimientos bursátiles en EE. UU. y fuera de EE. UU. 
es r = 0.46, lo que indica una relación positiva moderada. En general, cuando el mercado estadounidense 
presenta rendimientos altos, los mercados fuera de EE. UU. también tienden a tener buenos resultados, 
aunque la relación no es muy fuerte.

El coeficiente de determinación r² = 0.21 muestra que solo alrededor del 21 % de la variabilidad en 
los rendimientos fuera de EE. UU. puede explicarse por los rendimientos en EE. UU. Esto sugiere que, 
aunque existe cierta conexión entre ambos mercados, los rendimientos internacionales están influenciados 
principalmente por otros factores.

En conclusión, invertir fuera de EE. UU. ofrece una cierta diversificación, ya que los rendimientos no 
se mueven completamente al unísono con los del mercado estadounidense.
"

# (c) Halla la recta de regresión mínimo-cuadrática de los rendimientos fuera de
# EE UU en función de los rendimientos en EE UU. Traza la recta en el diagrama
# de dispersión.

modelo <- lm(Rendimiento_fuera_EEUU ~ Rendimiento_en_EEUU, data = ejercicio_2_45)

plot(ejercicio_2_45$Rendimiento_en_EEUU,
     ejercicio_2_45$Rendimiento_fuera_EEUU,
     main = "Rendimientos bursatiles",
     xlab = "Rendimiento bursatil en EE.UU",
     ylab = "Rendimiento bursatil fuera de EE.UU",
     col = "blue",
     pch = 16)
abline(modelo, col = "red", lwd = 2, lty = 2)
grid()

resultados <- data.frame(
  Medidas = c("Intercepto", "Pendiente"),
  Valores = c(coef(modelo)[1],coef(modelo)[2])
)

print(resultados)

"
Medidas     Valores
Intercepto  5.6940022
Pendiente   0.6200816
"
# (d) En 1997, el rendimiento de las acciones en EE UU fue del 33,4%. Utiliza la
# recta de regresión para predecir el rendimiento de las acciones fuera de EE UU. 
# El rendimiento fuera de EE UU fue del 2,1%. ¿Estás seguro de que las predicciones
# basadas en la recta de regresión serán suficientemente precisas? ¿Por qué?

rendimiento_1997 <- 5.6940022 + 0.6200816 * 33.4

print(rendimiento_1997)

"Como el coeficiente de correlación es r = 0.46, la relación entre los rendimientos en EE. UU. y 
fuera de EE. UU. es positiva pero moderada, lo que significa que los valores tienden a moverse en 
la misma dirección, aunque con bastante variabilidad. Dado que el coeficiente de determinación r² = 0.21 
indica que solo el 21 % de la variación fuera de EE. UU. se explica por lo que ocurre en EE. UU., 
las predicciones obtenidas con la recta de regresión no son muy fiables. En consecuencia, aunque el 
modelo muestra una tendencia general positiva, no permite predecir con precisión los rendimientos internacionales, 
como se observa en 1997, donde el valor real (2,1 %) fue muy diferente del estimado (≈ 26,4 %)."

# (e) Señala el punto que tenga el mayor residuo (positivo o negativo). ¿Qué
# año es? ¿Parece probable que existan puntos que sean observaciones muy influyentes?

residuos <- residuals(modelo)

indice <- which.max(abs(residuos))
año_max <- ejercicio_2_45$Año[indice]
residuo_max <- residuos[indice]

indice_extremo <- which.max(abs(ejercicio_2_45$Rendimiento_en_EEUU))
año_influyente <- ejercicio_2_45$Año[indice_extremo]

print(paste("Año con mayor residuo:", año_max))
print(paste("Residuo:", round(residuo_max, 2)))
print(paste("Posible año influyente (valor extremo en EEUU):", año_influyente))

plot(ejercicio_2_45$Rendimiento_en_EEUU,
     ejercicio_2_45$Rendimiento_fuera_EEUU,
     main = "Rendimientos bursátiles",
     xlab = "Rendimiento bursatil en EE.UU",
     ylab = "Rendimiento bursatil fuera de EE.UU",
     col = "blue",
     pch = 16)
abline(modelo, col = "red", lwd = 2, lty = 2)
points(ejercicio_2_45$Rendimiento_en_EEUU[indice],
       ejercicio_2_45$Rendimiento_fuera_EEUU[indice],
       col = "red", pch = 16, cex = 1.5)
text(ejercicio_2_45$Rendimiento_en_EEUU[indice],
     ejercicio_2_45$Rendimiento_fuera_EEUU[indice],
     labels = año_max, pos = 3, col = "red")
grid()

"
Año con mayor residuo: 1986
Residuo: 52.17
Posible año influyente (valor extremo en EEUU): 1995
"

# **************************************************
# PREGUNTA 2.46 - Cuatro conjuntos de datos de Anscombe
# **************************************************

# La tabla 2.9 presenta cuatro conjuntos de datos preparados por el estadístico 
# Frank Anscombe para ilustrar los peligros de hacer cálculos sin antes representar 
# los datos.

# CONJUNTO DE DATOS A:
x_A <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y_A <- c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68)

# CONJUNTO DE DATOS B:
x_B <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y_B <- c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74)

# CONJUNTO DE DATOS C:
x_C <- c(10, 8, 13, 9, 11, 14, 6, 4, 12, 7, 5)
y_C <- c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.42, 5.73)

# CONJUNTO DE DATOS D:
x_D <- c(8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 19)
y_D <- c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 5.56, 7.91, 6.89, 12.50)

# (a) Sin dibujar un diagrama de dispersión, halla la correlación y la recta de
# regresión mínimo-cuadrática para los cuatro grupos de datos. ¿Qué observas?
# Utiliza la recta de regresión para predecir y cuando x = 10.

r_A <- cor(x_A,y_A)
r_B <- cor(x_B,y_B)
r_C <- cor(x_C,y_C)
r_D <- cor(x_D,y_D)

r2_A <- r_A * r_A
r2_B <- r_B * r_B
r2_C <- r_C * r_C
R2_D <- r_D * r_D

modelo_A <- lm(y_A ~ x_A)
recta_A <- coef(modelo_A)[1] + coef(modelo_A)[2] * 10

modelo_B <- lm(y_B ~ x_B)
recta_B <- coef(modelo_B)[1] + coef(modelo_B)[2] * 10

modelo_C <- lm(y_C ~ x_C)
recta_C <- coef(modelo_C)[1] + coef(modelo_C)[2] * 10

modelo_D <- lm(y_D ~ x_D)
recta_D <- coef(modelo_D)[1] + coef(modelo_D)[2] * 10


resultados <- data.frame(
  Medida = c("r","r2"),
  Conjunto_A = c(r_A,r2_A),
  Conjunto_B = c(r_B,r2_B),
  Conjunto_C = c(r_C,r2_C),
  Conjunto_D = c(r_D,R2_D)
)

resultados_recta <- data.frame(
  Medidas = c("Intercepto","Pendiente"," x = 10"),
  Conjunto_A = c(coef(modelo_A)[1],coef(modelo_A)[2],recta_A),
  Conjunto_B = c(coef(modelo_B)[1],coef(modelo_B)[2],recta_B),
  Conjunto_C = c(coef(modelo_C)[1],coef(modelo_C)[2],recta_C),
  Conjunto_D = c(coef(modelo_D)[1],coef(modelo_D)[2],recta_D)
)

print(resultados)

print(resultados_recta)

"
  Medida Conjunto_A Conjunto_B Conjunto_C Conjunto_D
1      r  0.8164205  0.8162365  0.8162867  0.8165214
2     r2  0.6665425  0.6662420  0.6663240  0.6667073

==============================================================

     Medidas Conjunto_A Conjunto_B Conjunto_C Conjunto_D
1 Intercepto  3.0000909   3.000909  3.0024545  3.0017273
2  Pendiente  0.5000909   0.500000  0.4997273  0.4999091
3     x = 10  8.0010000   8.000909  7.9997273  8.0008182
"

# (b) Dibuja un diagrama de dispersión para cada uno de los conjuntos de datos
# con las rectas de regresión correspondientes.

par(mfrow = c(2,2))

par(mar = c(3, 3, 2, 1))

plot(x_A,y_A,main = "Conjunto de datos A",
     xlab = "x", ylab= "y", pch = 16, col = "blue")
abline(lm(y_A ~ x_A), col = "red")
grid()

plot(x_B, y_B, main = "Conjunto de Datos B", 
     xlab = "x", ylab = "y", pch = 19, col = "darkgreen")
abline(lm(y_B ~ x_B), col = "red")
grid()

plot(x_C, y_C, main = "Conjunto de Datos C", 
     xlab = "x", ylab = "y", pch = 19, col = "purple")
abline(lm(y_C ~ x_C), col = "red")  
grid()

plot(x_D, y_D, main = "Conjunto de Datos D", 
     xlab = "x", ylab = "y", pch = 19, col = "orange")
abline(lm(y_D ~ x_D), col = "red")  
grid()

par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)

# (c) ¿En cuál o cuáles de los cuatro casos utilizarías la recta de regresión para
# describir la dependencia de y en relación a x? Justifica tu respuesta en cada caso.

"En los conjuntos A y C pues en ambos la relación es lineal"

# **************************************************
# PREGUNTA 2.47 - ¿Cuál es mi nota?
# **************************************************

# En el curso de economía del profesor Marcet, la correlación entre la calificación 
# acumulada por los estudiantes antes de examinarse y la calificación del examen 
# final es r = 0,6. La media de las calificaciones acumuladas antes del examen final 
# es 280 y la desviación típica, 30, mientras que la media de las notas del examen 
# final es 75 y la desviación típica, 8.

# (a) ¿Cuál es la pendiente de la recta de regresión mínimo-cuadrática de la
# calificación del examen final con relación a la calificación acumulada antes del
# examen final de ese curso? ¿Cuál es la ordenada en el origen?


media_calificaciones_acumuladas <- 280
media_notas_examen <- 75


desv_calificaciones_acomumuladas <- 30
desv_notas_examen <- 8

r <- 0.6

b <- r * (desv_notas_examen / desv_calificaciones_acomumuladas)

a <- media_notas_examen - (b * media_calificaciones_acumuladas)

resultados <- data.frame(
  Medidas = c("Intercepto (a)","Pendiente (b)"),
  Valores = c(a,b)
)

print(resultados)

"
         Medidas Valores
1 Intercepto (a)   30.20
2  Pendiente (b)    0.16

====================================

La ordenada de origen corresponde a las calificaciones acumuladas por los estudiantes
antes de examinarse
"

# (b) Utiliza la recta de regresión para predecir la calificación del examen final
# de Julia (calificación acumulada = 300).


calificacion_acumulada_Julia <- 300

calificacion_predicha_Julia <- a + b * calificacion_acumulada_Julia

print(paste("Calificación predicha para Julia:",calificacion_predicha_Julia))

"Calificación predicha para Julia: 78.2"

# (c) Julia no cree que el método del profesor Marcet para predecir la calificación 
# de su examen sea muy bueno. Calcula r² para argumentar que la calificación
# real del examen final de Julia podía haber sido mucho más alta (o mucho más
# baja) que el valor predicho.

r2 <- r * r

print(paste0(round(r2,2)*100,"%"))

"
Solo un 36% de la variabilidad  en las notas del examen final se explica por las 
calificaciones acumuladas.
Por eso la nota real de Julia podría ser mucho más alta o más baja que los 78.2 
puntos predichos.
"

# **************************************************
# PREGUNTA 2.48 - Predicción sin sentido
# **************************************************

# Utiliza la regresión mínimo-cuadrática con los datos del ejercicio 2.44 para 
# predecir la altura de Sara a los 40 años (480 meses).
# La predicción es absurdamente grande. No es razonable utilizar datos con valores 
# entre 36 y 60 meses para predecir la altura a los 480 meses.

crecimiento_Sara <- data.frame(
  edad = c(36,48,51,54,57,60),
  altura = c(86,90,91,93,94,95)
)

regresion <- lm(altura ~ edad, data = crecimiento_Sara)

summary(regresion)

a <- coef(regresion)[1]

b <- coef(regresion)[2]

crecimiento_480_meses <- a + b * 480

print(paste("El crecimiento de Sara en 480 meses sería:", round(crecimiento_480_meses,2),"cms."))

"El crecimiento de Sara en 480 meses sería: 255.95 cms.

===================================================

Los modelos de regresión solo son confiables dentro o cerca del rango de datos usados para crearlos. 
Fuera de ese rango, las predicciones pierden sentido."

# **************************************************
# PREGUNTA 2.49 - Invertir en y fuera de EE UU (continuación)
# **************************************************

# El ejercicio 2.45 examinó la relación entre los rendimientos de los valores 
# bursátiles en EE UU y fuera de EE UU.

# (a) Halla los cinco números resumen de los rendimientos tanto en EE UU como fuera de ellos, 
# y dibuja los correspondientes diagramas de caja en un mismo gráfico para comparar las dos distribuciones.


# (b) Durante este periodo, ¿los rendimientos fueron mayores en EE UU o fuera? 
# Justifica tu respuesta.

# (c) En este periodo, ¿los rendimientos fueron más volátiles (más variables) en
# o fuera de EE UU? Razona tu respuesta.

# **************************************************
# PREGUNTA 2.50 - Asistencia a clase y calificaciones
# **************************************************

# Un estudio sobre la relación entre la asistencia a clase y las calificaciones
# de los estudiantes de primer curso de la Universidad Pompeu Fabra mostró que,
# en general, los alumnos que asisten a un mayor porcentaje de clases obtienen
# mejores calificaciones. Concretamente, la asistencia a clase explicaba el 16% de
# la variación en la media de las calificaciones obtenidas.

# (a) ¿Cuál es el valor de la correlación entre el porcentaje de asistencia a clase 
#     y la media de las calificaciones obtenidas?

# **************************************************
# PREGUNTA 2.51 - ¿Suspenderé el examen final?
# **************************************************

# El profesor Smith analizó las calificaciones de 346 estudiantes que se matricularon 
# en un curso de estadística durante un periodo de 10 años. La recta de regresión 
# mínimo-cuadrática para la predicción de la calificación del examen final a partir 
# de la calificación del examen parcial era ŷ = 46,6 + 0,41x.

# La calificación del examen parcial de María está 10 puntos por encima de
# la media de los estudiantes analizados.

# (a) ¿Cuál habría sido tu predicción sobre el número de puntos por encima de 
#     la media del examen final de María?

# **************************************************
# PREGUNTA 2.52 - Predicción del número de estudiantes matriculados
# **************************************************

# A la Facultad de Matemáticas de una gran universidad le gustaría utilizar el 
# número de estudiantes x recién llegados a la universidad, para predecir el número 
# de estudiantes y que se matriculará en el curso de Introducción al Análisis 
# Matemático del semestre de otoño.

# Años: 1991-1998
# Datos de x (estudiantes recién llegados) e y (matriculados en matemáticas)

# Estadísticas: r = 0,8333, recta de regresión: ŷ = 2.492,69 + 1,0663x
# Tabla de residuos para cada año

# (a) Dibuja un diagrama de dispersión con la recta de regresión. Ésta no da
#     una buena predicción. ¿Qué porcentaje de la variación en las matrículas para el
#     curso de matemáticas se explica a partir de la relación entre éstas y el recuento de
#     recién ingresados en la universidad?

# (b) Comprueba que los residuos suman cero (o aproximadamente cero si se
#     tiene en cuenta el error de redondeo).

# (c) Los diagramas de residuos son a menudo reveladores. Dibuja los residuos
#     con relación al año. Una de las facultades de la universidad ha cambiado recien
#     temente su programa docente. Ahora exige a sus estudiantes que tomen un curso
#     de matemáticas. ¿Cómo muestra el diagrama de residuos este cambio? ¿En qué
#     años tuvo lugar dicho cambio?

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::

