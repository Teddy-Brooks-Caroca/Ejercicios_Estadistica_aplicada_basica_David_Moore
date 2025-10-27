# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# REPASO CAPÍTULO II

# **************************************************
# PREGUNTA 2.89 - El vino, ¿es bueno para tu corazón?
# **************************************************

# La tabla 2.4 proporciona datos sobre el consumo de vino y muertes por 
# ataques al corazón en 19 países. Un diagrama de dispersión muestra una 
# relación relativamente fuerte.

# (a) La correlación para estas variables es r = -0,843. ¿Por qué la correlación
# es negativa? ¿Qué porcentaje de la variación de la tasa de mortalidad por 
# ataques al corazón se puede explicar a partir de la relación lineal entre 
# los ataques al corazón y el consumo de vino?

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

ejercicio_2_89 <- read.csv("ejercicio_2_11_alcohol.csv")

View(ejercicio_2_89)

r <- -0.843

r2 <- r^2

print(r2)

"La correlación es negativa (-0.843) porque a mayor consumo de vino, menor tasa de mortalidad por ataques al corazón, 
sugiriendo una relación inversa. El r² = 71% indica que el 71% de la variación en mortalidad cardiaca se explica por 
la relación lineal con el consumo de vino."

# (b) La recta de regresión mínimo-cuadrática para la predicción de la tasa de
# ataques al corazón a partir del consumo de vino es:

# y = 260,56 - 22,969x

# Utiliza esta ecuación para predecir la tasa de mortalidad por ataques al 
# corazón en un país en el que el consumo de alcohol, procedente del vino, 
# de los adultos es de 4 litros anuales.


y_sombrero <- 260.56 - 22.969 * 4

print(y_sombrero)

"
ŷ = 168.684

============================

En un país con consumo de 4 litros de vino anuales por adulto, se predice una tasa 
de mortalidad de 168.7 muertes por cada 100,000 personas debido a ataques al corazón
"
# (c) La correlación en (a) y la pendiente de la recta de regresión mínimo-cua-
# drática en (b) son ambas negativas. ¿Es posible que estos dos valores 
# tengan signos distintos? Justifica tu respuesta.

regresion <- lm(ejercicio_2_89$Tasa_muertes_corazon ~ ejercicio_2_89$Consumo_alcohol,data = ejercicio_2_89)

desv_consumo <- sd(ejercicio_2_89$Consumo_alcohol)
desv_ataques <- sd(ejercicio_2_89$Tasa_muertes_corazon) 

resumen <- data.frame(
  Medida = c("Correlacion", "Intercepto","Pendiente","Desviación Consumo","Desviación Ataques"),
  Valor_calculado = c(r,coef(regresion)[1],coef(regresion)[2],desv_consumo,desv_ataques))

print(resumen)

"
              Medida Valor_calculado
1        Correlacion       -0.843000
2         Intercepto      260.563375
3          Pendiente      -22.968767
4 Desviación Consumo        2.509724
5 Desviación Ataques       68.396291

==========================================================

No es posible que tengan signos distintos porque existe una relación matemática directa:

Pendiente = r × (sy/sx)

r = -0.843

sy/sx = 68.396 / 2.510 = 27.25 (siempre positivo)

Pendiente = -0.843 × 27.25 = -22.97

Dado que la razón sy/sx es siempre positiva, el signo de la pendiente depende exclusivamente 
del signo de r. Por lo tanto, correlación y pendiente siempre tendrán el mismo signo.
"
# **************************************************
# PREGUNTA 2.90 - Edad y educación en EE UU
# **************************************************

# En general, el nivel educativo de la gente mayor es menor que el de la gente 
# más joven; por tanto, podemos sospechar que existe una relación entre el 
# porcentaje de residentes de un Estado de 65 o más años y el porcentaje de 
# población sin estudios universitarios.

# (a) Explica lo que significa una asociación positiva entre estas variables.

# (b) En el diagrama destacan tres observaciones atípicas. Dos de ellas son Alaska
#     y Florida, que ya se identificaron como observaciones atípicas en el histograma
#     de la figura 1.2. La tercera observación atípica, ¿a qué Estado corresponde?

# (c) Si ignoramos las observaciones atípicas, ¿la relación entre las dos variables
#     tiene una forma y dirección claras? Justifica tu respuesta.

# (d) Si calculamos la correlación, con las tres observaciones atípicas y sin ellas,
#     obtenemos r = 0,054 y r = 0,259. ¿Cuál de estos valores corresponde a la 
#     correlación sin las observaciones atípicas? Justifica tu respuesta.

# **************************************************
# PREGUNTA 2.91 - Comida en mal estado
# **************************************************

# Datos sobre 18 personas que enfermaron después de ingerir comida en mal estado.
# Los datos dan la edad de cada persona en años, el periodo de incubación (el 
# tiempo en horas desde la ingestión de la comida hasta la aparición de los 
# primeros síntomas) y si la víctima sobrevivió (S) o murió (M).

# TABLA DE DATOS:
# ---------------------------------------------------------------
# Persona | Edad | Incubación | Resultado | Persona | Edad | Incubación | Resultado
# ---------------------------------------------------------------
# 1       | 29   | 13         | 0         | 10      | 30   | 36         | 0
# 2       | 39   | 46         | 1         | 11      | 32   | 48         | 0
# 3       | 44   | 43         | 1         | 12      | 59   | 44         | 1
# 4       | 37   | 34         | 0         | 13      | 33   | 21         | 0
# 5       | 42   | 20         | 0         | 14      | 31   | 32         | 0
# 6       | 17   | 20         | 1         | 15      | 32   | 86         | 1
# 7       | 38   | 18         | 0         | 16      | 32   | 48         | 0
# 8       | 43   | 72         | 1         | 17      | 36   | 28         | 1
# 9       | 51   | 19         | 0         | 18      | 50   | 16         | 0
# ---------------------------------------------------------------

# (a) Dibuja un diagrama de dispersión del tiempo de incubación con relación
#     a la edad. Utiliza símbolos distintos para las personas que murieron y para 
#     las que sobrevivieron.

# (b) ¿Existe alguna relación entre la edad y el tiempo de incubación? Si existe,
#     descríbela.

# (c) Más importante, ¿existe alguna relación entre la edad o el periodo de in-
#     cubación y si la víctima sobrevivió? Describe cualquier relación que aquí 
#     parezca importante.

# (d) ¿Existen observaciones atípicas que exijan una investigación aparte?

# **************************************************
# PREGUNTA 2.92 - Nematodos y tomateras
# **************************************************

# Los nematodos son gusanos microscópicos. Tenemos datos de un experimento
# para estudiar el efecto que producen los nematodos que se encuentran en la 
# tierra en el crecimiento de las plantas.

# TABLA DE DATOS:
# ---------------------------------------------------------------
# Nematodos | Crecimiento (cm)
# ---------------------------------------------------------------
# 0         | 10,8  9,1  13,5  9,2
# 1.000     | 11,1  11,1  8,2  11,3
# 5.000     | 5,4  4,6  7,4  5,0
# 10.000    | 5,8  5,3  3,2  7,5
# ---------------------------------------------------------------

# Analiza estos datos y presenta tus conclusiones sobre los efectos de los 
# nematodos en el crecimiento de las plantas.

# **************************************************
# PREGUNTA 2.93 - ¿Valores calientes?
# **************************************************

# En el mundo de las finanzas es frecuente describir el rendimiento de un 
# determinado valor mediante una recta de regresión que relaciona el rendimiento 
# del valor con el rendimiento general del mercado de valores.

# Datos del rendimiento mensual de Philip Morris y los rendimientos mensuales
# del índice bursátil Standard & Poor's correspondiente a 500 valores:
# x̄ = 1,304    ȳ = 1,878
# sx = 3,392   sy = 7,554
# r = 0,5251

# (a) A partir de esta información, halla la ecuación de la recta mínimo-cuadráti-
#     ca. ¿Qué porcentaje de la variación de Philip Morris se explica por la relación
#     lineal con el mercado en su conjunto?

# (b) Explica la información que nos proporciona sobre la pendiente de la recta
#     sobre la respuesta de Philip Morris a las variaciones del mercado. Esta pendiente
#     se llama "beta" en teoría de inversiones.

# (c) Los rendimientos de la mayoría de los valores están correlacionados po-
#     sitivamente con el rendimiento general del mercado. Explica por qué un
#     inversor debería preferir valores con beta > 1 cuando el mercado sube y 
#     acciones con beta < 1 cuando el mercado baja.

# **************************************************
# PREGUNTA 2.94 - La epidemia de gripe de 1918
# **************************************************

# El ejercicio 1.22 proporciona datos sobre la gran epidemia de gripe de 1918.

# (a) Dibuja tres diagramas de dispersión; en uno de ellos relaciona las muer-
#     tes semanales con los casos detectados la misma semana, en otro relaciona 
#     estas muertes semanales con los casos detectados la semana anterior y en 
#     el tercero de ellos relaciona las muertes semanales con los nuevos casos 
#     detectados dos semanas antes. Describe y compara las relaciones que observes.

# (b) Halla las correlaciones de cada una de las relaciones.

# (c) ¿Cuáles son tus conclusiones? ¿Cómo se predice mejor el número de muer-
#     tes, con los nuevos casos de la misma semana, con los nuevos casos de la 
#     semana anterior o los nuevos casos de dos semanas antes?

# **************************************************
# PREGUNTA 2.95 - Salarios de mujeres
# **************************************************

# Un estudio de la National Science Foundation de EEUU halló que la mediana
# del salario de ingenieras y científicas estadounidenses recién graduadas era 
# sólo un 73% de la mediana de sus homólogos varones.

# Porcentajes de salarios de mujeres respecto a hombres por especialidad:
# 94% 96% 98% 95% 85% 85% 84% 100%
# 103% 100% 107% 93% 104% 93% 106% 100%

# ¿Cómo es posible que el salario de las mujeres se encuentre muy por debajo
# del de los hombres cuando se consideran todas las disciplinas conjuntamente y,
# en cambio, sea prácticamente el mismo cuando se considera por especialidades?