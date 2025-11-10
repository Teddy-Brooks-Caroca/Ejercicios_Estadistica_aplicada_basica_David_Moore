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
# más joven; por tanto, podemos sospechar que existe una relación entre el porcentaje 
# de residentes de un Estado de 65 o más años y el porcentaje de población sin estudios 
# universitarios. La figura 2.22 muestra la relación entre estas variables. 
# Los datos son los que aparecen en la tablas 1.1 y 2.1.

# (a) Explica lo que significa una asociación positiva entre estas variables.

"Una asociación positiva significa que existe una relación directa entre las variables: a medida que 
aumenta el porcentaje de residentes mayores de 65 años en un estado, también tiende a aumentar el porcentaje 
de población sin estudios universitarios. En otras palabras, los estados con poblaciones más envejecidas tienden 
a tener niveles educativos más bajos en términos de educación universitaria."

# (b) En el diagrama destacan tres observaciones atípicas. Dos de ellas son Alaska
# y Florida, que ya se identificaron como observaciones atípicas en el histograma
# de la figura 1.2. La tercera observación atípica, ¿a qué Estado corresponde?

mayores_65 <- read.csv("ejercicio_2_90_mayores_65.csv")
educacion <- read.csv("ejercicio_2_14_educacion.csv")

puntuaciones_z <- scale(mayores_65$Porcentaje)
umbral_z <- 1.8 

indices_oas <- which(abs(puntuaciones_z) > umbral_z)
oas <- mayores_65[indices_oas, ]

promedio_mayores <- mean(mayores_65$Porcentaje)
promedio_sin_educacion <- mean(educacion$Porcentaje_sin_estudios_secundaria)

df_resultados <- rbind(
  data.frame(Categoria = "OA - Puntuación Z > |2|", Estado = oas$Estado, 
             Valor = oas$Porcentaje, Z_Score = round(puntuaciones_z[indices_oas], 2)),
  data.frame(Categoria = "Promedio mayores 65+", Estado = "Todos", 
             Valor = promedio_mayores, Z_Score = NA),
  data.frame(Categoria = "Promedio sin educación secundaria", Estado = "Todos", 
             Valor = promedio_sin_educacion, Z_Score = NA)
)

print(df_resultados)

"
                          Categoria  Estado    Valor Z_Score
1           OA - Puntuación Z > |2|  Alaska  5.20000   -3.73
2           OA - Puntuación Z > |2| Florida 18.50000    2.87
3           OA - Puntuación Z > |2|    Utah  8.80000   -1.94
4              Promedio mayores 65+   Todos 12.71200      NA
5 Promedio sin educación secundaria   Todos 23.77647      NA
"

# (c) Si ignoramos las observaciones atípicas, ¿la relación entre las dos variables
# tiene una forma y dirección claras? Justifica tu respuesta.

"Sí, la relación tiene forma y dirección claras. Si ignoramos las tres observaciones atípicas (Alaska, Florida y Utah), 
la relación muestra una dirección positiva clara: a mayor porcentaje de población anciana, mayor porcentaje de personas 
sin estudios universitarios. La forma es aproximadamente lineal, aunque con cierta dispersión normal en los datos."

# (d) Si calculamos la correlación, con las tres observaciones atípicas y sin ellas,
# obtenemos r = 0,054 y r = 0,259. ¿Cuál de estos valores corresponde a la correlación 
# sin las observaciones atípicas? Justifica tu respuesta.

mayores_65 <- read.csv("ejercicio_2_90_mayores_65.csv")
educacion <- read.csv("ejercicio_2_14_educacion.csv")

abrev_a_nombre <- c(
  "AL" = "Alabama", "AK" = "Alaska", "AZ" = "Arizona", "AR" = "Arkansas",
  "CA" = "California", "CO" = "Colorado", "CT" = "Connecticut", "DE" = "Delaware",
  "FL" = "Florida", "GA" = "Georgia", "HI" = "Hawai", "ID" = "Idaho",
  "IL" = "Illinois", "IN" = "Indiana", "IA" = "Iowa", "KS" = "Kansas",
  "KY" = "Kentucky", "LA" = "Luisiana", "ME" = "Maine", "MD" = "Maryland",
  "MA" = "Massachusetts", "MI" = "Michigan", "MN" = "Minnesota", "MS" = "Misisipí",
  "MO" = "Misuri", "MT" = "Montana", "NE" = "Nebraska", "NV" = "Nevada",
  "NH" = "New Hampshire", "NJ" = "Nueva Jersey", "NM" = "Nuevo México",
  "NY" = "Nueva York", "NC" = "Carolina del Norte", "ND" = "Dakota del Norte",
  "OH" = "Ohio", "OK" = "Oklahoma", "OR" = "Oregón", "PA" = "Pensilvania",
  "RI" = "Rhode Island", "SC" = "Carolina del Sur", "SD" = "Dakota del Sur",
  "TN" = "Tennessee", "TX" = "Tejas", "UT" = "Utah", "VT" = "Vermont",
  "VA" = "Virginia", "WA" = "Washington", "WV" = "Virginia Occidental",
  "WI" = "Wisconsin", "WY" = "Wyoming"
)

educacion$Estado_nombre <- abrev_a_nombre[educacion$Estado]

datos <- merge(mayores_65, educacion, by.x = "Estado", by.y = "Estado_nombre")

cor_con <- cor(datos$Porcentaje, datos$Porcentaje_sin_estudios_secundaria)

datos_sin_oas <- datos[!datos$Estado %in% c("Alaska", "Florida", "Utah"), ]

cor_sin <- cor(datos_sin_oas$Porcentaje, datos_sin_oas$Porcentaje_sin_estudios_secundaria)

cat("CORRELACIONES FINALES:\n")
cat("CON OAs:", round(cor_con, 3), "\n")
cat("SIN OAs:", round(cor_sin, 3), "\n")

"El valor r = 0.259 corresponde a la correlación SIN las observaciones atípicas, mientras que r = 0.054 
corresponde a la correlación CON todas las observaciones.
Las observaciones atípicas (especialmente Utah con bajo porcentaje de ancianos pero también bajo porcentaje 
sin estudios universitarios, y Alaska con características similares) distorsionan y debilitan la relación positiva 
general. Al eliminarlas, la correlación se hace más fuerte y evidente, aumentando de 0.054 a 0.259, lo que indica 
una relación positiva más clara entre las variables."

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
# ------------------------------------------------------------------------------
# 1       | 29   | 13         | 0         | 10      | 30   | 36         | 0
# 2       | 39   | 46         | 1         | 11      | 32   | 48         | 0
# 3       | 44   | 43         | 1         | 12      | 59   | 44         | 1
# 4       | 37   | 34         | 0         | 13      | 33   | 21         | 0
# 5       | 42   | 20         | 0         | 14      | 31   | 32         | 0
# 6       | 17   | 20         | 1         | 15      | 32   | 86         | 1
# 7       | 38   | 18         | 0         | 16      | 32   | 48         | 0
# 8       | 43   | 72         | 1         | 17      | 36   | 28         | 1
# 9       | 51   | 19         | 0         | 18      | 50   | 16         | 0
# ------------------------------------------------------------------------------

# 1 = sobrevivió / 0 = murió 

# (a) Dibuja un diagrama de dispersión del tiempo de incubación con relación
#     a la edad. Utiliza símbolos distintos para las personas que murieron y para 
#     las que sobrevivieron.

personas_enfermas <- data.frame(
  Persona = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  Edad = c(29, 39, 44, 37, 42, 17, 38, 43, 51, 30, 32, 59, 33, 31, 32, 32, 36, 50),
  Incubación = c(13, 46, 43, 34, 20, 20, 18, 72, 19, 36, 48, 44, 21, 32, 86, 48, 28, 16),
  Resultado = c(0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0)
)

print(personas_enfermas)

sobrevivieron <- personas_enfermas[personas_enfermas$Resultado == 0, ]
murieron <- personas_enfermas[personas_enfermas$Resultado == 1, ]

plot(personas_enfermas$Edad,
     personas_enfermas$Incubación,
     main = "Personas enfermas",
     xlab = "Edad del paciente (en años)",
     ylab = "Período de incubación(en horas)")
points(sobrevivieron$Edad, sobrevivieron$Incubación, 
       pch = 16, col = "blue", cex = 1.2)  
points(murieron$Edad, murieron$Incubación, 
       pch = 17, col = "red", cex = 1.2)
grid()

# (b) ¿Existe alguna relación entre la edad y el tiempo de incubación? Si existe,
# descríbela.

correlacion <- cor(personas_enfermas$Edad,personas_enfermas$Incubación)

print(correlacion)

"A simple vista no se ve que haya alguna relación entre las variables, más aún cuando vemos que
la correlación entre las mismas es de 0.028"

# (c) Más importante, ¿existe alguna relación entre la edad o el periodo de incubación 
# y si la víctima sobrevivió? Describe cualquier relación que aquí parezca importante.

cor_edad <- cor(personas_enfermas$Edad, personas_enfermas$Resultado)

cor_incubacion <- cor(personas_enfermas$Incubación, personas_enfermas$Resultado)

resumen_correlaciones <- data.frame(
  Correlaciones = c("Edad","Incubación"),
  Resultado = c(round(cor_edad,3),round(cor_incubacion,3))
)

print(resumen_correlaciones)

"
  Correlaciones Resultado
1          Edad     0.091
2    Incubación     0.525

==============================================

Los pacientes con tiempos de incubación más cortos tienen mayor riesgo de muerte, 
mientras que los de incubación más larga tienden a sobrevivir.
"

# (d) ¿Existen observaciones atípicas que exijan una investigación aparte?

media_edad <- mean(personas_enfermas$Edad)
media_inc <- mean(personas_enfermas$Incubación) 

sd_edad <- sd(personas_enfermas$Edad)
sd_inc <- sd(personas_enfermas$Incubación)

personas_enfermas$z_edad <- (personas_enfermas$Edad - media_edad) / sd_edad
personas_enfermas$z_inc <- (personas_enfermas$Incubación - media_inc) / sd_inc

umbral_z <- 2 

out_edad <- personas_enfermas[abs(personas_enfermas$z_edad) > umbral_z, ]
out_inc <- personas_enfermas[abs(personas_enfermas$z_inc) > umbral_z, ]

outliers_combinado <- rbind(
  data.frame(Variable = "Edad", 
             Persona = out_edad$Persona,
             Valor = out_edad$Edad,
             Z_Score = out_edad$z_edad,
             Resultado = out_edad$Resultado),
  data.frame(Variable = "Incubación", 
             Persona = out_inc$Persona,
             Valor = out_inc$Incubación,
             Z_Score = out_inc$z_inc,
             Resultado = out_inc$Resultado)
)

print(outliers_combinado)

"
    Variable Persona Valor   Z_Score Resultado
1       Edad       6    17 -2.112762         1
2       Edad      12    59  2.215823         1
3 Incubación      15    86  2.537536         1
"
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

nematodos <- data.frame(
  Nematodos = rep(c(0, 1000, 5000, 10000), each = 4),
  Crecimiento = c(
    10.8, 9.1, 13.5, 9.2,
    11.1, 11.1, 8.2, 11.3,
    5.4, 4.6, 7.4, 5.0,
    5.8, 5.3, 3.2, 7.5
  )
)

r <- cor(nematodos$Nematodos, nematodos$Crecimiento)
cat("Correlación:", round(r, 3), "\n")

modelo <- lm(Crecimiento ~ Nematodos, data = nematodos)
summary(modelo)

plot(nematodos$Nematodos, nematodos$Crecimiento,
     main = "Relación entre nematodos y crecimiento",
     xlab = "Número de nematodos",
     ylab = "Crecimiento (cm)",
     pch = 16, col = "darkgreen")
abline(modelo, col = "red", lwd = 2)
grid()

"Existe una relación lineal negativa muy clara entre el número de nematodos y 
el crecimiento de las tomateras.
El modelo indica que cada incremento en la cantidad de nematodos reduce significativamente 
el desarrollo de las plantas, lo cual respalda la hipótesis de que los nematodos afectan de 
manera adversa el crecimiento vegetal."


# **************************************************
# PREGUNTA 2.93 - ¿Valores calientes?
# **************************************************

# En el mundo de las finanzas es frecuente describir el rendimiento de un 
# determinado valor mediante una recta de regresión que relaciona el rendimiento 
# del valor con el rendimiento general del mercado de valores.Esta representación 
# nos ayuda a visualizar en qué medida el valor sigue la pauta general del mercado. 
# Analizamos el rendimiento mensual y de Philip Morris y los rendimientos mensuales 
# x del índice bursátil Standard & Poor’s correspondiente a 500 valores, que representa 
# el mercado en su conjunto, entre julio de 1990 ymayo de1997. He aquí los resultados:

# x̄ = 1,304    ȳ = 1,878
# sx = 3,392   sy = 7,554
# r = 0,5251

# (a) A partir de esta información, halla la ecuación de la recta mínimo-cuadrática. 
# ¿Qué porcentaje de la variación de Philip Morris se explica por la relación lineal 
# con el mercado en su conjunto?

pendiente <- 0.5251 * (7.554 / 3.392)

intercepto <- 1.878 - (pendiente * 1.304)

porcentaje_variacion <- 0.5251 * 0.5251

valores_recta <- data.frame(
  Medida = c("Pendiente","Intercepto","% de variación"),
  Valores = c(pendiente,intercepto,porcentaje_variacion)
)

print(valores_recta)

"
          Medida   Valores
1      Pendiente 1.1694002
2     Intercepto 0.3531022
3 % de variación 0.2757300

======================================

La ecuación de la recta de regresión es ŷ = 0,353 + 1,169x. Esto indica que, en promedio, 
por cada punto porcentual que aumenta el rendimiento del mercado, el de Philip Morris aumenta 1,17 puntos. 
Además, alrededor del 27,6 % de la variación en los rendimientos de Philip Morris se explica por su relación 
lineal con el mercado bursátil.
"

# (b) Explica la información que nos proporciona sobre la pendiente de la recta sobre 
# la respuesta de Philip Morris a las variaciones del mercado. Esta pendiente se llama "beta" 
# en teoría de inversiones.

"La pendiente o beta = 1,17 indica que el rendimiento de Philip Morris tiende a variar más que proporcionalmente 
respecto al mercado: por cada 1 % que cambia el rendimiento del mercado, el de Philip Morris cambia, en promedio, 
un 1,17 %. Esto significa que la acción es más volátil o sensible a las variaciones del mercado, amplificando tanto 
las subidas como las bajadas."

# (c) Los rendimientos de la mayoría de los valores están correlacionados positivamente 
# con el rendimiento general del mercado. Explica por qué un inversor debería preferir 
# valores con beta > 1 cuando el mercado sube y acciones con beta < 1 cuando el mercado baja.

"Cuando el mercado sube, un inversor busca maximizar ganancias, por lo que prefiere acciones con beta > 1, ya que 
aumentan más que el promedio del mercado. En cambio, cuando el mercado baja, conviene elegir acciones con beta < 1, 
pues estas tienden a caer menos, ofreciendo mayor protección frente a pérdidas."

# **************************************************
# PREGUNTA 2.94 - La epidemia de gripe de 1918
# **************************************************

# El ejercicio 1.22 proporciona datos sobre la gran epidemia de gripe de 1918.

# (a) Dibuja tres diagramas de dispersión; en uno de ellos relaciona las muertes 
# semanales con los casos detectados la misma semana, en otro relaciona estas muertes 
# semanales con los casos detectados la semana anterior y en el tercero de ellos relaciona 
# las muertes semanales con los nuevos casos detectados dos semanas antes. Describe y 
# compara las relaciones que observes.

ejercicio_2_94 <- read.csv("ejercicio_1_22_epidemia.csv")

str(ejercicio_2_94)

View(ejercicio_2_94)

par(mfrow = c(1, 3))  

#Grafico 1: misma semana
plot(ejercicio_2_94$Muertos,
     ejercicio_2_94$Nuevos_casos,
     main = "Relacion de casos con gripe 1918",
     xlab = "Decesos",
     ylab = "Nuevos casos",
     pch = 16,
     col = "blue")
grid()

#Grafico 2: una semana de desfase
una_semana <- c(NA, head( ejercicio_2_94$Nuevos_casos , -1))

valido <- !is.na(una_semana)

plot(ejercicio_2_94$Muertos[valido],
     una_semana[valido],
     main = "Comparacion de casos de gripe 1918",
     sub = "Comparativa de una semana",
     xlab = "Decesos",
     ylab = "Nuevos casos",
     pch = 19,
     col = "red")
grid()

#Gráfico 3: dos semanas de desfase
dos_semanas <- c(NA,NA, head( ejercicio_2_94$Nuevos_casos , -2))

valido2 <- !is.na(dos_semanas)

plot(ejercicio_2_94$Muertos[valido2],
     dos_semanas[valido2],
     main = "Comparacion de casos de gripe 1918",
     sub = "Comparativa de dos semana",
     xlab = "Decesos",
     ylab = "Nuevos casos",
     pch = 19,
     col = "green")
grid()

par(mfrow = c(1, 1))

# (b) Halla las correlaciones de cada una de las relaciones.

cor_misma_semana <- cor(ejercicio_2_94$Muertos,ejercicio_2_94$Nuevos_casos)
cor_una_semana <- cor(ejercicio_2_94$Muertos[valido],una_semana[valido])
cor_dos_semanas <- cor(ejercicio_2_94$Muertos[valido2],dos_semanas[valido2])

resultaods_correlaciones <- data.frame(
  Correlaciones = c("Misma semana","Una semana desfase","Dos semanas desfase"),
  Valores = c(cor_misma_semana,cor_una_semana,cor_dos_semanas)
)

print(resultaods_correlaciones)

"
        Correlaciones   Valores
1        Misma semana 0.8562287
2  Una semana desfase 0.9109524
3 Dos semanas desfase 0.3932467
"

# (c) ¿Cuáles son tus conclusiones? ¿Cómo se predice mejor el número de muertes, 
# con los nuevos casos de la misma semana, con los nuevos casos de la semana anterior 
# o los nuevos casos de dos semanas antes?

"La correlación más alta (r ≈ 0.91) se observa cuando se comparan las muertes con los casos de 
una semana antes, lo que indica que el número de muertes está más estrechamente relacionado con 
los contagios ocurridos la semana previa. Esto tiene sentido biológicamente, ya que las muertes 
suelen producirse unos días después del contagio. En cambio, la correlación con los casos de la 
misma semana o de dos semanas antes es menor, lo que sugiere que la mejor predicción del número 
de muertes se obtiene usando los casos de la semana anterior."

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