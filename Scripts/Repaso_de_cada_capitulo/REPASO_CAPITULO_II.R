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
# sólo un 73% de la mediana de sus homólogos varones.Ahora bien, cuando las recién 
# graduadas se agrupaban por especialidades, la situación era distinta. Las medianas 
# de los salarios de las mujeres, expresadas como un porcentaje de las medianas del 
# salario de los hombres, en 16 campos de estudio eran:

# Porcentajes de salarios de mujeres respecto a hombres por especialidad:
# 94% 96% 98% 95% 85% 85% 84% 100%
# 103% 100% 107% 93% 104% 93% 106% 100%

# ¿Cómo es posible que el salario de las mujeres se encuentre muy por debajo
# del de los hombres cuando se consideran todas las disciplinas conjuntamente y,
# en cambio, sea prácticamente el mismo cuando se considera por especialidades?

"Aunque los salarios medianos por especialidad muestran que las mujeres ganan aproximadamente 
lo mismo (e incluso más) que los hombres dentro de cada campo, el salario global promedio de las 
mujeres resulta mucho menor (73% del de los hombres). Esto ocurre porque las mujeres están sobrerrepresentadas 
en las especialidades peor pagadas y subrepresentadas en las mejor pagadas.

En otras palabras, la distribución desigual entre campos con distintos niveles salariales distorsiona la 
comparación global, dando la impresión de una brecha salarial mayor de la que realmente existe dentro de 
cada especialidad.

Este fenómeno se conoce como paradoja de Simpson, y refleja cómo una tendencia que se mantiene en varios grupos 
individuales puede invertirse o desaparecer al combinar los datos."

# **************************************************
# PREGUNTA 2.96 - Transformación de datos
# **************************************************

# Los ecólogos recogen datos para estudiar la naturaleza. La tabla 2.12 
# proporciona datos sobre la media del número de semillas producidas durante 
# un año por algunas especies comunes de árboles y también sobre el peso 
# medio (en miligramos) de éstas.

## TABLA 2.12 - Peso y recuento del número de semillas producidas por especies arbóreas:
# ------------------------------------------------------------------------------------
# Especies               | Número semillas | Peso semillas (mg)
# ------------------------------------------------------------------------------------
# Abedul para papel      | 27239           | 0.6
# Abedul amarillo        | 12158           | 1.6
# Picea del Canadá       | 7202            | 2.0
# Picea de Engelman      | 3671            | 3.3
# Picea roja del Canadá  | 5051            | 3.4
# Tulipanero             | 13509           | 9.1
# Pino ponderosa         | 2667            | 37.7
# Abeto                  | 5196            | 40.0
# Arce del azúcar        | 1751            | 48.0
# Pino                   | 1159            | 216.0
# Haya americana         | 463             | 247
# Haya americana         | 1892            | 247
# Encina                 | 93              | 1851
# Encina escarlata       | 525             | 1930
# Roble rojo americano   | 411             | 2475
# Roble rojo americano   | 253             | 2475
# Avellano de América    | 40              | 3423
# Roble blanco del Canadá| 184             | 3669
# Roble blanco americano | 107             | 4535
# ------------------------------------------------------------------------------------

# (a) Dibuja un diagrama de dispersión que muestre cómo se puede explicar el
# número de semillas producidas por un árbol, a partir del peso de éstas. Describe
# la forma, la dirección y la fuerza de la relación.

ejercicio_2_96 <- read.csv("ejercicio_2_96_arboles.csv")

str(ejercicio_2_96)

View(ejercicio_2_96)

modelo <- lm(Numero_semillas ~ Peso_semillas_mg, data = ejercicio_2_96)

plot(ejercicio_2_96$Peso_semillas_mg,
     ejercicio_2_96$Numero_semillas,
     main = "Especies arboreas de EE.UU",
     xlab = "Peso de la semilla (en mg)",
     ylab = "Cantidad de semillas",
     col = "blue",
     pch = 16)
text(ejercicio_2_96$Peso_semillas_mg,
     ejercicio_2_96$Numero_semillas,
     labels = ejercicio_2_96$Especie,
     pos = 4,   
     cex = 0.7) 
abline(modelo, col = "red", lwd = 2)
grid()

"El diagrama de dispersión muestra una relación inversa y claramente no lineal entre el peso y el número de semillas. 
En general, las especies con semillas más ligeras producen muchas más semillas, mientras que aquellas con semillas más 
pesadas generan pocas. La dirección de la relación es negativa, y su fuerza es alta, aunque la forma de la nube de puntos 
sugiere que una transformación logarítmica podría describir mejor la tendencia que una recta simple."

# (b) Cuando tratamos con tamaños y pesos, los logaritmos de los datos originales son a 
# menudo la forma más adecuada de expresar los datos. Utiliza tu calculadora o un programa 
# informático para calcular los logaritmos de los pesos y recuentos de la tabla 2.12. Dibuja 
# un nuevo diagrama de dispersión utilizando los datos transformados. Ahora, ¿cuál es la forma, 
# la dirección y la fuerza de la relación?

ejercicio_2_96$log_peso_semillas <- log(ejercicio_2_96$Peso_semillas_mg)
ejercicio_2_96$log_cantidad_semillas <- log(ejercicio_2_96$Numero_semillas)

head(ejercicio_2_96[, c("Peso_semillas_mg", "Numero_semillas", "log_peso_semillas", "log_cantidad_semillas")])

par(mfrow = c(1, 2)) 

modelo <- lm(Numero_semillas ~ Peso_semillas_mg, data = ejercicio_2_96)

modelo_log <- lm(log_cantidad_semillas ~ log_peso_semillas, data = ejercicio_2_96)

plot(ejercicio_2_96$Peso_semillas_mg,
     ejercicio_2_96$Numero_semillas,
     main = "Especies arboreas de EE.UU",
     xlab = "Peso de la semilla (en mg)",
     ylab = "Cantidad de semillas",
     col = "blue",
     pch = 16)
text(ejercicio_2_96$Peso_semillas_mg,
     ejercicio_2_96$Numero_semillas,
     labels = ejercicio_2_96$Especie,
     pos = 4,   
     cex = 0.7) 
abline(modelo, col = "red", lwd = 2)
grid()

plot(ejercicio_2_96$log_peso_semillas,
     ejercicio_2_96$log_cantidad_semillas,
     main = "Especies arboreas de EE.UU (escala logarítmica)",
     xlab = "Peso de la semilla (en mg)",
     ylab = "Cantidad de semillas",
     col = "darkgreen",
     pch = 16)
text(ejercicio_2_96$log_peso_semillas,
     ejercicio_2_96$log_cantidad_semillas,
     labels = ejercicio_2_96$Especie,
     pos = 4,   
     cex = 0.7) 
abline(modelo_log, col = "red", lwd = 2)
grid()

par(mfrow = c(1, 1))

"Al aplicar los logaritmos al número y peso de las semillas, la relación se vuelve aproximadamente lineal, 
negativa y fuerte. En la escala original, la dispersión era muy amplia y la relación parecía curvilínea —los 
árboles con semillas más ligeras producían muchas más unidades, pero de forma no proporcional. Después de la 
transformación logarítmica, la tendencia se aclara: a medida que aumenta el peso de las semillas, el número 
de semillas disminuye de forma regular y predecible."

# **************************************************
# PREGUNTA 2.97 - Hombres y mujeres
# **************************************************

# La altura media de las mujeres estadounidenses cuando tienen 20 años de edad es 
# de aproximadamente 164 cm, con una desviación típica de unos 6,35 cm. 
# La altura media de los hombres de la misma edad es de aproximadamente 174 cm, 
# con una desviación típica de unos 6,86 cm. Si la correlación entre las alturas 
# de parejas de hombres y mujeres jóvenes es aproximadamente r = 0,5.

# DATOS:
# Mujeres: μ_x = 164 cm, σ_x = 6.35 cm
# Hombres:  μ_y = 174 cm, σ_y = 6.86 cm
# Correlación: r = 0.5

# (a) ¿Cuál es la pendiente de la recta de regresión de la altura de los hombres
# con relación a la altura de sus mujeres en las parejas jóvenes?

pendiente <- 0.5 * (6.86 / 6.35)

cat("La pendiente es:", round(pendiente,2),"cm.")

"La pendiente es: 0.54 cm."

# (b) Dibuja un gráfico de esta recta de regresión.

intercepto <- 174 - (pendiente * 164)

x_valores <- seq(140, 190, length.out = 100)

y_valores <- intercepto + pendiente * x_valores #recta de regresión

plot(x_valores, y_valores,
     type = "l",
     col = "blue",
     lwd = 2,
     xlab = "Altura mujeres (cm)",
     ylab = "Altura hombres (cm)",
     main = "Recta de regresión: altura hombres sobre altura mujeres")
grid()

# (c) Predice la altura de un hombre cuya mujer mide 170 cm de altura.

prediccion_hombre <- intercepto + pendiente * 170

cat("La altura de un hombre cuya mujer mide 170 cm es:", round(prediccion_hombre,2),"cm.")

"La altura de un hombre cuya mujer mide 170 cm es: 177.24 cm."

# **************************************************
# PREGUNTA 2.98 - Un juego informático
# **************************************************

# Un sistema multimedia para aprender estadística incluye una prueba para 
# valorar la destreza de los sujetos en la utilización del ratón (mouse). 
# El programa informático hace que aparezca, al azar, un círculo en la pantalla. 
# El sujeto tiene que situarse sobre el círculo y clicar tan rápido como pueda. 
# Tan pronto como el usuario ha clicado sobre el círculo, aparece uno nuevo.

# La tabla 2.13 proporciona datos sobre los ensayos realizados por un sujeto, 
# 20 con cada mano. "Distancia" es la distancia desde el centro del círculo al 
# punto donde se halla el cursor en el momento del clicado, las unidades de 
# medida dependen del tamaño de la pantalla. "Tiempo" es el tiempo transcurrido 
# entre el clicado de dos círculos consecutivos, en milisegundos.

# TABLA 2.13 - Tiempos de respuesta en un juego informático:
# ------------------------------------------------------------------------------------
# Tiempo | Distancia | Mano      | Tiempo | Distancia | Mano
# ------------------------------------------------------------------------------------
# 115   | 190.70    | derecha   | 240   | 190.70    | izquierda
# 96    | 138.52    | derecha   | 190   | 138.52    | izquierda
# 110   | 165.08    | derecha   | 170   | 165.08    | izquierda
# 100   | 126.19    | derecha   | 125   | 126.19    | izquierda
# 111   | 163.19    | derecha   | 315   | 163.19    | izquierda
# 101   | 305.66    | derecha   | 240   | 305.66    | izquierda
# 111   | 176.15    | derecha   | 141   | 176.15    | izquierda
# 106   | 162.78    | derecha   | 210   | 162.78    | izquierda
# 96    | 147.87    | derecha   | 200   | 147.87    | izquierda
# 96    | 271.46    | derecha   | 401   | 271.46    | izquierda
# 95    | 40.25     | derecha   | 320   | 40.25     | izquierda
# 96    | 24.76     | derecha   | 113   | 24.76     | izquierda
# 96    | 104.80    | derecha   | 176   | 104.80    | izquierda
# 106   | 136.80    | derecha   | 211   | 136.80    | izquierda
# 100   | 308.60    | derecha   | 238   | 308.60    | izquierda
# 113   | 279.80    | derecha   | 316   | 279.80    | izquierda
# 123   | 125.51    | derecha   | 176   | 125.51    | izquierda
# 111   | 329.80    | derecha   | 173   | 329.80    | izquierda
# 95    | 51.66     | derecha   | 210   | 51.66     | izquierda
# 108   | 201.95    | derecha   | 170   | 201.95    | izquierda
# ------------------------------------------------------------------------------------

# (a) Sospechamos que el tiempo depende de la distancia. Dibuja un diagrama de 
# dispersión del tiempo con relación a la distancia. Utiliza símbolos distintos
# para cada mano.

ejercicio_2_98 <- read.csv("ejercicio_2_98_mouse.csv")

str(ejercicio_2_98)

head(ejercicio_2_98)

derecha <- ejercicio_2_98[ejercicio_2_98$Mano == "derecha", ]
izquierda <- ejercicio_2_98[ejercicio_2_98$Mano == "izquierda", ]

plot(ejercicio_2_98$Tiempo,
     ejercicio_2_98$Distancia,
     main = "Juego del ratón",
     xlab = "Tiempo (en milisegundo)",
     ylab = "Distancia")
points(derecha$Tiempo, derecha$Distancia, 
       pch = 16, col = "blue", cex = 1.2)  
points(izquierda$Tiempo, izquierda$Distancia, 
       pch = 17, col = "red", cex = 1.2)
grid()

# (b) Describe la relación que observas. ¿Puedes afirmar que el sujeto es diestro?

"Se puede afirmar que el sujeto es diestro, ya que la relacion entre la distancia y tiempo es mas pequeña con
la mañana derecha que con la mano izquierda"

# (c) Halla la recta de regresión del tiempo con relación a la distancia para las
# dos manos de forma independiente. Dibuja estas rectas en tu diagrama. De las dos
# regresiones, ¿cuál es mejor para predecir el tiempo a partir de la distancia? Da
# medidas numéricas que describan la precisión de las dos regresiones.

#MANO DERECHA
media_derecha_tiempo <- mean(derecha$Tiempo)
media_derecha_distacia <- mean(derecha$Distancia)

desv_derecha_tiempo <- sd(derecha$Tiempo)
desv_derecha_distancia <- sd(derecha$Distancia)

r_derecha <- cor(derecha$Tiempo,derecha$Distancia)

pendiente_derecha <- r_derecha * (desv_derecha_tiempo / desv_derecha_distancia)

intercepto_derecha <- media_derecha_tiempo - (pendiente_derecha * media_derecha_distacia)

#MANO IZQUIERDA
media_izquierda_tiempo <- mean(izquierda$Tiempo)
media_izquierda_distacia <- mean(izquierda$Distancia)

desv_izquierda_tiempo <- sd(izquierda$Tiempo)
desv_izquierda_distancia <- sd(izquierda$Distancia)

r_izquierda <- cor(izquierda$Tiempo,izquierda$Distancia)

pendiente_izquierda <- r_izquierda * (desv_izquierda_tiempo / desv_izquierda_distancia)

intercepto_izquierda <- media_izquierda_tiempo - (pendiente_izquierda * media_izquierda_distacia)

resultados <- data.frame(
  mano = c("derecha", "izquierda"),
  media_tiempo = c(media_derecha_tiempo, media_izquierda_tiempo),
  media_distancia = c(media_derecha_distacia, media_izquierda_distacia),
  desv_tiempo = c(desv_derecha_tiempo, desv_izquierda_tiempo),
  desv_distancia = c(desv_derecha_distancia, desv_izquierda_distancia),
  r = c(r_derecha, r_izquierda),
  pendiente = c(pendiente_derecha, pendiente_izquierda),
  intercepto = c(intercepto_derecha, intercepto_izquierda)
)

print(resultados)

plot(ejercicio_2_98$Tiempo,
     ejercicio_2_98$Distancia,
     main = "Juego del ratón",
     xlab = "Tiempo (en milisegundo)",
     ylab = "Distancia")
points(derecha$Tiempo, derecha$Distancia, 
       pch = 16, col = "blue", cex = 1.2)  
points(izquierda$Tiempo, izquierda$Distancia, 
       pch = 17, col = "red", cex = 1.2)
abline(intercepto_derecha,pendiente_derecha,col = "darkblue",lwd = 2,lty = "dashed")
abline(intercepto_izquierda,pendiente_izquierda,col ="darkred",lwd = 2, lty = "dashed")
grid()

"
       mano media_tiempo media_distancia desv_tiempo desv_distancia         r  pendiente intercepto
1   derecha       104.25        172.5765    8.245413       88.74672 0.3047128 0.02831071   99.36424
2 izquierda       216.75        172.5765   73.014689       88.74672 0.3183573 0.26192243  171.54834

========================================================================

La regresión de la mano izquierda es ligeramente mejor porque su correlación con la distancia es un poco 
más alta que la de la mano derecha, lo que indica que la distancia explica un poco mejor el tiempo en esa mano; 
sin embargo, en ambos casos la relación es débil, por lo que ninguna recta predice muy bien los tiempos.
"

# (d) Debido al aprendizaje, es posible que el sujeto lo haga mejor en los últimos ensayos. 
# También es posible que lo haga peor debido a la fatiga. Dibuja un diagrama de residuos en 
# el que los residuos aparezcan ordenados de acuerdo al orden de realización de los ensayos 
# (de arriba abajo en la tabla 2.12). ¿Existe algún efecto sistemático en el orden de realización 
# de las pruebas?


derecha$Prediccion <- intercepto_derecha + pendiente_derecha * derecha$Distancia
derecha$Residuos <- derecha$Tiempo - derecha$Prediccion

izquierda$Prediccion <- intercepto_izquierda + pendiente_izquierda * izquierda$Distancia
izquierda$Residuos <- izquierda$Tiempo - izquierda$Prediccion

derecha$Orden <- 1:20
izquierda$Orden <- 1:20

residuos_combinados <- rbind(
  data.frame(Mano = "Derecha", Orden = derecha$Orden, Residuos = derecha$Residuos),
  data.frame(Mano = "Izquierda", Orden = izquierda$Orden, Residuos = izquierda$Residuos)
)

plot(residuos_combinados$Orden, residuos_combinados$Residuos,
     main = "Residuos vs Orden de Ensayos",
     xlab = "Orden del Ensayo",
     ylab = "Residuos",
     ylim = c(-50, 150),
     col = ifelse(residuos_combinados$Mano == "Derecha", "blue", "red"),
     pch = ifelse(residuos_combinados$Mano == "Derecha", 16, 17)
     )
abline(h = 0, col = "gray", lty = 2)
abline(lm(Residuos ~ Orden, data = residuos_combinados[residuos_combinados$Mano == "Derecha",]), 
       col = "blue", lty = 2)
abline(lm(Residuos ~ Orden, data = residuos_combinados[residuos_combinados$Mano == "Izquierda",]), 
       col = "red", lty = 2)
grid()

"No existe un efecto sistemático evidente relacionado con el orden de realización de las pruebas, 
ya que los residuos no muestran una tendencia clara de aumento o disminución a lo largo de los ensayos 
para ninguna de las dos manos, lo que indica que factores como el aprendizaje o la fatiga no influyeron
significativamente en el desempeño del sujeto durante la prueba."

# **************************************************
# PREGUNTA 2.99 - Calificaciones SAT en EE UU
# **************************************************

# La tabla 2.1 proporciona datos sobre la educación en los Estados de EE UU.
# Utiliza un programa estadístico para examinar la relación entre las calificaciones
# de Matemáticas y de Lengua en la prueba SAT de la manera siguiente:

# (a) Quieres predecir la calificación de Matemáticas en la prueba SAT de un Estado 
# a partir de su calificación de Lengua. Con este fin, halla la recta de regresión
# mínimo-cuadrática. Sabes que la calificación media de Lengua de un determinado 
# Estado al año siguiente fue 455. Utiliza tu recta de regresión para predecir su
# calificación media de Matemáticas.

ejercicio_2_99 <- read.csv("ejercicio_2_14_educacion.csv")

str(ejercicio_2_99)

media_sat_lengua <- mean(ejercicio_2_99$SAT_Lengua)
media_sat_mate <- mean(ejercicio_2_99$SAT_Matematicas)

desv_sat_lengua <- sd(ejercicio_2_99$SAT_Lengua)
desv_sat_mate <- sd(ejercicio_2_99$SAT_Matematicas)

r <- cor(ejercicio_2_99$SAT_Lengua,ejercicio_2_99$SAT_Matematicas)

pendiente <- r * (desv_sat_mate / desv_sat_lengua)

intercepto <- media_sat_mate - (pendiente * media_sat_lengua)

prediccion_455 <- intercepto + (pendiente * 455)

resultados <- data.frame(
  Medidas = c("media_sat_lengua","media_sat_mate","desv_sat_lengua","desv_sat_mate",
              "r","pendiente","intercepto","prediccion_455"),
  Valores = c(media_sat_lengua,media_sat_mate,desv_sat_lengua,desv_sat_mate,
              r,pendiente,intercepto,prediccion_455)
)

resultados$Valores <- round(resultados$Valores, 2)

print(resultados)

"
           Medidas Valores
1 media_sat_lengua  531.90
2   media_sat_mate  529.27
3  desv_sat_lengua   33.76
4    desv_sat_mate   34.83
5                r    0.97
6        pendiente    1.00
7       intercepto   -3.28
8   prediccion_455  452.28
"

# (b) Representa los residuos de tu regresión con relación a la calificación de
# Lengua en la prueba SAT (un programa estadístico lo puede hacer). Hay un Estado 
# que constituye una observación atípica, ¿cuál es? ¿Tiene dicho Estado una calificación 
# media de Matemáticas más alta o más baja que la que se hubiera predicho a partir 
# de su calificación media de Lengua?

ejercicio_2_99$Prediccion_SAT_Matematicas <- intercepto + pendiente * ejercicio_2_99$SAT_Lengua
ejercicio_2_99$Residuo_SAT_Matematicas <- ejercicio_2_99$SAT_Matematicas - ejercicio_2_99$Prediccion_SAT_Matematicas

indice_outlier <- which.max(abs(ejercicio_2_99$Residuo_SAT_Matematicas))

estado_outlier <- ejercicio_2_99$Estado[indice_outlier]

plot(ejercicio_2_99$SAT_Lengua,
     ejercicio_2_99$Residuo_SAT_Matematicas,
     main = "Desempeño SAT Lengua en relación a Matemáticas",
     xlab = "Puntajes SAT Lengua",
     ylab = "Residuo SAT Lengua",
     col = "blue",
     pch = 16)
abline(h = 0, col = "darkgreen", lty = 2)
points(ejercicio_2_99$SAT_Lengua[indice_outlier],
       ejercicio_2_99$Residuo_SAT_Matematicas[indice_outlier],
       pch = 16, col = "red", cex = 1.5)
text(ejercicio_2_99$SAT_Lengua[indice_outlier],
     ejercicio_2_99$Residuo_SAT_Matematicas[indice_outlier],
     labels = ejercicio_2_99$Estado[indice_outlier],  
     pos = 4,  
     col = "red")
grid()

comparacion_outlier <- data.frame(
  Estado = ejercicio_2_99$Estado[indice_outlier],
  Lengua_Real = ejercicio_2_99$SAT_Lengua[indice_outlier],
  Matematicas_Real = ejercicio_2_99$SAT_Matematicas[indice_outlier],
  Matematicas_Predicho = ejercicio_2_99$Prediccion_SAT_Matematicas[indice_outlier],
  Residuo = ejercicio_2_99$Residuo_SAT_Matematicas[indice_outlier]
)

print(comparacion_outlier)

"
  Estado Lengua_Real Matematicas_Real Matematicas_Predicho  Residuo
1     HI         485              510              482.315 27.68496

==========================================================================

El estado de Hawái (HI) constituye la observación atípica, con un residuo positivo de 27.68 puntos. 
Esto significa que Hawái tiene una calificación media de Matemáticas (510 puntos) significativamente 
más alta que la que se hubiera predicho a partir de su calificación de Lengua (482.32 puntos)."

# **************************************************
# PREGUNTA 2.100 - Aspirina y ataques al corazón
# **************************************************

# ¿Tomar aspirinas regularmente ayuda a prevenir los ataques al corazón? 
# Un estudio (Physicians’ Health Study) intentó averiguarlo, tomando como 
# sujetos a 22.071 médicos sanos que tenían al menos 40 años. La mitad de 
# los sujetos, seleccionados al azar, tomó una aspirina un día sí y otro no. 
# La otra mitad tomó un placebo, una píldora falsa que tenía el mismo aspecto 
# y sabor que una aspirina.

# TABLA DE RESULTADOS:
# ---------------------------------------------------------------
# Evento               | Grupo aspirina | Grupo placebo | Total
# ---------------------------------------------------------------
# Ataques mortales     | 10             | 26            | 36
# Otros ataques corazón| 129            | 213           | 342
# Embolias             | 119            | 98            | 217
# Total pacientes      | 11.037         | 11.034        | 22.071
# ---------------------------------------------------------------

# ¿Qué indican los datos sobre la relación que existe entre tomar aspirinas, y
# los ataques al corazón y las embolias? Utiliza porcentajes para hacer más precisos
# tus razonamientos. ¿Crees que el estudio proporciona suficiente evidencia de que
# las aspirinas reducen los ataques al corazón (relación causa-efecto)?

matrix_aspirina <- matrix(c(10,26,36,
                            129,213,342,
                            119,98,217,
                            11037,11034,22071),
                          nrow = 4, ncol = 3,
                          byrow = TRUE)
rownames(matrix_aspirina) = c("Ataques mortales","Otros ataques corazón","Embolias","Total pacientes")
colnames(matrix_aspirina) = c("Grupo aspirina","Grupo Placebo","Total")

print(matrix_aspirina)

prc_mortal_aspirina <- (matrix_aspirina[1,1] / matrix_aspirina[4,1]) * 100
prc_otros_aspirina <- (matrix_aspirina[2,1] / matrix_aspirina[4,1]) * 100
prc_embolia_aspirina <- (matrix_aspirina[3,1] / matrix_aspirina[4,1]) * 100

prc_mortal_placebo <- (matrix_aspirina[1,2] / matrix_aspirina[4,2]) * 100
prc_otros_placebo <- (matrix_aspirina[2,2] / matrix_aspirina[4,2]) * 100
prc_embolia_placebo <- (matrix_aspirina[3,2] / matrix_aspirina[4,2]) * 100


prc_condicionales <- data.frame(
  Evento = c("Ataques mortales","Otros ataques corazón","Embolias"),
  Grupo_aspirina = c(paste0(round(prc_mortal_aspirina,2),"%"),
                     paste0(round(prc_otros_aspirina,2),"%"),
                     paste0(round(prc_embolia_aspirina,2),"%")),
  Grupo_placebo = c(paste0(round(prc_mortal_placebo,2),"%"),
                    paste0(round(prc_otros_placebo,2),"%"),
                    paste0(round(prc_embolia_placebo,2),"%"))
)

print(prc_condicionales)

"
                 Evento Grupo_aspirina Grupo_placebo
1      Ataques mortales          0.09%         0.24%
2 Otros ataques corazón          1.17%         1.93%
3              Embolias          1.08%         0.89%

===================================================================

Los datos muestran que el grupo que tomó aspirinas presenta porcentajes notablemente menores 
tanto en ataques mortales como en otros ataques al corazón, lo que indica un efecto protector claro. 
En cambio, en el caso de las embolias, el porcentaje del grupo aspirina es apenas superior al del 
grupo placebo, pero la diferencia es pequeña y podría deberse al azar.

Dado que se trata de un estudio experimental con asignación aleatoria y una muestra muy grande, 
sí podemos afirmar que existe evidencia sólida de un efecto causal: tomar aspirinas reduce el riesgo 
de ataques al corazón
"

# **************************************************
# PREGUNTA 2.101 - Suicidios
# **************************************************

# He aquí una tabla de contingencia sobre los suicidios ocurridos en 1993, clasificados 
# según el sexo de la víctima y el método utilizado

# TABLA DE DATOS:
# ---------------------------------------------------------------
# Método        | Hombres   | Mujeres   
# ---------------------------------------------------------------
# Arma de fuego | 16.381    | 2.559     
# Veneno        | 3.569     | 2.110     
# Ahorcamiento  | 3.824     | 803       
# Otros         | 1.641     | 623       
# ---------------------------------------------------------------


# Basándote en estos datos, escribe un breve informe sobre las diferencias entre los
# suicidios de hombres y de mujeres. Asegúrate de que utilizas los recuentos y
# los porcentajes adecuados para justificar tus afirmaciones.

matrix_suicidios <- matrix(c(16381,2559,
                             3569,2110,
                             3824,803,
                             1641,623),
                           nrow = 4, ncol = 2,
                           byrow = TRUE)
colnames(matrix_suicidios) = c("Hombres","Mujeres")
rownames(matrix_suicidios) = c("Arma de fuego","Veneno","Ahorcamiento","Otros")

print(matrix_suicidios)

total_hombres <- sum(matrix_suicidios[1:4,1])

total_mujeres <- sum(matrix_suicidios[1:4,2])

total_poblacion <- total_hombres + total_mujeres

hombre_arma <- (matrix_suicidios[1,1] / total_hombres) * 100
hombre_veneno <- (matrix_suicidios[2,1] / total_hombres) * 100
hombre_ahorcamiento <- (matrix_suicidios[3,1] / total_hombres) * 100
hombre_otros <- (matrix_suicidios[4,1] / total_hombres) * 100

mujer_arma <- (matrix_suicidios[1,2] / total_mujeres) * 100
mujer_veneno <- (matrix_suicidios[2,2] / total_mujeres) * 100
mujer_ahorcamiento <- (matrix_suicidios[3,2] / total_mujeres) * 100
mujer_otro <- (matrix_suicidios[4,2] / total_mujeres) * 100

prc_condicional_suicidios <- data.frame(
  Evento = c("Arma de fuego","Veneno","Ahorcamiento","Otros"),
  Grupo_hombres = c(paste0(round(hombre_arma,2),"%"),
                    paste0(round(hombre_veneno,2),"%"),
                    paste0(round(hombre_ahorcamiento,2),"%"),
                    paste0(round(hombre_otros,2),"%")),
  Grupo_mujeres = c(paste0(round(mujer_arma,2),"%"),
                    paste0(round(mujer_veneno,2),"%"),
                    paste0(round(mujer_ahorcamiento,2),"%"),
                    paste0(round(mujer_otro,2),"%"))
  )

print(prc_condicional_suicidios)

"
         Evento Grupo_hombres Grupo_mujeres
1 Arma de fuego        64.45%        41.99%
2        Veneno        14.04%        34.62%
3  Ahorcamiento        15.05%        13.17%
4         Otros         6.46%        10.22%

=======================================================

En 1993 se observa una marcada diferencia en los métodos de suicidio según el sexo: aunque en 
ambos grupos el uso de armas de fuego es el método más frecuente, los hombres lo emplean proporcionalmente 
mucho más que las mujeres, mientras que ellas recurren al veneno en una proporción considerablemente mayor 
que los hombres. Estas diferencias muestran patrones claramente diferenciados en la elección del método entre 
ambos sexos.
"

# **************************************************
# PREGUNTA 2.102 - Permanecer vivo y fumar
# **************************************************

# A mediados de los años setenta, un estudio médico contactó al azar con gente 
# de un distrito de Inglaterra. He aquí los datos sobre 1.314 mujeres que eran 
# fumadoras habituales y mujeres que nunca habían fumado. La tabla clasifica a 
# estas mujeres según su edad en el momento inicial de realización del estudio, 
# según su situación con relación al tabaco y según si permanecían vivas al cabo 
# de 20 años.

# TABLAS DE DATOS POR GRUPO DE EDAD:
# ---------------------------------------------------------------
# De 18 a 44 años     | Fumadora | No fumadora
# ---------------------------------------------------------------
# Fallecidas          | 19       | 13
# Vivas               | 269      | 327
# ---------------------------------------------------------------
#
# De 45 a 64 años     | Fumadora | No fumadora
# ---------------------------------------------------------------
# Fallecidas          | 78       | 52
# Vivas               | 167      | 147
# ---------------------------------------------------------------
#
# Mayores de 65 años  | Fumadora | No fumadora
# ---------------------------------------------------------------
# Fallecidas          | 42       | 165
# Vivas               | 7        | 28
# ---------------------------------------------------------------

# (a) A partir de estos datos, construye una sola tabla de contingencia que rela-
#     cione fumar (sí o no) con fallecer o vivir. ¿Qué porcentaje de fumadoras perma-
#     neció con vida durante 20 años? ¿Qué porcentaje de no fumadoras sobrevivió?
#     Parece sorprendente que el porcentaje de mujeres que permaneció con vida fuera
#     mayor entre las fumadoras.

# (b) La edad de la mujer en el momento inicial de realización del estudio es
#     una variable latente. Muestra que dentro de cada uno de los tres grupos de edad,
#     el porcentaje de mujeres que permaneció con vida después de 20 años fue mayor
#     entre las no fumadoras. Estamos ante otro ejemplo de la Paradoja de Simpson.

# (c) Los autores del estudio dieron la siguiente explicación: “Entre las mujeres
#     mayores (de 65 o más años al inicio del estudio), pocas eran fumadoras; sin em-
#     bargo, muchas de ellas murieron durante el tiempo de seguimiento del estudio”.
#     Compara el porcentaje de fumadoras en cada uno de los tres grupos de edad para
#     verificar esta explicación.

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::
