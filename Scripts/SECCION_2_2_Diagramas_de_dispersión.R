# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO II "Análisis de relaciones"

# SECCIÓN 2.2 "Diagramas de dispersión"

# **************************************************
# PREGUNTA 2.4 - Manatíes en peligro
# **************************************************

# Los manatíes son unos animales grandes y dóciles que viven a lo largo de la 
# costa de Florida. Cada año, lanchas motoras hieren o matan muchos manatís. 
# A continuación, se presenta una tabla que contiene el número de licencias para 
# lanchas motoras (expresado en miles de licencias por año) expedidas en Florida 
# y el número de manatís muertos entre los años 1977 y 1990.

# Año  Licencias (1.000)  Manatís muertos
# ---------------------------------------
# 1977       447                13
# 1978       460                21
# 1979       481                24
# 1980       498                16
# 1981       513                24
# 1982       512                20
# 1983       526                15
# 1984       559                34
# 1985       585                33
# 1986       614                33
# 1987       645                39
# 1988       675                43
# 1989       711                50
# 1990       719                47

# (a) Queremos analizar la relación entre el número de licencias anualmente
# expedidas en Florida y el número de manatís muertos cada año. 
# ¿Cuál es la variable explicativa?

"La variable explicativa es el numero de licencias expedidas por año en Florida"

# (b) Dibuja un diagrama de dispersión con estos datos. (Indica en los ejes los
# nombres de las variables, no te limites a indicar x e y.) 
# ¿Qué nos dice el diagrama de dispersión sobre la relación entre estas dos variables?

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

ejercicio_2_04 <- read.csv("ejercicio_2_04_manaties.csv")

head(ejercicio_2_04)

str(ejercicio_2_04)

plot(ejercicio_2_04$Licencias_miles,
     ejercicio_2_04$Manatis_muertos,
     main = "Relación entre licencias expedidas y manatíes muertos",
     xlab = "Licencias expedidas (x 1000)",
     ylab = "Manatíes muertos",
     col = "blue")
grid()

"El diagrama nos muestra que a mayor cantidad de licencias expedidas, mayor es la cantidad de manatíes muertos"

# **************************************************
# PREGUNTA 2.5 - Más sobre manatíes en peligro
# **************************************************

# En el ejercicio 2.4 dibujaste un diagrama de dispersión del número de licencias 
# para lanchas motoras registradas anualmente en Florida y del número de manatís 
# que matan las lanchas cada año.

# (a) Describe la dirección de la relación. Las variables, ¿están asociadas positiva
# o negativamente?

"La dirección del diagrama muestra una asociación positiva"

# (b) Describe la forma de la relación. ¿Es lineal?

"La forma de la relación es aproximadamente lineal"

# (c) Describe la fuerza de la relación. ¿Se puede predecir con precisión el nú
# mero de manatís muertos cada año conociendo el número de licencias expedidas
# en ese año? Si Florida decidiera congelar el número de licencias en 716.000, ¿cuán
# tos manatís matarían, aproximadamente, las lanchas motoras cada año?

"La fuerza de la relación es bastante clara, a mayor cantidad de licencias, mayor e sla cantidad de
manatíes muertos. Con 716.000 lanchas registradas, esperamos la muerte de 50 manatís al año."

# **************************************************
# PREGUNTA 2.6 - El consumo, ¿aumenta con la velocidad?
# **************************************************

# ¿Cómo varía el consumo de gasolina de un coche a medida que aumenta su velocidad? 
# Aquí se presentan los datos correspondientes al modelo británico del Ford Escort. 
# La velocidad se ha medido en kilómetros por hora y el consumo de carburante en 
# litros de gasolina por 100 kilómetros.

# Velocidad (km/h)  Consumo (litros/100km)
# -----------------------------------------
# 10                21,00
# 20                13,00
# 30                10,00
# 40                8,00
# 50                7,00
# 60                5,90
# 70                6,30
# 80                6,95
# 90                7,57
# 100               8,27
# 110               9,03
# 120               9,87
# 130               10,79
# 140               11,77
# 150               12,83

# (a) Dibuja un diagrama de dispersión. ¿Cuál es la variable explicativa?

ejercicio_2_06 <- read.csv("ejercicio_2_06_consumo_coche.csv")

head(ejercicio_2_06)

str(ejercicio_2_06)

plot(ejercicio_2_06$Velocidad_km_h,
     ejercicio_2_06$Consumo_litros_100km,
     main = "Consumo modelo Ford Escort",
     xlab = "Velocidad (km/h)",
     ylab = "Consumo (litros / 100 km)",
     col = "red",
     pch = 16,
     cex = 1.2)
grid()

# (b) Describe la forma de la relación. ¿Por qué no es lineal? Explica lo que
# indica la forma de la relación.

"Tiene una forma curva, lo que significa que tiene un mayor consumo tanto en una velocidad 
lenta como a una mayor velocidad"

# (c) ¿Por qué no tiene sentido decir que las variables están asociadas positiva
# o negativamente?

"No tiene sentido, ya que la curva expresa que el mayor consumo es a velocidades lentas como 
a altas velocidad, sin embargo sebemos considar como OA los resultados a baja velocidad"

# (d) La relación, ¿es razonablemente fuerte o, por el contrario, es más bien
# débil? Justifica tu respuesta.

"Al ser claramente una curva, la relación entre las variables es fuerte, lo que puede indicar que 
a hay una estabilidad entre velocidad y consumo entre los 80 a 100 kilometros por hora"

# **************************************************
# PREGUNTA 2.7 - La gente obesa, ¿consume más energía?
# **************************************************

# El nivel metabólico de una persona, es decir, el ritmo al que su cuerpo consume 
# energía, es un factor importante a tener en cuenta en estudios de dietética. 
# La tabla 2.3 proporciona datos sobre el sexo, el peso magro (peso total descontando 
# su contenido en grasa) y el nivel metabólico en reposo de 12 mujeres y 7 hombres 
# que eran los sujetos de un estudio de dietética. El nivel metabólico se expresa 
# en calorías consumidas en 24 horas, la misma unidad utilizada para expresar el 
# valor energético de los alimentos. Los investigadores creen que el peso magro 
# corporal tiene una importante influencia en el nivel metabólico.

# Sujeto  Sexo  Peso (kg)  Nivel metabólico
# -----------------------------------------
# 1       H     62,0      1.792
# 2       H     62,9      1.666
# 3       M     36,1      0.995
# 4       M     54,6      1.425
# 5       M     48,5      1.396
# 6       M     42,0      1.418
# 7       H     47,4      1.362
# 8       M     50,6      1.502
# 9       M     42,0      1.256
# 10      H     48,7      1.614
# 11      M     40,3      1.189
# 12      M     33,1      0.913
# 13      H     51,9      1.460
# 14      M     42,4      1.124
# 15      M     34,5      1.052
# 16      M     51,1      1.347
# 17      M     41,2      1.204
# 18      H     51,9      1.867
# 19      H     46,9      1.439

# (a) Dibuja un diagrama de dispersión sólo con los datos de las mujeres. ¿Cuál
# sería la variable explicativa?

ejercicio_2_07 <- read.csv("ejercicio_2_07_peso.csv")

head(ejercicio_2_07)

str(ejercicio_2_07)

peso_mujeres <- ejercicio_2_07[ejercicio_2_07$Sexo == "M", ]

head(peso_mujeres)

plot(peso_mujeres$Peso_kg,
     peso_mujeres$Nivel_metabolico,
     main = "Relación entre peso y nivel metabólico del grupo mujer",
     xlab = "Peso (kg)",
     ylab = "Nivel Metabolico",
     col = "green",
     pch = 16,
     cex = 1.2)
grid()

"En este caso la variable explicativa es el peso expresado en kilos"

# (b) La asociación entre estas dos variables, ¿es positiva o negativa? ¿Cuál es
# la forma de la relación? ¿Cuál es la fuerza de la relación?

"Presenta una asociación positiva,lo que significa que a mayor peso, mayor en el nivel metabólico.
Tiene una forma lineal, lo que confirma la hipótesis, y tiene fuerza la relación"

# (c) Ahora, añade en el diagrama de dispersión los datos de los hombres utilizando 
# un color o un símbolo distinto al utilizado para las mujeres. La relación
# entre el nivel metabólico y el peso magro de los hombres, ¿es igual al de las mujeres? 
# ¿En qué se distinguen el grupo de hombres y el grupo de mujeres?

peso_hombres <- ejercicio_2_07[ejercicio_2_07$Sexo == "H", ]

plot(peso_mujeres$Peso_kg,
     peso_mujeres$Nivel_metabolico,
     main = "Relación entre peso y nivel metabólico del grupo mujer",
     xlab = "Peso (kg)",
     ylab = "Nivel Metabolico",
     col = "green",
     pch = 16,
     cex = 1.5,
     xlim = range(ejercicio_2_07$Peso_kg),
     ylim = range(ejercicio_2_07$Nivel_metabolico))
points(peso_hombres$Peso_kg,
       peso_hombres$Nivel_metabolico,
       col = "blue",
       pch = 17,
       cex = 1.5)
grid()

"Entre los hombres, la relación es básicamente la misma, pero con una mayor dispersión 
(la fuerza es menor). En general, los valores de las dos variables son mayores entre los hombres."

# **************************************************
# PREGUNTA 2.8 - Inteligencia y calificaciones escolares
# **************************************************

# Los estudiantes que tienen coeficientes de inteligencia mayores, ¿tienden a ser 
# mejores en la escuela? La figura 2.5 es un diagrama de dispersión correspondiente
# a las calificaciones medias escolares y a los coeficientes de inteligencia de 78 
# estudiantes de primero de bachillerato en una escuela rural.

# (a) Explica en palabras qué significaría una asociación positiva entre el coeficiente 
# de inteligencia y la nota media escolar. El diagrama, ¿muestra una asociación positiva?

install.packages("magick")
library(magick)

figura_2_05 <- image_read("figura_2_05.png")

print(figura_2_05)

"Vemos que el diagrama tiene una dirección de asociación positiva, a mayor CI mejores
son las notas de los alumnos"

# (b) ¿Cuál es la forma de la relación? ¿Es aproximadamente lineal? ¿Es una
# relación muy fuerte? Justifica tus respuestas.

"La forma de la realción es aproximadamente lineal, sin embargo hay observaciones atípicas
que debilitan la fuerza de la relación, con lo cual podríamos cuestionar la hipotesis
inicial"

# (c) En la parte baja del diagrama aparecen algunos puntos que podríamosllamar observaciones atípicas. 
# En concreto, un estudiante tiene una nota escolar muy baja, a pesar de tener un coeficiente de inteligencia medio. 
# ¿Cuáles son, de forma aproximada, el coeficiente de inteligencia y la nota media escolar de este estudiante?

"De forma aproximada tiene un coeficiente de 105 puntos y un rendimiento medio de 0.5, lo que puede significar
una mala manipulación de los datos o una escala de rendición diferente al resto"

# **************************************************
# PREGUNTA 2.9 - Calorías y sal en salchichas
# **************************************************

# Las salchichas con un contenido alto en calorías, ¿tienen también un contenido 
# alto en sal? La figura 2.6 es un diagrama de dispersión que relaciona las calorías 
# con el contenido en sal (expresado en miligramos de sodio) de 17 marcas distintas 
# de salchichas elaboradas con carne de ternera.

# (a) Di de manera aproximada cuáles son los valores máximo y mínimo del contenido 
# en calorías de las distintas marcas. De forma aproximada, ¿cuáles son los contenidos 
# de sal de las marcas con más y con menos calorías?

figura_2_06 <- image_read("figura_2_06.png")

print(figura_2_06)

"Los valores mínimo y máximo del contenido de calorías son 105 y 200.El contenido de
sal de las marcas con menos calores es 150"

# (b) El diagrama de dispersión, ¿muestra alguna asociación positiva o negativa
# clara? Explica con palabras el significado de esta asociación.

"El diagrama muestra una relación de asociación positiva, significa que a mayor cantidad
de calorias, mayor es la cantidad de sal"

# (c) ¿Has identificado alguna observación atípica? Prescindiendo de las posibles 
# observaciones atípicas, ¿existe una relación lineal entre estas variables? Si
# ignoras las observaciones atípicas, ¿crees que existe una asociación fuerte entre
# ambas variables?

"Aún si no prescindieramos de la que podria ser la unica OA, la fuerza de la relación
es bastante clara hacia una forma lineal, lo que confirma la hipótesis de que una mayor
cantidad de calorias tiene una mayor cantidad de sal"

# **************************************************
# PREGUNTA 2.10 - Estados ricos y Estados pobres
# **************************************************

# Una medida de la riqueza de un Estado es la mediana de ingresos por hogar. 
# Otra medida de riqueza es la media de ingresos por persona. La figura 2.7 es 
# un diagrama de dispersión que relaciona estas dos variables en EE UU. Ambas 
# variables se expresan en miles de dólares. Debido a que las dos variables se 
# expresan en las mismas unidades, la separación entre unidades es la misma en 
# ambos ejes.

# (a) En el diagrama de dispersión, hemos señalado el punto correspondiente a Nueva York. 
# ¿Cuáles son, aproximadamente, los valores de la mediana de ingresos por hogar y la media 
# de ingresos por persona?

figura_2_07 <- image_read("figura_2_07.png")

print(figura_2_07)

"
Mediana de ingresos por hogar: US$ 34.000
Media de ingresos por persona: US$ 27.000
"
# (b) Explica por qué esperamos que haya una asociación positiva entre estas variables. 
# Explica también, por qué esperamos que los ingresos por hogar sean mayores que los 
# ingresos por persona.

"Esperamos una asociación positiva porque estados más ricos tienen mayores ingresos 
tanto por hogar como por persona, y los ingresos por hogar son mayores porque usualmente 
incluyen múltiples perceptores de ingresos."

# (c) Sin embargo, en un determinado Estado, la media de los ingresos por persona puede 
# ser mayor que la mediana de ingresos por hogar. De hecho, el Distrito de Columbia tiene 
# una mediana de ingresos por hogar de 30.748 $ y una media de ingresos por persona de 33.435 $. 
# Explica por qué esto puede ocurrir.

"Esto puede ocurrir debido a una alta desigualdad de ingresos, donde pocas personas con salarios 
muy altos elevan la media personal por encima de la mediana familiar."

# (d) Alaska es el Estado con la mediana de ingresos por hogar mayor. ¿Cuál es aproximadamente 
# su mediana de ingresos por hogar? Podemos considerar Alaska y el Distrito de Columbia 
# observaciones atípicas.

"Alaska tiene la mediana de ingresos por hogar más alta, aproximadamente entre 48.000 
y 50.000 dólares, siendo considerado un valor atípico junto con el Distrito de Columbia."

# (e) Obviando las observaciones atípicas, describe la forma, la dirección y la
# fuerza de la relación.

"Obviando los atípicos, la relación es lineal, positiva y fuerte, mostrando que ambas 
variables aumentan juntas de manera consistente en la mayoría de los estados."

# **************************************************
# PREGUNTA 2.11 - El vino, ¿es bueno para tu corazón?
# **************************************************

# Existe alguna evidencia de que tomar vino con moderación ayuda a prevenir 
# los ataques al corazón. La tabla 2.4 proporciona datos sobre el consumo de vino 
# (en litros de alcohol, procedente del vino, por cada 100.000 personas) y sobre 
# las muertes anuales por ataques al corazón (muertes por cada 100.000 personas) 
# en 19 países desarrollados.


# País            Consumo de alcohol*  Tasa de muertes por ataques al corazón**
# ------------------------------------------------------------
# Alemania        2,7                  172
# Australia       2,5                  211
# Austria         3,9                  167
# Bélgica/Lux.    2,9                  131
# Canadá          2,4                  191
# Dinamarca       2,9                  220
# España          6,5                  86
# EEUU            1,2                  199
# Finlandia       0,8                  297
# Francia         9,1                  71
# Holanda         1,8                  167
# Irlanda         0,7                  300
# Islandia        0,8                  211
# Italia          7,9                  107
# Noruega         0,8                  227
# N.Zelanda       1,9                  266
# ReinoUnido      1,3                  285
# Suecia          1,6                  207
# Suiza           5,8                  115

# (a) Dibuja un diagrama de dispersión que muestre cómo el consumo nacional
# de vino ayuda a explicar las muertes por ataques al corazón.

ejercicio_2_11 <- read.csv("ejercicio_2_11_alcohol.csv")

head(ejercicio_2_11)

str(ejercicio_2_11)

plot(ejercicio_2_11$Consumo_alcohol,
     ejercicio_2_11$Tasa_muertes_corazon,
     main = "El vino, ¿es bueno para tu corazón?",
     xlab = "Consumo de alcohol (en litros de alcohol)",
     ylab = "Tasa de muertes (por cada 100.000 habitantes)",
     pch = 16,
     cex = 1.5)
grid()

# (b) Describe la forma de la relación. ¿Existe una relación lineal? ¿Es una rela
# ción fuerte?

"Hay una relación lineal y fuerte entre las variables counsumo de alcohol y tasa de muertes
por ataques de corazón"

# (c) La dirección de la asociación, ¿es positiva o negativa? Explica de forma llana 
# qué dice la relación sobre el consumo de vino y los ataques al corazón. Estos datos, 
#¿proporcionan una clara evidencia de que tomar vino causa una reducciónde las muertes 
# por ataques al corazón? ¿Por qué?

"La relación entre las variables es negativa, lo que significa que el alto consumo de alcohol no influye
en la tasa de muertes por ataques al corazón"

# **************************************************
# PREGUNTA 2.12 - El profesor Moore y la natación
# **************************************************

# El profesor Moore nada 1.800 metros de forma regular. Un intento inútil de 
# contrarrestar el paso de los años. He aquí los tiempos (en minutos) y su ritmo 
# cardíaco después de nadar (en pulsaciones por minuto) en 23 sesiones de natación.

# Minutos:   34,12 35,72 34,72 34,05 34,13 35,72 36,17 35,57 35,37 35,57 35,43 
#            36,05 34,85 34,70 34,75 33,93 34,60 34,00 34,35 35,62 35,68 35,28 35,97

# Pulsaciones: 152 124 140 152 146 128 136 144 148 144 136 124 148 144 140 156 
#              136 148 148 132 124 132 139

# (a) Dibuja un diagrama de dispersión. (¿Cuál es la variable explicativa?)

ejercicio_2_12 <- read.csv("ejercicio_2_12_tiempos.csv",sep = ";")

head(ejercicio_2_12)

plot(ejercicio_2_12$Minutos,
     ejercicio_2_12$Pulsaciones,
     main = "El profesor Moore y la natación",
     xlab = "Tiempo registrado (en minutos)",
     ylab = "Ritmo cardíaco (en pulsaciones por minuto)",
     pch = 16,
     cex = 1.5)
grid()

"En este caso la variable explicativa en el tiempo registrado, ya que lo que se busca es verla cantidad
de pulsaciones por minutos de los 23 registros"

# (b) La asociación entre estas variables, ¿es positiva o negativa? Explica por
# qué crees que la relación va en este sentido.

"Cuando el tiempo que tarda Moore en nadar aumenta, el número de pulsaciones tiende a disminuir.
Es decir, la relación es negativa: a mayor tiempo (más lento nada), menor ritmo cardíaco.
Si nada más despacio (más minutos en completar la distancia), su esfuerzo es menor, 
por lo tanto su corazón late más lento al finalizar."

# (c) Describe la forma y la fuerza de la relación.

"Forma: la nube de puntos parece bastante alineada de forma aproximadamente 
lineal (aunque con cierta dispersión).

Fuerza: la relación es moderada a fuerte. No es una correlación perfecta, 
pero sí clara: los puntos siguen una tendencia descendente bastante definida."

# **************************************************
# PREGUNTA 2.13 - ¿Qué densidad de siembra es excesiva?
# **************************************************

# ¿Cuál debe ser la densidad de siembra del maíz para que un agricultor obtenga 
# el máximo rendimiento? Para determinar la densidad de siembra óptima, se hace 
# un experimento que consiste en sembrar plantas de maíz a distintas densidades 
# de siembra en parcelas de fertilidad similar. Los rendimientos obtenidos son 
# los siguientes:

# Densidad de siembra (plantas por hectárea)  Rendimiento (toneladas por hectárea)
# --------------------------------------------------------------------------------
# 30.000                                        10,1  7,6  7,9  9,6
# 40.000                                        11,2  8,1  9,1  10,1
# 50.000                                        11,1  8,7  9,4  10,1
# 60.000                                        9,1  9,3  10,5
# 70.000                                        8,0  10,1

# (a) ¿Cuál es la variable explicativa: el rendimiento o la densidad de siembra?

"En este caso la densidad de la siembra es la variable explicativa"

# (b) Dibuja un diagrama de dispersión con los datos del rendimiento y de la
# densidad de siembra.

resultados_rendimientos <- data.frame(
  Densidad_siembra = c(rep(30000, 4), rep(40000, 4), rep(50000, 4), rep(60000, 3), rep(70000, 2)),
  Rendimiento = c(10.1, 7.6, 7.9, 9.6, 11.2, 8.1, 9.1, 10.1, 11.1, 8.7, 9.4, 10.1, 9.1, 9.3, 10.5, 8.0, 10.1)
)

print(resultados_rendimientos)

plot(resultados_rendimientos$Densidad_siembra,
     resultados_rendimientos$Rendimiento,
     main = "¿Qué densidad de siembra es excesiva?",
     xlab = "Densidad de la siembra (plantas por hectárea)",
     ylab = "Rendimiento (toneladas por hectárea)",
     pch = 16, 
     col = "grey")
grid()

# (c) Describe el aspecto general de la relación. ¿Es una relación lineal? ¿Existe
# una asociación positiva, negativa o ninguna de las dos?

"La relación entre la densidad de siembra y el rendimiento no es lineal, 
sino curvilínea: el rendimiento aumenta a medida que la densidad pasa de 30.000 a 50.000 
plantas por hectárea, alcanza su punto máximo alrededor de 50.000, y luego disminuye 
cuando la densidad se incrementa más allá de ese valor; por lo tanto, no puede describirse 
como una asociación exclusivamente positiva ni negativa."

# (d) Calcula los rendimientos medios de cada una de las densidades de siembra. 
# Dibuja un diagrama de dispersión que relacione estas medias con la densidad
# de siembra. Une las medias con segmentos para facilitar la interpretación del
# diagrama. ¿Qué densidad de siembra recomendarías a un agricultor que quisiera
# sembrar maíz en un campo de fertilidad similar a la del experimento?

resultados_rendimientos$Promedio <- ave(resultados_rendimientos$Rendimiento,
                                        resultados_rendimientos$Densidad_siembra,
                                        FUN = mean)

print(resultados_rendimientos)

write.csv(resultados_rendimientos, "ejercicio_2_13_siembra.csv", row.names = FALSE)

plot(resultados_rendimientos$Densidad_siembra,
     resultados_rendimientos$Promedio,
     main = "¿Qué densidad de siembra es excesiva?",
     xlab = "Densidad de la siembra (plantas por hectárea)",
     ylab = "Promedio de rendimiento",
     pch = 16, 
     col = "blue")
grid()

"Recomendaría aquel que tiene una densidad de 50000 plantas por hectárea"


# **************************************************
# PREGUNTA 2.14 - Salario de profesores
# **************************************************

# La tabla 2.1 muestra datos sobre la educación en EE UU. Es posible que los 
# Estados con un nivel educativo menor paguen menos a sus profesores. Esto se 
# podría explicar por el hecho de que son más pobres.

# Estado*  Región**  Población(1.000)  SAT_Lengua  SAT_Matemáticas  %alumnos_presentados  %sin_estudios_secundaria  Salario_profesores($1.000)
# -------------------------------------------------------------------------------------------------------------------------------------------------
# AL       ESC       4.273              565         558              8                      33.1                      31.3
# AK       PAC       607                521         513              47                     13.4                      49.6
# AZ       MTN       4.428              525         521              28                     21.3                      32.5
# AR       WSC       2.510              566         550              6                      33.7                      29.3
# CA       PAC       31.878             495         511              45                     23.8                      43.1
# CO       MTN       3.823              536         538              30                     15.6                      35.4
# CT       NE        3.274              507         504              79                     20.8                      50.3
# DE       SA        725                508         495              66                     22.5                      40.5
# DC       SA        543                489         473              50                     26.9                      43.7
# FL       SA        14.400             498         496              48                     25.6                      33.3
# GA       SA        7.353              484         477              63                     29.1                      34.1
# HI       PAC       1.184              485         510              54                     19.9                      35.8
# ID       MTN       1.189              543         536              15                     20.3                      30.9
# IL       ENC       11.847             564         575              14                     23.8                      40.9
# IN       ENC       5.841              494         494              57                     24.4                      37.7
# IA       WNC       2.852              590         600              5                      19.9                      32.4
# KS       WNC       2.572              579         571              9                      18.7                      35.1
# KY       ESC       3.884              549         544              12                     35.4                      33.1
# LA       WSC       4.351              559         550              9                      31.7                      26.8
# ME       NE        1.243              504         498              68                     21.2                      32.9
# MD       SA        5.072              507         504              64                     21.6                      41.2
# MA       NE        6.092              507         504              80                     20.0                      42.9
# MI       ENC       9.594              557         565              11                     23.2                      44.8
# MN       WNC       4.658              582         593              9                      17.6                      36.9
# MS       ESC       2.716              569         557              4                      35.7                      27.7
# MO       WNC       5.359              570         569              9                      26.1                      33.3
# MT       MTN       879                546         547              21                     19.0                      29.4
# NE       WNC       1.652              567         568              9                      18.2                      31.5
# NV       MTN       1.603              508         507              31                     21.2                      36.2
# NH       NE        1.162              520         514              70                     17.8                      35.8
# NJ       MA        7.988              498         505              69                     23.3                      47.9
# NM       MTN       1.713              554         548              12                     24.9                      29.6
# NY       MA        18.185             497         499              73                     25.2                      48.1
# NC       SA        7.323              490         486              59                     30.0                      30.4
# ND       WNC       644                596         599              5                      23.3                      27.0
# OH       ENC       11.173             536         535              24                     24.3                      37.8
# OK       WSC       3.301              566         557              8                      25.4                      28.4
# OR       PAC       3.204              523         521              50                     18.5                      39.6
# PA       MA        12.056             498         492              71                     25.3                      46.1
# RI       NE        990                501         491              69                     28.0                      42.2
# SC       SA        3.699              480         474              57                     31.7                      31.6
# SD       WNC       732                574         566              5                      22.9                      26.3
# TN       ESC       5.320              563         552              14                     32.9                      33.1
# TX       WSC       19.128             495         500              48                     27.9                      32.0
# UT       MTN       2.000              583         575              4                      14.9                      30.6
# VT       NE        589                506         500              70                     19.2                      36.3
# VA       SA        6.675              507         496              68                     24.8                      35.0
# WA       PAC       5.533              519         519              47                     16.2                      38.0
# WV       SA        1.826              526         506              17                     34.0                      32.2
# WI       ENC       5.160              577         586              8                      21.4                      38.2
# WY       MTN       481                544         544              11                     17.0                      31.6

# * Estados de EE.UU.
# ** Regiones: ESC (Este Centro Sur), PAC (Pacífico), MTN (Montaña), WSC (Oeste Centro Sur), 
#    NE (Noreste), SA (Sur Atlántico), ENC (Este Centro Norte), WNC (Oeste Centro Norte), MA (Mid-Atlantic)

# (a) Dibuja un diagrama de dispersión que relacione la media de los salarios
# de los profesores y el porcentaje de residentes que no tienen una carrera univer
# sitaria. Considera esta última variable como explicativa.

ejercicio_2_14 <- read.csv("ejercicio_2_14_educacion.csv")

str(ejercicio_2_14)

View(ejercicio_2_14)

plot(ejercicio_2_14$Porcentaje_sin_estudios_secundaria,
     ejercicio_2_14$Salario_profesores_miles,
     main = "Salario de profesores",
     xlab = "Porcentaje sin estudios (población x 100.000)",
     ylab = "Salario de profesores (US$1000)",
     pch = 16, 
     col = "green")
grid()

# (b) El diagrama muestra una asociación negativa débil entre las dos variables.
# ¿Por qué decimos que la relación es negativa? ¿Por qué decimos que es débil?

"Decimos que es negativa porque los valores son inferiores a la media, en este caso a medida
que aumenta el porcentaje de población sin estudios, disminuye el promedio de salario de los profesores.

Decimos que es débil, debido a que la dispersión entre los datos no muestra una relación lineal clara entre
las variables, por lo que que hay muchos Estados que presentan un aumento en el procentaje de personas sin estudio
pero dedican una inversión sustancial al salario de profesores
"
# (c) En la parte superior izquierda de tu diagrama hay una observación atípica.
# ¿A qué Estado corresponde?

estados_menor_porcentaje <- ejercicio_2_14[ejercicio_2_14$Porcentaje_sin_estudios_secundaria <= 15 
                                           & ejercicio_2_14$Salario_profesores_miles >= 45, ]

print(estados_menor_porcentaje[, c("Estado","Porcentaje_sin_estudios_secundaria","Salario_profesores_miles")])

"
Estado Porcentaje_sin_estudios_secundaria Salario_profesores_miles
    AK                               13.4                     49.6
"

# (d) Existe un grupo bastante claro formado por nueve Estados en la parte inferior 
# derecha del diagrama. Estos Estados tienen muchos residentes que no se
# graduaron en una escuela secundaria y además los salarios de los profesores son
# bajos. ¿Qué Estados son? ¿Se sitúan en alguna parte concreta del país?

estados_menor_salario <- ejercicio_2_14[ejercicio_2_14$Porcentaje_sin_estudios_secundaria >=30
                                        & ejercicio_2_14$Salario_profesores_miles <= 35, ]

print(estados_menor_salario[,c("Estado","Region","Porcentaje_sin_estudios_secundaria","Salario_profesores_miles")])

"   
   Estado Region Porcentaje_sin_estudios_secundaria Salario_profesores_miles
1      AL    ESC                               33.1                     31.3
4      AR    WSC                               33.7                     29.3
18     KY    ESC                               35.4                     33.1
19     LA    WSC                               31.7                     26.8
25     MS    ESC                               35.7                     27.7
34     NC     SA                               30.0                     30.4
41     SC     SA                               31.7                     31.6
43     TN    ESC                               32.9                     33.1
49     WV     SA                               34.0                     32.2

Muchos de estos Estados se encuentran en el centro del país
"

# **************************************************
# PREGUNTA 2.15 - Transformación de datos
# **************************************************

# Al analizar datos, a veces conviene hacer una transformación de datos que 
# simplifique el aspecto general de la relación. A continuación se presenta un 
# ejemplo de cómo transformando la variable respuesta se puede simplificar el 
# aspecto del diagrama de dispersión. La población europea entre los años 1750 
# y 1950 creció de la siguiente manera:

# Año          Población (millones)
# --------------------------------
# 1750         125
# 1800         187
# 1850         274
# 1900         423
# 1950         594

# (a) Dibuja el diagrama de dispersión correspondiente a estos datos. Describe
# brevemente el tipo de crecimiento en el periodo señalado.

crecimiento_poblacion <- data.frame(
  Año = c(1750,1800,1850,1900,1950),
  Poblacion = c(125,187,274,423,594)
)

print(crecimiento_poblacion)

plot(crecimiento_poblacion$Año,
     crecimiento_poblacion$Poblacion,
     main = "La población europea entre los años 1750 y 1950",
     xlab = "Año",
     ylab = "Población (millones)",
     pch = 16, 
     col = "green")
grid()

"El diagrama presenta una relación lineal positiva fuerte, lo que significa que conforme 
pasan los años aumenta la cantidad de personas que residen en Europa"

# (b) Calcula los logaritmos de la población de cada uno de los años (puedes
# utilizar tu calculadora). Dibuja un nuevo diagrama de dispersión con la variable
# población transformada. ¿Qué tipo de crecimiento observas ahora?

crecimiento_poblacion$log_natural <- log(crecimiento_poblacion$Poblacion)

crecimiento_poblacion$log_base10 <- log10(crecimiento_poblacion$Poblacion)

write.csv(crecimiento_poblacion, "ejercicio_2_15_poblacion.csv", row.names = FALSE)


plot(crecimiento_poblacion$Año,
     crecimiento_poblacion$log_natural,
     main = "La población europea entre los años 1750 y 1950",
     xlab = "Año",
     ylab = "Población (millones)",
     pch = 19, 
     col = "blue", 
     cex = 1.5, 
     ylim = c(0, 600))
points(crecimiento_poblacion$Año, crecimiento_poblacion$log_natural * 100, 
       pch = 17, col = "red", cex = 1.5)
points(crecimiento_poblacion$Año, crecimiento_poblacion$log_base10 * 150,   
       pch = 15, col = "darkgreen", cex = 1.5)
legend("topright", 
       legend = c("Población Original", "ln(Población) × 100", "log10(Población) × 150"),
       pch = c(19, 17, 15), 
       col = c("blue", "red", "darkgreen"),
       bty = "n",
       cex = 0.5,          
       pt.cex = 0.6,       
       text.width = 20)
grid()

# **************************************************
# PREGUNTA 2.16 - Variable categórica explicativa
# **************************************************

# Un diagrama de dispersión muestra la relación entre dos variables cuantitativas. 
# Vamos a ver un gráfico similar en el que la variable explicativa será una variable 
# categórica en vez de una cuantitativa.

# La presencia de plagas (insectos nocivos) en los cultivos se puede determinar
# con la ayuda de trampas. Una de ellas consiste en una lámina de plástico de dis
# tintos colores que contiene en su superficie un material pegajoso. ¿Qué colores 
# atraen más a los insectos? Para responder a esta pregunta un grupo de investiga
# dores llevó a cabo un experimento que consistió en situar en un campo de avena
# 24 trampas de las cuales había 6 de color amarillo, 6 blancas, 6 verdes y 6 azules.

# Color de la trampa  Insectos capturados
# ---------------------------------------
# Amarillo            45  59  48  46  38  47
# Blanco              21  12  14  17  13  17
# Verde               37  32  15  25  39  41
# Azul                16  11  20  21  14  7

# (a) Dibuja un gráfico que relacione los recuentos de insectos capturados con
#     el color de la trampa (sitúa el color de las trampas a distancias iguales en el eje
#     de las abscisas). Calcula las medias de insectos atrapados en cada tipo de trampa,
#     añádelas al gráfico y únelas con segmentos.

insectos <- data.frame(
  Color = rep(c("Amarillo", "Blanco", "Verde", "Azul"), each = 6),
  Capturas = c(
    45, 59, 48, 46, 38, 47,     
    21, 12, 14, 17, 13, 17,     
    37, 32, 15, 25, 39, 41,     
    16, 11, 20, 21, 14, 7       
  )
)

str(insectos)

print(insectos)

insectos$Color <- factor(insectos$Color, levels = c("Amarillo", "Blanco", "Verde", "Azul"))


posiciones <- as.numeric(insectos$Color)

media_amarillo <- mean(insectos$Capturas[insectos$Color == "Amarillo"])
media_blanco   <- mean(insectos$Capturas[insectos$Color == "Blanco"])
media_verde    <- mean(insectos$Capturas[insectos$Color == "Verde"])
media_azul     <- mean(insectos$Capturas[insectos$Color == "Azul"])

medias <- c(media_amarillo, media_blanco, media_verde, media_azul)

plot(posiciones, insectos$Capturas,
     main = "Capturas por color de trampa",
     xlab = "Color de la trampa", 
     ylab = "Insectos capturados",
     xaxt = "n",
     col = "forestgreen",
     pch = 16)
axis(1, at = 1:4, labels = levels(insectos$Color))
points(1:4, medias, pch = 19, col = "red", cex = 1.5)
lines(1:4, medias, col = "red", lwd = 2)

write.csv(insectos, "ejercicio_2_16_insectos.csv", row.names = FALSE)

# (b) ¿Qué conclusión puedes obtener de este gráfico sobre la atracción de estos
# colores sobre los insectos?

"Los insectos muestran una preferencia marcada por trampas de color amarillo, seguidas 
por las verdes. Esto sugiere que el color de la trampa influye significativamente en la 
cantidad de insectos capturados."

# (c) ¿Tiene sentido hablar de una asociación positiva o negativa entre el color
#     de la trampa y el número de insectos capturados?

"No tiene sentido hablar de una asociación positiva o negativa en el sentido clásico de 
correlación entre dos variables numéricas, porque el color es una variable categórica, 
no cuantitativa.

Sin embargo, sí podemos decir que hay una relación clara entre el tipo de color y la cantidad 
de insectos capturados. Esta relación no se mide con pendiente o dirección, sino con diferencias 
entre grupos."

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::

