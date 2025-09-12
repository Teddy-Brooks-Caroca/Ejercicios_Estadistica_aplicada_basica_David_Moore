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
#     de vino ayuda a explicar las muertes por ataques al corazón.

# (b) Describe la forma de la relación. ¿Existe una relación lineal? ¿Es una rela
#     ción fuerte?

# (c) La dirección de la asociación, ¿es positiva o negativa? Explica de forma lla
#     na qué dice la relación sobre el consumo de vino y los ataques al corazón. Estos
#     datos, ¿proporcionan una clara evidencia de que tomar vino causa una reducción
#     de las muertes por ataques al corazón? ¿Por qué?

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
# (b) La asociación entre estas variables, ¿es positiva o negativa? Explica por
#     qué crees que la relación va en este sentido.
# (c) Describe la forma y la fuerza de la relación.