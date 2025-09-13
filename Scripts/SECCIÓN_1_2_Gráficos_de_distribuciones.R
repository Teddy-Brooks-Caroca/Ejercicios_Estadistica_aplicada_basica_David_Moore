
# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO I "Análisis de distribuciones"

# SECCIÓN 1.2 "Gráficos de distribuciones" 

# **************************************************
# PREGUNTA 1.1 - Datos de consumo de vehículos (1998)
# **************************************************

# He aquí un pequeño conjunto de datos sobre el consumo (en litros a los 100 kilómetros) de vehículos de 1998:

# Marca y modelo    Tipo de vehículo  Tipo de cambio  Número de cilindros  Consumo en ciudad  Consumo en carretera
# --------------------------------------------------------------------------------------------------------------
# BMW318I           Pequeño           Automático      4                   10.8                7.6
# BMW318I           Pequeño           Manual          4                   10.3                7.4
# Buick Century     Medio             Automático      6                   11.8                8.2
# Chevrolet Blazer  Todoterreno       Automático      6                   14.8                11.8

# (a) ¿Qué individuos describe este conjunto de datos?
"
El conjunto descrito corresponde a un pequeño grupo de autos
"

# (b) Para cada individuo, ¿qué variables se dan? ¿Cuáles de estas variables son categóricas y cuáles numéricas?
"
Variables categóricas: Marca y módelo; Tipo de vehiculo; Tipo de cambio
Variables numéricas: Número de cilindros;Consumo en ciudad; Consumo en carretera 
"

# **************************************************
# PREGUNTA 1.2 - Estudio médico
# **************************************************

# Los datos sobre un estudio médico contienen valores de muchas variables para cada uno de los sujetos del estudio. 
# De las siguientes variables, ¿cuáles son categóricas y cuáles son numéricas?

# (a) Género (hombre o mujer) 
"CATEGÓRICA"

# (b) Edad (años) 
"NUMÉRICA"

# (c) Raza (asiática, negra, blanca u otras) 
"CATEGÓRICA"

# (d) Fumador (sí, no) 
"CATEGÓRICA"

# (e) Presión sanguínea (en milímetros de mercurio) 
"NUMÉRICA"

# (f) Concentración de calcio en la sangre (en microgramos por litro) 
"NUMÉRICA"

# **************************************************
# PREGUNTA 1.3 - Doctoras en EE.UU. (1994)
# **************************************************

# Datos sobre el porcentaje de mujeres que se doctoraron en distintas disciplinas en EE UU durante 1994:

# Disciplina       Porcentaje
# --------------------------
# Informática     15.4%
# Pedagogía       60.8%
# Ingeniería      11.1%
# Biología        40.7%
# Física          21.7%
# Psicología      62.2%

# (a) Presenta estos datos en forma de diagrama de barras.

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//LIBRO DE MOORE//Archivos")

ejercicio_1_3 <- read.csv("ejercicio_1_3_doctoras.csv")

View(ejercicio_1_3)
str(ejercicio_1_3)

ejercicio_1_3$Porcentaje <- as.numeric(gsub("%", "", ejercicio_1_3$Porcentaje))


barplot(ejercicio_1_3$Porcentaje,
        names.arg = ejercicio_1_3$Disciplina,
        main = "Porcentaje de doctoras por disciplina",
        xlab = "Disciplina",
        ylab = "Porcentaje (%)")


# (b) ¿Sería también correcto utilizar un diagrama de sectores para mostrar estos datos? Justifica tu respuesta.

"En este caso no, ya que aun cuando los datos estén representados en porcentajes, 
estos no representan la fracción de la totalidad"

# **************************************************
# PREGUNTA 1.4 - Defunciones en hospitales españoles (1996)
# **************************************************

# Causas de muerte más significativas en los hospitales españoles en 1996:

# Causa de defunción                                Número de casos
# -----------------------------------------------------------------
# Trastornos del aparato circulatorio                       133,499
# Tumores                                                    89,204
# Trastornos del aparato respiratorio                        34,718
# Trastornos del aparato digestivo                           18,861
# Trastornos del sistema inmunológico (incluye sida)          5,504
# Causas externas de traumatismos y envenenamientos          16,324

# (a) Halla el porcentaje de cada una de las causas de defunción y exprésalo con valores enteros. 
# ¿Qué porcentaje de defunciones se debió a tumores?


ejercicio_1_4 <- read.csv("ejercicio_1_4_causas.csv")

str(ejercicio_1_4)

View(ejercicio_1_4)

ejercicio_1_4$Porcentaje <- (ejercicio_1_4$Número.de.casos / sum(ejercicio_1_4$Número.de.casos)) * 100

ejercicio_1_4$Porcentaje<- round(ejercicio_1_4$Porcentaje,2)

"El 29.92% corresponde a Tumores como causa de defunción"


# (b) Dibuja un diagrama de barras de la distribución de las causas de muerte en los hospitales españoles. 
# Identifica bien cada barra.

barplot(ejercicio_1_4$Número.de.casos,
        names.arg = ejercicio_1_4$Causa.de.defunció,
        main = "Defunciones en hospitales españoles (1996)",
        xlab = "Causa de defunción",
        ylab = "Número de casos",
        las = 2)

# (c) ¿También sería correcto utilizar un diagrama de sectores para representar los datos? Justifica tu respuesta.

"En este caso si es correcto, ya que los porcentajes son una fracción del total"

pie(ejercicio_1_4$Porcentaje,
    labels = ejercicio_1_4$Causa.de.defunción,
    main = "Defunciones en hospitales españoles (1996)")

# **************************************************
# PREGUNTA 1.5 - Consumo de gasolina (1998)
# **************************************************

# El Ministerio de Industria exige que los fabricantes de automóviles den a conocer 
# el consumo en ciudad y en carretera de cada modelo de automóvil. 
# La tabla muestra los consumos en carretera de 26 coches durante 1998. 
# Dibuja un histograma sobre los consumos en carretera de los automóviles

# Tabla de datos:
# Modelo              Consumo (litros/100km)
# ------------------------------------------
# Acura3,5RL              9,5
# AudiA6Quattro           9,1
# BuickCentury            8,2
# CadillacCatera          9,9
# CadillacEldorado        9,1
# ChevroletLumina         8,2
# ChryslerCirrus          7,9
# DodgeStratus            8,4
# FordTaurus              8,4
# HondaAccord             8,2
# HyundaiSonata           8,5
# InfinitiI30             8,4
# InfinitiQ45            10,3
# LexusGS300             10,3
# LexusLS400              9,5
# LincolnMarkVIII         9,1
# Mazda626                7,2
# Mercedes-BenzE320       8,2
# Mercedes-BenzE420       9,1
# MitsubishiDiamante      9,9
# NissanMaxima            8,4
# OldsmobileAurora        9,1
# Rolls-RoyceSilverSpur  14,8
# Saab900S                9,5
# ToyotaCamry             7,9
# VolvoS70                9,5

ejercicio_1_5 <- read.csv("ejercicio_1_5_autos.csv")

str(ejercicio_1_5)

View(ejercicio_1_5)

hist(ejercicio_1_5$Consumo..l.100km.,
     main = "Consumo de gasolina (1998)",
     xlab = "Consumo de gasolina",
     ylab = "Frecuencia (cantidad de modelos)")

# **************************************************
# PREGUNTA 1.6 - Consumo de gasolina de automóviles
# **************************************************

# La tabla 1.2 proporciona datos sobre el consumo de automóviles. 
# Basándote en el histograma de estos datos:

# (a) Describe las características principales (forma, centro, dispersión y observaciones atípicas) 
# de la distribución del consumo en carretera.

"La forma del gráfico es asimétrica hacia la derecha, cuyo centro está en 9 litros por cada 100 kilometros,
habiendo una dispersión entre 8 litros hasta los 14 litros, siendo una dispersión atípica de 14 litros, lo 
cual se puede deber a la propia capacidad del motor"

# (b) El Gobierno impone un impuesto especial para coches con un consumo muy elevado. 
#   ¿Qué modelos crees que podrían ser objeto de este impuesto?

"Todos aquellos autos que sobrepasan los 10 litros de consumo es Infiniti Q45, 
Lexus GS300 y el Rolls-Royce SilverSpur"

# **************************************************
# PREGUNTA 1.7 - Descripción de distribuciones
# **************************************************

# ¿Cómo describirías el centro y la dispersión de la distribución del primer relámpago del día 
# de la figura 1.3? 

install.packages("magick")
library(magick)

figura_1_3 <- image_read("figura_1_3.png")
print(figura_1_3)

"La forma del gráfico es aproximadamente simétrica, cuyo centro esta en 12 hrs (mediodía), habiendo una dipersión
entre 7 hrs (07:00 AM) hasta 17 hrs (05:00 PM), no habbiendo desviaciones atípicas entre los extremos y su distribución"

# ¿Y de la distribución de la longitud de las palabras de la figura 1.4?

figura_1_4 <- image_read("figura_1_4.png")
print(figura_1_4)

"La forma del gráfico es asimetrica a la derecha, cuyo centro esta entre 3 letras por palabra y 4 letras por palabra, 
habiendo una dipersión entre 1 letra por palabra hasta 12 letras por palabra; podemos considerar que el extremo izquierdo
(12 letras por palabra) puede ser una observación atípica debido a la propia extensión de la palabra."

# **************************************************
# PREGUNTA 1.8 - Rendimiento de acciones
# **************************************************

# El rendimiento total de una acción se obtiene teniendo en cuenta su precio de venta en Bolsa 
# y los dividendos pagados por la empresa. El rendimiento total se expresa normalmente como 
# un porcentaje sobre el precio de compra inicial. 

# La figura 1.5 es un histograma sobre la distribución de los rendimientos totales de 1.528 
# acciones en la Bolsa de Nueva York durante un año. Al igual que la figura 1.4, la figura 1.5 
# es un histograma de los porcentajes de cada clase y no un histograma de recuentos.

# (a) Describe la forma de la distribución de los rendimientos totales.

figura_1_5 <- image_read("figura_1_5.png")

print(figura_1_5)

"La forma del gráfico es aproximadamente simétrica, cuyo centro está en el 20% del rendimiento, 
habiendo una dispersión que va desde -60% hasta 100%, no creemos que los extremos sean observaciones
atípicas, sino fluctuaciones propias del desarrollo en el desempeño."

# (b) ¿Cuál es el centro aproximado de esta distribución? (Recuerda que, por ahora, consideramos 
# el centro como aquel valor respecto al cual la mitad de las acciones tienen valores 
# superiores y la otra mitad inferiores.)

"El centro está aproximadamente en el 20% de rendimiento total"

# (c) De una manera aproximada, ¿cuáles son los rendimientos mínimo y máximo? 
# (Estos resultados describen la dispersión de la distribución.)

"El mínimo observado está en -60% y el máximo está en el 100%"

# (d) Un rendimiento total menor que cero significa que se ha perdido dinero. 
# ¿Qué porcentaje de las acciones lo ha perdido?

"Aproximadamente entre -30% al -20%"

# **************************************************
# PREGUNTA 1.9 - Motivación y actitud de los estudiantes (SSHA)
# **************************************************

# La prueba SSHA (Survey of Study Habits and Attitudes) es una prueba psicológica
# que valora la motivación y la actitud de los estudiantes. Una universidad privada
# somete a la prueba SSHA a una muestra de 18 alumnas de primer curso.

# Datos de las puntuaciones SSHA:
# 154 109 137 115 152 140 154 178 101
# 103 126 126 137 165 165 129 200 148

# (a) Dibuja un diagrama de tallos con estos datos.La forma de la distribución es irregular, 
# lo cual es frecuente cuando se dispone sólo de un número pequeño de observaciones.

ejercicio_1_9 <- read.csv("ejercicio_1_9_motivacion.csv")

str(ejercicio_1_9)

View(ejercicio_1_9)

stem(ejercicio_1_9$Valores)


# (b) ¿Has detectado observaciones atípicas?

"Se ha detectado una observación atípica que corresponde al valor 200, lo significa
que dicha alumna ha aprobado con excelencia"

# (d) ¿Dónde se encuentra el centro de la distribución, es decir, la puntuación
# tal que una mitad de las puntuaciones son mayores y la otra mitad menores?

"Al ser una curva asimétrica a la derecha(valores mas cercanos al eje) su centro  
se ubica entre 120 puntos"

# (e) ¿Cuál es la dispersión de los datos (prescindiendo de las posibles 
# observaciones atípicas)?

"La dispersión se ubica entre los 100 puntos hasta los 160 puntos"

# **************************************************
# PREGUNTA 1.10 - Fondos de inversión (EEUU)
# **************************************************

# Los intereses medios anuales, en porcentaje, pagados por unos determinados 
# fondos de inversión en EE UU son los siguientes:

# Año   Intereses   Año   Intereses   Año   Intereses   Año   Intereses
# ----------------------------------------------------------------------
# 1973     7,60     1979    10,92     1985     7,77     1991     5,70
# 1974    10,79     1980    12,88     1986     6,30     1992     3,31
# 1975     6,39     1981    17,16     1987     6,17     1993     2,62
# 1976     5,11     1982    12,55     1988     7,09     1994     3,65
# 1977     4,92     1983     8,69     1989     8,85     1995     5,37
# 1978     7,25     1984    10,21     1990     7,81     1996     4,80

# (a) Dibuja un diagrama temporal con los intereses de los fondos de inversión.

ejercicio_1_10 <- read.csv("ejercicio_1_10_fondos.csv")

str(ejercicio_1_10)

View(ejercicio_1_10)

plot(ejercicio_1_10$Año, ejercicio_1_10$Intereses....,
     main = "Fondos de inversión (EEUU)",
     xlab = "Año",
     ylab = "Intereses",
     type = "o",
     xaxt = "n",  
     yaxt = "n")
axis(1, at = ejercicio_1_10$Año, labels = ejercicio_1_10$Año)
axis(2, at = seq(min(ejercicio_1_10$Intereses....),
                 max(ejercicio_1_10$Intereses....), by = 1))

# (b) Las tasas de interés, al igual que muchas variables económicas, muestran
# ciclos, es decir, subidas y bajadas de su valor que aunque irregulares son claras.
# ¿En qué años aparecen picos temporales en los ciclos de la tasa de interés?

"Podemos ver que los picos los tenemos en los años 1974, 1981, 1984,1989,1995"

# (c) Además de la presencia de ciclos, los diagramas temporales pueden mostrar
# una tendencia consistente. De los años considerados, ¿en cuál se llega a
# alcanzar el valor máximo? A partir de ese año, ¿se observa una tendencia
# general decreciente?

"Podemos observa que en 1981 se llega al valor máximo de inetereses, después de 
ese año hay una tendencia decreciente hasta el año 1987 donde hay un valle en el
valor de los intereses."

# **************************************************
# PREGUNTA 1.11 - Salarios de técnicos de la FAO
# **************************************************

# He aquí una pequeña parte de un conjunto de datos que describe los salarios 
# pagados por la Organización de las Naciones Unidas para la Agricultura y la 
# Alimentación (FAO) a sus técnicos de alto nivel durante el periodo 1999/2000:

# Técnico              Nacionalidad   Posición                       Edad   Salario
# ---------------------------------------------------------------------------------
# Josep Ferre          Española       Oficial de enlace              38     58.378
# Akima Mohamed        Marroquí       Coordinadora de programa       27     63.477
# Robert Plumb         Británica      Oficial superior de finanzas   63     65.321
# Jorge Pérez          Mexicana       Especialista en gestión        43     57.567

# (a) ¿Qué individuos describe este conjunto de datos?

"Los individuos que decribe son un conjunto de técnicos de alto nivel pertenecientes a la FAO"

# (b) Aparte del nombre de los técnicos, ¿cuántas variables contiene el conjunto
# de datos? De estas variables, ¿cuáles son categóricas y cuáles cuantitativas?

"Hay otras cuatro variables; las categóricas son la nacionalidad y la posición y las
cuantitativas son la edad y salario"

# (c) Basándote en la tabla, ¿cuáles crees que son las unidades de medida de
# cada una de las variables cuantitativas?

"Edad en años, salario en miles de dólares anuales"

# **************************************************
# PREGUNTA 1.12 - ¿A qué edad muere la gente joven?
# **************************************************

# En 1997 las muertes de personas entre 15 y 24 años en EEUU se debieron a 
# siete causas principales:

# Causa de muerte         Número de muertes
# -----------------------------------------
# Accidentes              12.958
# Homicidios               5.793
# Suicidios                4.146
# Cáncer                   1.583
# Enfermedades del corazón 1.013
# Defectos congénitos        383
# SIDA                       276

# (a) Dibuja un diagrama de barras para mostrar la distribución de estos datos.

ejercicio_1_12 <- read.csv("ejercicio_1_12_mortandad.csv")

str(ejercicio_1_12)

View(ejercicio_1_12)

barplot(ejercicio_1_12$Número.de.muertes,
        names.arg = ejercicio_1_12$Causa.de.muerte,
        main = "¿A qué edad muere la gente joven?",
        xlab = "Causa de muerte",
        ylab = "Número de muertes")

# (b) Para dibujar un diagrama de sectores, ¿qué otra información necesitas?

"Necesito los porcentajes que representen la fracción de la totalidad"

ejercicio_1_12$Porcentaje <- (ejercicio_1_12$Número.de.muertes / sum(ejercicio_1_12$Número.de.muertes)) * 100

ejercicio_1_12$Porcentaje<- round(ejercicio_1_12$Porcentaje,2)

pie(ejercicio_1_12$Porcentaje,
    labels = ejercicio_1_12$Causa.de.muerte,
    main = "¿A qué edad muere la gente joven?")

# **************************************************
# PREGUNTA 1.13 - Estilo de escritura y estadística
# **************************************************

# Los datos numéricos pueden distinguir diferentes estilos de escritura e incluso 
# a veces hasta autores individuales. Tenemos datos sobre el porcentaje de palabras 
# de 1 a 15 letras utilizadas en los artículos de la revista Popular Science:

# Longitud:   1    2    3    4    5    6    7    8
# Porcentaje: 3,6  14,8 18,7 16,0 12,5 8,2  8,1  5,9

# Longitud:   9    10   11   12   13   14   15
# Porcentaje: 4,4  3,6  2,1  0,9  0,6  0,4  0,2

# (a) Dibuja un histograma correspondiente a esta distribución. Describe la forma, 
# el centro y la dispersión.

ejercicio_1_13 <- read.csv("ejercicio_1_13_longitud.csv")

str(ejercicio_1_13)

View(ejercicio_1_13)

barplot(ejercicio_1_13$Porcentaje,
        names.arg = ejercicio_1_13$Longitud,
        main = "Estilo de escritura y estadística",
        xlab = "Longitud de la palabra (letras)",
        ylab = "Porcentaje",
        col = "lightblue")

"La forma que presenta es sesgada a la derecha. La mayoría de las palabras tienen entre 2 y 6 letras; 
las más largas son mucho menos frecuentes. Por ende, su centro es aproximadamente entre 3 y 5 letras 
(moda cerca de 3 letras con 18,7%), habiendo una dispersión bastante concentrada en palabras cortas (1–6 letras), 
aunque con cola larga hacia palabras de hasta 15 letras."

# (b) ¿Cómo podemos comparar la distribución de la longitud de las palabras
# utilizadas en Popular Science con la distribución de la longitud de las palabras en
# las obras de Shakespeare de la figura 1.4? Fíjate especialmente en las palabras
# cortas (2, 3 y 4 letras) y en las palabras muy largas (más de 10 letras).

"Shakespeare utiliza más palabras cortas y menos palabras muy largas
 que Popular Science."

# **************************************************
# PREGUNTA 1.14 - Huracanes
# **************************************************

# El histograma de la figura 1.8 muestra el número de huracanes que alcanzaron 
# la costa este de EE UU durante un periodo de 70 años.

# (a) Describe de manera breve la forma de esta distribución. 

figura_1_8 <- image_read("figura_1_8.png")
print(figura_1_8)

"El grafico presenta una forma asimétrica a la derecha estando su centro entre 3 a 4 huracanes 
con un leve pico en 6 huracan por año,habiendo una dispersión desde 0 a 9 huracanes por año, sin 
haber observaciones atípicas claras, podemos concluir que es común que en un año puedan haber hasta 
4 huracanes por año"

# (b) ¿Dónde queda aproximadamente su centro?

"Esta aproximadamente en 4 huracanes por año"

# **************************************************
# PREGUNTA 1.15 - Número de goles
# **************************************************

# La figura 1.9 muestra la distribución del número de goles de los jugadores de 
# primera división de la liga española de fútbol que al menos marcaron 5 goles 
# durante la temporada 1999/2000.

# (a) La distribución, ¿es aproximadamente simétrica, claramente asimétrica o
# ninguna de la dos cosas?

figura_1_9 <- image_read("figura_1_9.png")
print(figura_1_9)

"La distribución es claramente asimetrica a la derecha, lo que significa que existe una cantidad 
de jugadores que marcan menos de 5 goles por temporada"

# (b) ¿Cuál es el número de goles típico de un jugador de la liga española de
# fútbol de la temporada 1999/2000? ¿Cuáles son el máximo y el mínimo de goles
# marcados?

"La cantidad tipica va desde 5 goles hasta 6 goles por temporada, siendo el minimo de goles 5 y el maximo 27,
pero se pueden tomar como observaciones atipicas despues de 13 goles anotados."

# **************************************************
# PREGUNTA 1.16 - Monedas y fechas
# **************************************************

# Supón que tus amigos y tú mismo vaciáis vuestros monederos y vais apuntando 
# la fecha que aparece en cada moneda que sacáis. La distribución de estos
# datos es asimétrica hacia la izquierda. Explica por qué.

"La distribución es asimétrica hacia la izquierda porque la mayoría de las monedas 
en circulación son de años recientes, mientras que solo unas pocas monedas antiguas 
siguen apareciendo. Esto concentra los valores en los años más nuevos y genera una 
cola que se extiende hacia los años anteriores."

# **************************************************
# PREGUNTA 1.17 - Pirámide de edad en EE UU
# **************************************************

# La pirámide de edad de un país tiene una gran influencia sobre sus condiciones 
# sociales y económicas. La tabla muestra la distribución por edades de los 
# residentes en EE UU en el año 1950 y en el 2075, en millones de personas.

# Grupo de edad       1950   2075
# ---------------------------------
# Menor de 10 años    29,3   34,9
# De 10 a 19 años     21,8   35,7
# De 20 a 29 años     24,0   36,8
# De 30 a 39 años     22,8   38,1
# De 40 a 49 años     19,3   37,8
# De 50 a 59 años     15,5   37,5
# De 60 a 69 años     11,0   34,5
# De 70 a 79 años     5,5    27,2
# De 80 a 89 años     1,6    18,8
# De 90 a 99 años     0,1    7,7
# De 100 a 109 años   -      1,7
# Total              151,1  310,6

# (a) Como la población total del año 2075 es muy superior a la de 1950, la
# comparación de los porcentajes de cada grupo de edades es más clara que la
# comparación de los recuentos. Construye una tabla sobre los porcentajes de población
# total en cada grupo de edad para 1950 y para 2075.

ejercicio_1_17 <- read.csv("ejercicio_1_17_edades.csv")

str(ejercicio_1_17)

View(ejercicio_1_17)

ejercicio_1_17$Porcentaje_1950 <- (ejercicio_1_17$X1950 / sum(ejercicio_1_17$X1950)) * 100

ejercicio_1_17$Porcentaje_1950<- round(ejercicio_1_17$Porcentaje_1950,2)

ejercicio_1_17$Porcentaje_2075 <- (ejercicio_1_17$X2075 / sum(ejercicio_1_17$X2075)) * 100

ejercicio_1_17$Porcentaje_2075<- round(ejercicio_1_17$Porcentaje_2075,2)

# (b) Dibuja el histograma de la distribución por edades (en porcentajes) del
# año 1950. Luego describe las características más importantes de esta distribución.
# En particular, fíjate en el porcentaje de niños respecto al total de la población.

barplot(ejercicio_1_17$Porcentaje_1950,
        names.arg = ejercicio_1_17$Grupo.de.edad,
        main = "Pirámide de edad en EE UU",
        xlab = "Grupo de edad",
        ylab = "Porcentaje de población",
        las = 2,
        col = "lightblue")

"El grafico presenta una forma asimetrica a la derecha, lo que quiere decir que hay una población mayoritariamente 'jóven'
teniendo un centro entre los grupos etarios de 20 y 30 años y una dispersión representativa que va desde los 0 años hasta 
los 69 años, esto se puede explicar por una natalidad constante post segunda guerra y la mantención de los grupos etarios aptos
para escenarios bélicos."

# (c) Dibuja un histograma con los datos estimados del año 2075. Utiliza las
# mismas escalas que has empleado en el apartado (b) para facilitar la comparación.
# ¿Cuáles son los cambios más importantes en la distribución por edades de
# la población estimada de EE UU durante el periodo de 125 años entre 1950 y 2075?

barplot(ejercicio_1_17$Porcentaje_2075,
        names.arg = ejercicio_1_17$Grupo.de.edad,
        main = "Pirámide de edad en EE UU",
        xlab = "Grupo de edad",
        ylab = "Porcentaje de población",
        las = 2,
        col = "lightblue")

"El grafico presenta una forma asimetríca, pero no tan pronunciada como el grafico anterior, esto 
se puede deber al engrosamientode las perspectivas para el año 2075, lo cual podemos ver en un aumento 
de los grupos etarios mas 'viejos' y una extensión en laesperanza de vida"

# **************************************************
# PREGUNTA 1.18 - Goles marcados por Paulino Alcántara
# **************************************************

# He aquí el número de goles que marcó Paulino Alcántara mientras fue jugador
# del F.C. Barcelona, desde la temporada 1911/12 hasta la temporada 1926/27:
# 6 15 21 25 33 0 5 42 47 19 42 34 39 6 15 8

# (a) Dibuja un diagrama de tallos con estos datos.

ejercicio_1_18 <- read.csv("ejercicio_1_18_goles_Paulino.csv")

str(ejercicio_1_18)

View(ejercicio_1_18)

stem(ejercicio_1_18$Goles)

# (b) La distribución, ¿es aproximadamente simétrica, claramente asimétrica o nada de esto?

"La distribución es asimentrica a la derecha"

# (c) En un año típico, ¿cuántos goles marcó aproximadamente Paulino Alcántara?

"En un año típico, Paulino podía anotar entre 15 a 25 goles por temporada"

# (d) ¿Existe alguna observación atípica?

"Hay que poner observación a los años en los que tiene  mas de 40 anotaciones por temporada"

# **************************************************
# PREGUNTA 1.19 - Goles marcados por Ladislao Kubala
# **************************************************

# Ladislao Kubala ha sido uno de los mejores jugadores de fútbol de todos los tiempos.
# He aquí el número de goles que marcó por temporada mientras fue jugador
# del F.C. Barcelona desde la temporada 1950/51 hasta la temporada 1960/61:
# 16 48 18 28 19 22 14 19 17 25 17

# (a) Un diagrama de tallos doble nos ayuda a comparar dos distribuciones.

install.packages("aplpack")
library(aplpack)

ejercicio_1_19 <- read.csv("ejercicio_1_19_goles_Ladislao.csv")

str(ejercicio_1_19)

View(ejercicio_1_19)

stem.leaf.backback(ejercicio_1_18$Goles, ejercicio_1_19$Goles)

# (b) Describe brevemente las diferencias existentes entre ambas distribuciones.

"Ambos presentan un forma asimentrica, sin embargo la distribución de Ladislao es mas pronunciada
lo que explica que haya hecho menos goles, pero la extensión temoral estudada es menor.Ahora bien, se debe considerar
el año donde anotó 48 goles que puede ser considerada como observación atipica por no estar en la distribución
de sus anotaciones"

# **************************************************
# PREGUNTA 1.20 - Mercado en baja
# **************************************************

# Los inversores hablan de un "mercado en baja" cuando el valor de las acciones
# cae sustancialmente. La tabla proporciona datos de todas las caídas de al menos
# un 10% del índice Standard & Poor's entre 1940 y 1977.

# Año         Descenso (%) Duración (meses)
# -----------------------------------------
# 1940-1942   42           28
# 1946        27           5
# 1950        14           1
# 1953        15           8
# 1955        10           1
# 1956-1957   22           15
# 1959-1960   14           15
# 1962        26           6
# 1966        22           8
# 1968-1970   36           18
# 1973-1974   48           21
# 1981-1982   26           19
# 1983-1984   14           10
# 1987        34           3
# 1990        20           3

# (a) Dibuja un diagrama de tallos con los porcentajes de las bajadas del valor
# de las acciones durante estos años. Vuelve a dibujar el diagrama de tallos, pero
# dividiendo los tallos. ¿Qué diagrama prefieres? ¿Por qué?

ejercicio_1_20 <- read.csv("ejercicio_1_20_mercados.csv")

str(ejercicio_1_20)

View(ejercicio_1_20)

stem(ejercicio_1_20$Descenso....)

stem(ejercicio_1_20$Descenso....,scale = 2)

"Personalmente el de tallos divididos ya que distribuye la información de tal manera que no 
parece 'abultada' pudiendo ver los cambios sutiles en la distribución de los datos"

# (b) La forma de esta distribución es irregular, de todas formas la podemos
# describir como algo asimétrica. La distribución, ¿es asimétrica hacia la derecha o
# hacia la izquierda?

"En ambos casos es asimetrica a la derecha"

# (c) Describe el centro y la dispersión de los datos. ¿Qué le dirías a un inversor
# sobre la disminución del valor de las acciones en años con el mercado en baja?

"El descenso porcentual en los mercados en baja varía entre un 10% y un 48%, con un valor típico (mediana y promedio) 
cercano al 20–25%. La duración oscila entre 1 y 28 meses, con una mediana de alrededor de 10–12 meses. La dispersión 
es amplia: algunas caídas son rápidas y poco profundas, mientras que otras son prolongadas y muy severas.

A un inversor se le puede decir que, en promedio, un mercado en baja significa perder cerca de un cuarto del valor de 
las acciones en aproximadamente un año, aunque debe estar preparado para episodios extremos que pueden implicar pérdidas 
cercanas al 50% y durar más de dos años."

# **************************************************
# PREGUNTA 1.21 - Maratón de Boston
# **************************************************

# A partir de 1972, se permitió la participación de mujeres en la maratón de Boston.
# En la tabla se muestran los tiempos (en minutos) de las mujeres que ganaron
# desde 1972 hasta 1998.

# Año  Tiempo  Año  Tiempo  Año  Tiempo
# --------------------------------------
# 1972 190     1981 147     1990 145
# 1973 186     1982 150     1991 144
# 1974 167     1983 143     1992 144
# 1975 162     1984 149     1993 145
# 1976 167     1985 154     1994 142
# 1977 168     1986 145     1995 145
# 1978 165     1987 146     1996 147
# 1979 155     1988 145     1997 146
# 1980 154     1989 144     1998 143

# (a) Dibuja un diagrama temporal con los tiempos de las ganadoras.

ejercicio_1_21 <- read.csv("ejercicio_1_21_maraton.csv")

str(ejercicio_1_21)

View(ejercicio_1_21)

plot(ejercicio_1_21$Año,ejercicio_1_21$Tiempo,
     main = "Maratón de Boston",
     xlab = "Año",
     ylab = "Tiempo",
     type = "o",
     xaxt = "n",
     yaxt = "n")
axis(1, at = ejercicio_1_21$Año, labels = ejercicio_1_21$Año, las = 2, cex.axis = 0.8)
axis(2, at = ejercicio_1_21$Tiempo, labels = ejercicio_1_21$Tiempo, las = 1, cex.axis = 0.8)
grid()

# (b) Describe de forma breve la distribución de los tiempos de las ganadoras
# de la maratón a lo largo de estos años. En los últimos años, ¿los tiempos han
# dejado de bajar?

"La forma del grafico presenta una asimetría a la derecha, lo que significa que conforme pasaban los años
los tiempos de las ganadoras era cada vez menor, lo que se puede suponer un mejor desempeño y/o profesionalismo
en las corredoras; Sin embargo debemos poner atención a los tiempos de los años 1975 y 1983 como observaciones
atipícas, por representar valores abruptos en la secuencia"

# **************************************************
# PREGUNTA 1.22 - La epidemia de gripe de 1918
# **************************************************

# Entre 1918 y 1919 una epidemia de gripe mató a más de 25 millones de personas
# en todo el planeta. He aquí datos sobre el número de nuevos casos de gripe
# y la cantidad de muertos en San Francisco, semana a semana, desde el 5 de
# octubre de 1918 hasta el 25 de enero de 1919.

# Fecha     Nuevos casos  Muertos
# ------------------------------
# 5-oct     36           0
# 12-oct    531          0
# 19-oct    4.233        130
# 26-oct    8.682        552
# 2-nov     7.164        738
# 9-nov     2.229        414
# 16-nov    600          198
# 23-nov    164          90
# 30-nov    57           56
# 7-dic     722          50
# 14-dic    1.517        71
# 21-dic    1.828        137
# 28-dic    1.539        178
# 4-ene     2.416        194
# 11-ene    3.148        290
# 18-ene    3.465        310
# 25-ene    1.440        149

# (a) Dibuja un diagrama temporal con los datos de nuevos casos de gripe.
# Basándote en tu diagrama, describe la progresión de la enfermedad.

ejercicio_1_22 <- read.csv("ejercicio_1_22_epidemia.csv")

str(ejercicio_1_22)

View(ejercicio_1_22)

plot(ejercicio_1_22$Nuevos_casos,
     main = "La epidemia de gripe de 1918",
     xlab = "Fecha",
     ylab = "Nuevos casos",
     type = "o",
     col = "red",
     xaxt = "n")
axis(1, at = 1:17, labels = ejercicio_1_22$Fecha, las = 2, cex.axis = 0.8) #Ponemos las fechas manualmente
grid()

# (b) Nos gustaría comparar la distribución del número de nuevos casos con la
# distribución del número de muertos. Para conseguir que las magnitudes de las
# dos variables sean similares y facilitar la comparación, representa el número de
# muertos con relación al tiempo desde el 5 de octubre hasta el 25 de enero. Luego,
# utilizando otro color, representa en el mismo gráfico el número de nuevos casos
# dividido por 10. ¿Qué ves? Concretamente, ¿cuál es el desfase entre el cambio en
# el número de nuevos casos y el cambio en el número de muertos?

plot(1:nrow(ejercicio_1_22),
     ejercicio_1_22$Muertos,
     type = "o",
     col = "blue",
     pch = 16,
     lwd = 2,
     ylim = c(0, max(c(ejercicio_1_22$Muertos, ejercicio_1_22$Nuevos_casos/10))),
     main = "Comparación: Muertos vs Nuevos casos/10 (1918)",
     xlab = "Fecha",
     ylab = "Cantidad",
     xaxt = "n")
lines(1:nrow(ejercicio_1_22),
      ejercicio_1_22$Nuevos_casos/10,
      type = "o",
      col = "red",
      pch = 16,
      lwd = 2)
axis(1, 
     at = 1:nrow(ejercicio_1_22),
     labels = ejercicio_1_22$Fecha,
     las = 2,
     cex.axis = 0.8)
legend("topright",
       legend = c("Muertos", "Nuevos casos/10"),
       col = c("blue", "red"),
       lwd = 2,
       pch = 16)
grid()

pico_casos <- which.max(ejercicio_1_22$Nuevos_casos)  
pico_muertos <- which.max(ejercicio_1_22$Muertos)     

cat("=== ANÁLISIS DE DESFASE ===\n")
cat("• Pico de nuevos casos:", ejercicio_1_22$Fecha[pico_casos], "\n")
cat("• Pico de muertos:", ejercicio_1_22$Fecha[pico_muertos], "\n")  
cat("• Desfase temporal:", desfase_semanas, "semanas (", desfase_dias, "días)\n")
cat("• El pico de muertos ocurre 1 semana después del pico de casos\n")


# **************************************************
# PREGUNTA 1.23 - ¡Fíjate en las escalas!
# **************************************************

# La impresión que proporciona un gráfico temporal depende de las escalas que 
# utilices en los dos ejes. Si estiras el eje de las ordenadas y comprimes el eje 
# de las abscisas, los cambios aparecen como más rápidos. En cambio, si comprimes 
# el eje de las ordenadas y estiras el eje de las abscisas los cambios aparecen 
# como más lentos. 

# Dibuja dos diagramas temporales más con los datos del ejemplo 1.6, de 
# manera que en un gráfico dé la impresión de que las muertes por cáncer 
# aumentan muy rápidamente y en el otro, en cambio, que dicho incremento 
# parezca muy suave.La moraleja de este ejercicio es: "Fíjate en las escalas 
# cuando mires un diagrama temporal"

ejercicio_1_23 <- read.csv("ejercicio_1_23_escalas.csv")

str(ejercicio_1_23)

View(ejercicio_1_23)

# 1. GRÁFICO NORMAL (referencia)

plot(ejercicio_1_23$Año,ejercicio_1_23$Muertos..x.100.000.,
     main = "Tasa de mortalidad por cáncer en EE UU",
     xlab = "Año",
     ylab = "Cantidad de muertos",
     type = "o")
grid()

# 2. AUMENTO RÁPIDO (estirar eje Y, comprimir eje X)

plot(ejercicio_1_23$Año, ejercicio_1_23$Muertos..x.100.000.,
     main = "Aumento RÁPIDO - Eje Y estirado, Eje X comprimido",
     xlab = "Año", ylab = "Muertos por 100,000 hab.",
     type = "o", col = "red", lwd = 2, pch = 16,
     xlim = c(1945, 1995),    # Eje X comprimido (mismo rango pero menos espacio)
     ylim = c(130, 210))      # Eje Y estirado (mismo rango pero más amplio)
grid()

# 3. AUMENTO SUAVE (comprimir eje Y, estirar eje X)

plot(ejercicio_1_23$Año, ejercicio_1_23$Muertos..x.100.000.,
     main = "Aumento SUAVE - Eje Y comprimido, Eje X estirado",
     xlab = "Año", ylab = "Muertos por 100,000 hab.",
     type = "o", col = "blue", lwd = 2, pch = 16,
     xlim = c(1940, 2000),    # Eje X estirado (rango más amplio)
     ylim = c(120, 210))      # Eje Y comprimido (mismo rango pero menos espacio)
grid()



# **************************************************
# PREGUNTA 1.24 - Población de los Estados Europeos
# **************************************************

# La tabla presenta algunos datos sobre Estados europeos. La primera columna 
# identifica los Estados. La segunda indica la región socio-política a la que 
# pertenece cada uno de ellos: los países de la Unión Europea (UE), los países 
# del Este (EE, ex bloque soviético) y otros países (OT). 

# Estado              Región  Población(1.000hab.) Superficie(km2)  PIB_per_capita(dólares) Periódicos* Televisiones* %PIB_educacion
# ----------------------------------------------------------------------------------------------------------------------------------
# Albania             EE      3.389                28.748           360                     49          89            -
# Alemania            UE      80.857               356.755          25.580                  323         559           -
# Andorra             OT      61                   453              15.000                  67          367           -
# Austria             UE      7.863                83.849           24.950                  398         479           5,80
# Bélgica             UE      10.046               30.513           22.920                  310         453           5,10
# Bielorrusia         EE      10.188               207.595          2.160                   186         272           5,30
# Bosnia-Herzegovina  EE      3.707                51.129           700                     131         -             -
# Bulgaria            EE      8.870                110.912          1.160                   164         260           5,80
# Croacia             EE      4.511                56.538           2.530                   532         338           -
# Dinamarca           UE      5.165                43.069           28.110                  332         538           7,40
# Eslovaquia          EE      5.314                49.035           2.230                   317         474           5,70
# Eslovenia           EE      1.937                20.521           7.140                   160         297           6,20
# España              UE      39.514               504.782          13.280                  104         400           4,60
# Estonia             EE      1.553                45.100           2.820                   -           361           5,90
# Finlandia           UE      5.058                337.009          18.850                  512         504           7,20
# Francia             UE      57.508               547.026          23.470                  205         412           5,80
# Grecia              UE      10.377               131.994          7.480                   135         202           3,10
# Holanda             UE      15.285               40.844           21.970                  383         491           5,90
# Hungría             EE      10.210               93.030           3.840                   282         427           6,70
# Irlanda             UE      3.524                70.283           13.630                  186         301           6,20
# Islandia            OT      263                  103.000          24.590                  519         335           5,60
# Italia              UE      57.127               301.225          19.270                  106         429           5,40
# Letonia             EE      2.611                64.500           2.290                   98          460           6,70
# Liechtenstein       OT      30                   157              35.000                  653         337           -
# Lituania            EE      3.712                65.200           1.350                   225         383           4,40
# Luxemburgo          UE      395                  2.586            39.850                  372         261           -
# Macedonia           EE      2.119                25.713           790                     27          165           5,00
# Malta               OT      361                  316              8.000                   150         745           4,60
# Moldavia            EE      4.408                33.700           870                     47          -             6,50
# Mónaco              OT      31                   2                25.000                  258         739           -
# Noruega             UE      4.299                324.219          26.480                  607         427           8,40
# Polonia             EE      38.303               312.677          2.470                   159         298           5,50
# Portugal            UE      9.838                92.082           9.370                   47          190           5,00
# ReinoUnido          UE      57.924               244.046          18.410                  383         435           5,20
# RepúblicaCheca      EE      10.296               78.864           3.210                   583         476           5,80
# Rumania             EE      23.023               237.500          1.230                   324         200           3,60
# Rusia               EE      147.760              17.075.400       2.650                   387         372           4,40
# SanMarino           OT      24                   61               20.000                  -           352           -
# Suecia              UE      8.694                449.964          23.630                  511         470           8,30
# Suiza               OT      7.056                41.288           23.630                  377         400           5,20
# Ucrania             EE      51.551               603.700          1.570                   118         339           6,10
# Yugoslavia          EE      10.623               87.968           1.000                   52          179           -

# *Por 1.000 habitantes
# EE = Estado del Este
# UE = Unión Europea
# OT = Otros Estados

# (a) Dibuja un diagrama de tallos sobre la población de los Estados.

ejercicio_1_24 <- read.csv("ejercicio_1_24_estados_europeos.csv")

str(ejercicio_1_24)

View(ejercicio_1_24)

stem(ejercicio_1_24$Población.1.000hab..,scale = 4)

# (b) Describe brevemente la forma, el centro y la dispersión de la distribución poblacional.

"Forma: La distribución es marcadamente asimétrica hacia la derecha (sesgo positivo). 
La mayoría de los países tienen poblaciones pequeñas, mientras que unos pocos países 
tienen poblaciones muy grandes.

Centro: La mediana estaría alrededor de los 5-10 millones de habitantes. La media 
sería significativamente más alta debido a la influencia de los países muy poblados.

Dispersión: La dispersión es muy amplia, desde 24,000 habitantes (San Marino) hasta 
147,760,000 habitantes (Rusia). El rango intercuartílico también sería considerable."

# (c) Explica por qué la forma de la distribución no es sorprendente.

"La forma de la distribución no es sorprendente porque sigue el patrón típico de distribución 
de tamaños poblacionales a nivel mundial. Es común encontrar:

- Muchos países pequeños y medianos

- Pocos países muy grandes (potencias demográficas)

- Este patrón se observa en la mayoría de las distribuciones de tamaño poblacional a nivel global"

# (d) ¿Hay algún Estado que consideres que es una observación atípica?

"Sí, hay observaciones atípicas claras:

Rusia (147.760 miles de hab.) - Es claramente un valor atípico, con una población 
más del doble que el segundo país más poblado (Alemania)

Alemania (80.857 miles de hab.) - También podría considerarse atípico por su tamaño 
significativamente mayor que la mayoría de los países europeos

Los microestados como San Marino (24), Mónaco (31), Liechtenstein (30) y Andorra (61) 
también son atípicos por su extremadamente pequeña población.

Estos valores extremos son los que causan el fuerte sesgo positivo de la distribución."

# **************************************************
# PREGUNTA 1.25 - Distribución del PIB per cápita
# **************************************************

# (a) Dibuja un diagrama de tallos con la distribución del PIB per cápita de los 
# Estados europeos (tabla 1.6).

stem(ejercicio_1_24$PIB_per_capita.dólares.,scale = 2)

# (b) Describe brevemente la forma de la distribución.

"La distribución del PIB per cápita es bimodal con dos grupos claros:
  
- Un grupo de países con PIB bajo (menos de $10,000)

- Un grupo de países con PIB alto (más de $15,000)

Hay una brecha económica evidente entre los países del Este y los de la UE"

# (c) Halla el punto medio de los datos y señala su valor en el diagrama de tallos.

"El valor medio entre el mínimo ($360) y el máximo ($39,850) es:
(360 + 39,850) / 2 = 20,105

Este valor caería en el tallo '201' (20,105)"

# **************************************************
# PREGUNTA 1.26 - Televisores por cada 1.000 habitantes
# **************************************************

# (a) Representa gráficamente la distribución del número de televisores por cada 
# 1.000 habitantes en los Estados europeos (tabla 1.6).

stem(ejercicio_1_24$Televisiones,scale = 2)

# (b) ¿Cuál es la forma de la distribución?

"La distribución es aproximadamente simétrica con una ligera asimetría hacia la izquierda. 
La mayoría de los países tienen entre 300-500 televisiones por cada 1,000 habitantes."

# (c) ¿Existe alguna observación atípica o desviación notable?

" Sí, hay algunas observaciones atípicas:
  
Malta (745) - Valor extremadamente alto

Mónaco (739) - Valor extremadamente alto

Yugoslavia (179) - Valor extremadamente bajo

Macedonia (165) - Valor extremadamente bajo

Portugal (190) - Valor muy bajo

Estos valores atípicos reflejan diferencias significativas en el acceso a medios 
de comunicación entre los países europeos. Malta y Mónaco tienen valores excepcionalmente 
altos, posiblemente debido a su pequeño tamaño y alta densidad poblacional, mientras 
que los países balcánicos muestran valores notablemente más bajos."

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::
