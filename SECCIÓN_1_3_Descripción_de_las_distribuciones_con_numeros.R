# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO I "Análisis de distribuciones"

# SECCION 1.3 " Descripción de las distribuciones con números"

# **************************************************
# PREGUNTA 1.27 - Actitud de los estudiantes (SSHA)
# **************************************************

# He aquí los resultados de 18 estudiantes universitarias de primer curso en la 
# prueba SSHA (Survey of Study Habits and Attitudes) sobre hábitos de estudio y actitud:

# Puntuaciones SSHA:
# 154 109 137 115 152 140 154 178 101
# 103 126 126 137 165 165 129 200 148

# (a) Halla sin calculadora la media de estos datos utilizando la fórmula. Ahora,
# calcula la media con la ayuda de una calculadora. Comprueba que obtienes el
# mismo resultado.

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")


ejercicio_1_27 <- read.csv("ejercicio_1_27_SSHA.csv")

str(ejercicio_1_27)

View(ejercicio_1_27)

mean(ejercicio_1_27$Puntuacion)

"La media es: 141.0556"

# (b) El diagrama de tallos del ejercicio 1.9 sugiere que una puntuación de 200
# es una observación atípica. Utiliza tu calculadora para hallar la media prescindiendo
# de dicho valor. Describe brevemente cómo la observación atípica modifica
# la media.

ejercicio_1_9 <- read.csv("ejercicio_1_9_motivacion.csv")

excluir <- 200

mean(ejercicio_1_9$Valores)
mean(ejercicio_1_9$Valores[ejercicio_1_9$Valores != excluir])

"Al prescindir de la observación atípica (200) cambia el valor d ela media, en este caso a una puntuación menor
esto debido al alto valor de la observación mencionada, con lo cual podemos ver lo sensible de la media a los valores
entregados

Con observacion = 141.0556 pts
Sin observacion = 137.5882 pts
"

# **************************************************
# PREGUNTA 1.28 - Médicos suizos (Cesáreas)
# **************************************************

# Un estudio en Suiza examinó el número de cesáreas llevadas a cabo por 
# 15 médicos (hombres) durante un año. Sus resultados fueron:

# Número de cesáreas: 27 50 33 25 86 25 85 31 37 44 20 36 59 34 28

# (a) Dibuja un histograma con estos datos. Fíjate en que existen dos observaciones atípicas.

ejercicio_1_28 <- read.csv("ejercicio_1_28_cesarea.csv")

str(ejercicio_1_28)

View(ejercicio_1_28)

barplot(ejercicio_1_28$Numero_cesareas,
        names.arg = ejercicio_1_28$Doctor,
        main = "Cesáreas por médico",
        xlab = "Médico",
        ylab = "Número de cesáreas",
        las = 2,  
        cex.names = 0.7,  
        col = "lightgreen")

# (b) Halla la media y la mediana del número de cesáreas. ¿Cómo se puede
# explicar, a partir de las observaciones atípicas, la diferencia entre ambas?

mean(ejercicio_1_28$Numero_cesareas)

median(ejercicio_1_28$Numero_cesareas)

"
media = 41.333
mediana = 34

Podemos ver como es de sensible la media pues al haber observaciones atípicas, estas
aumentan el valor de la media con respecto a la mediana
"
# (c) Halla la media y la mediana del número de cesáreas sin las dos observaciones atípicas. 
# Los resultados en (b) y en (c), ¿ilustran la robustez de la mediana
# y la falta de robustez de la media?

excluir <- ejercicio_1_28$Numero_cesareas > 80

mean(ejercicio_1_28$Numero_cesareas[!excluir])

median(ejercicio_1_28$Numero_cesareas[!excluir])

"
media = 34.53846
mediana = 33

En este caso vemos como son de similares sus valores, en este caso la media se parece
a la mediana
"
# **************************************************
# PREGUNTA 1.29 - Los más ricos (EEUU)
# **************************************************

# En EE UU la distribución de los ingresos individuales es muy asimétrica hacia la derecha. 
# En 1997 la media y la mediana de los ingresos del 1% de los estadounidenses más ricos 
# era de 330.000 y 675.000 dólares, respectivamente.

# (a) ¿Cuál de estos valores corresponde a la media y cuál a la mediana?

"
Media = 330.000 dólares

Mediana = 675.000 dólares
"
# (b) Justifica tu respuesta.

"En una distribución asimétrica hacia la derecha (también llamada sesgada positivamente):

La media es mayor que la mediana en distribuciones con sesgo positivo. Pero aquí ocurre lo contrario: 
la mediana (675.000) es mayor que la media (330.000), esto indica que la distribución tiene valores 
extremadamente altos que afectan a la media"

# **************************************************
# PREGUNTA 1.30 - Mediana vs Media (SSHA)
# **************************************************

# En el ejercicio 1.27 hallaste la media de los resultados en la prueba SSHA de
# 18 estudiantes universitarias de primer curso. 

# Puntuaciones SSHA:
# 154 109 137 115 152 140 154 178 101
# 103 126 126 137 165 165 129 200 148

# (a) Ahora, calcula la mediana de estos resultados.

ejercicio_1_30 <- read.csv("ejercicio_1_27_SSHA.csv")

View(ejercicio_1_30)

median(ejercicio_1_30$Puntuacion)

"
mediana = 138.5
"
# (b) ¿La mediana es mayor o menor que la media?

mean(ejercicio_1_30$Puntuacion)

"
media = 141.0556

En este caso la mediana es menor a la media
"
# (c) Explica por qué ocurre de esta manera.

"En este caso se da porque hay una observación atípica (200 pts) que 'arrastra' la
media a tener un valor más alto
"

# **************************************************
# PREGUNTA 1.31 - Médicos suizos (Doctoras)
# **************************************************

# El ejercicio 1.28 proporciona el número de cesáreas realizadas por 15 médicos en Suiza. 
# El mismo estudio también proporciona el número de cesáreas llevadas a cabo por 10 doctoras:

# Número de cesáreas (doctoras): 5 7 10 14 18 19 25 29 31 33

# (a) Halla los cinco números resumen de cada grupo.

ejercicio_1_31 <- read.csv("ejercicio_1_31_cesareas_doctoras.csv")

str(ejercicio_1_31)

View(ejercicio_1_31)

min(ejercicio_1_31$Numero_cesareas)
quantile(ejercicio_1_31$Numero_cesareas,0.25)
median(ejercicio_1_31$Numero_cesareas)
quantile(ejercicio_1_31$Numero_cesareas,0.75)
max(ejercicio_1_31$Numero_cesareas)

min(ejercicio_1_28$Numero_cesareas)
quantile(ejercicio_1_28$Numero_cesareas,0.25)
median(ejercicio_1_28$Numero_cesareas)
quantile(ejercicio_1_28$Numero_cesareas,0.75)
max(ejercicio_1_28$Numero_cesareas)

"
mínimo cesareas   =  5    20
primer cuartil    = 11    27.5
mediana           = 18.5  34
segundo cuartil   = 28    47
máximo cesareas   = 33    86
"

# (b) Dibuja un diagrama de tallos doble para comparar el número de operaciones
# realizadas por los doctores y las doctoras. ¿Cuáles son tus conclusiones?


install.packages("aplpack")
library(aplpack)

stem.leaf.backback(ejercicio_1_28$Numero_cesareas, ejercicio_1_31$Numero_cesareas)

"El diagrama stem-and-leaf back-to-back evidencia diferencias en la distribución del 
número de cesáreas según el sexo del profesional. En el grupo de doctores (ejercicio_1_28) 
se observa mayor variabilidad, con valores que llegan hasta seis y una concentración notable 
en torno a dos y tres cesáreas. En contraste, en el grupo de doctoras (ejercicio_1_31) 
los casos se concentran mayormente entre cero y dos cesáreas, mostrando menor dispersión y 
ausencia de valores extremos. Esto sugiere que los doctores tienden a registrar un rango más 
amplio y elevado de cesáreas, mientras que las doctoras presentan una práctica más homogénea 
y centrada en valores bajos."

# **************************************************
# PREGUNTA 1.32 - Edad de presidentes de EE UU
# **************************************************

# ¿Qué edad tenían los presidentes de EE UU al inicio de su mandato? 
# Bill Clinton tenía 46 años, ¿era muy joven cuando tomó posesión de su cargo? 
# La tabla proporciona las edades de todos los presidentes de EE UU al inicio de su mandato.

# Presidente        Edad   Presidente      Edad   Presidente      Edad
# --------------------------------------------------------------------
# Washington        57     Buchanan        65     Harding        55
# J. Adams          61     Lincoln         52     Coolidge       51
# Jefferson         57     A. Johnson      56     Hoover         54
# Madison           57     Grant           46     F. Roosevelt   51
# Monroe            58     Hayes           54     Truman         60
# J. Q. Adams       57     Garfield        49     Eisenhower     61
# Jackson           61     Arthur          51     Kennedy        43
# Van Buren         54     Cleveland       47     L. Johnson     55
# W. H. Harrison    68     B. Harrison     55     Nixon          56
# Tyler             51     Cleveland       55     Ford           61
# Polk              49     McKinley        54     Carter         52
# Taylor            64     T. Roosevelt    42     Reagan         69
# Fillmore          50     Taft            51     Bush           64
# Pierce            48     Wilson          56     Clinton        46

# (a) Representa mediante un diagrama de tallos la distribución de las edades.
# A partir de la forma de la distribución, ¿crees que la mediana tiene que ser mucho
# menor que la media, igual o mucho mayor?

ejercicio_1_32 <- read.csv("ejercicio_1_32_presidentes.csv")

str(ejercicio_1_32)

View(ejercicio_1_32)

stem(ejercicio_1_32$Edad)

"Presenta una forma simétrica, lo que puede significar que media y mediana sean
iguales"

# (b) Calcula la media y los cinco números resumen, y comprueba que la mediana
# está donde tú esperabas hallarla.

summary(ejercicio_1_32$Edad)

"
media = 54.83
mediana = 55

Lo que significa que las edaddes de los presidentes oscila entre los 50 años"

# (c) ¿Cuál es el recorrido del 50% de las observaciones centrales de las edades
# de los presidentes al inicio de su mandato? ¿Bill Clinton estaba entre el 25% de
# presidentes más jóvenes?

"El recorrido va desde los 42 años (min) hasta los 55 años (median), considerando que el
primer cuartil es hasta los 51 años,podemos afirmar que el presidente Bill Clinton esta entre
los presidentes mas jóvenes"

# **************************************************
# PREGUNTA 1.33 - PIB per cápita de Estados europeos
# **************************************************

# La tabla 1.6 contiene datos sobre los Estados europeos. Queremos comparar la 
# distribución del PIB per cápita de los países de la Unión Europea (UE) con la 
# de los países que formaban parte del bloque soviético (EE).

# Resultados del programa estadístico Minitab:

# PAÍSES EE (EX BLOQUE SOVIÉTICO):
# N = 19
# MEAN = 2124.7
# MEDIAN = 2160
# STDEV = 1541.3
# MIN = 360
# MAX = 7140
# Q1 = 1080
# Q3 = 2590

# PAÍSES UE (UNIÓN EUROPEA):
# N = 15
# MEAN = 21078.1
# MEDIAN = 22445
# STDEV = 7900.7
# MIN = 7480
# MAX = 39850
# Q1 = 16020
# Q3 = 24290

# (a) Utiliza estos resultados para dibujar en un mismo gráfico el diagrama de caja
# de los países de la Unión Europea (UE) y el diagrama de caja de los países que
# formaban parte del bloque soviético (EE).

boxplot_data <- list(
  "Ex Bloque Soviético (EE)" = c(360, 1080, 2160, 2590, 7140),
  "Unión Europea (UE)" = c(7480, 16020, 22445, 24290, 39850)
)

boxplot(boxplot_data,
        main = "PIB per cápita - Diagramas de Caja",
        ylab = "Dólares",
        col = c("lightblue", "lightgreen"),
        border = c("blue", "darkgreen"))


# (b) Describe brevemente la comparación de las dos distribuciones.

"Existe una brecha económica abismal entre ambos grupos. Los países de la UE tienen 
economías mucho más ricas y diversas, mientras que los ex países soviéticos muestran 
economías más pobres y uniformes, reflejando el diferente desarrollo económico tras 
la Guerra Fría."