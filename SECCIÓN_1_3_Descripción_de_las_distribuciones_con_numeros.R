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

# **************************************************
# PREGUNTA 1.34 - Concentración de fosfatos en sangre
# **************************************************

# He aquí las mediciones del nivel de fosfatos en la sangre de un paciente que 
# realizó seis visitas consecutivas a una clínica, expresadas en miligramos de 
# fosfato por decilitro de sangre:

# Mediciones: 5,6 5,2 4,6 4,9 5,7 6,4

# (a) Halla la media a partir de su definición. Es decir, halla la suma de las
# 6 observaciones y divide por 6.

media_fosfato_sangre <- sum(5.6, 5.2, 4.6, 4.9, 5.7, 6.4) / 6

cat("La media del nivel de fosfato en sangre del paciente es:",media_fosfato_sangre,"mg/dl")

# (b) Halla la desviación típica a partir de su definición. Es decir, calcula la
# desviación de cada observación respecto a su media y eleva estas desviaciones al
# cuadrado. Luego, calcula la varianza y la desviación típica. El ejemplo 1.10 ilustra
# este método.

desviaciones <- c(5.6-5.4, 5.2-5.4, 4.6-5.4, 4.9-5.4, 5.7-5.4, 6.4-5.4)

desviaciones_cuad <- desviaciones^2

suma_cuadrados <- sum(desviaciones_cuad)

varianza <- suma_cuadrados / (6 - 1)

desviacion_tipica <- sqrt(varianza)

cat("Desviación típica:", round(desviacion_tipica, 4), "mg/dl\n")
cat("Varianza:", round(varianza, 4), "(mg/dl)²\n")

# (c) Ahora introduce los datos en tu calculadora y halla la media y la desviación
# típica. ¿Has obtenido los mismos resultados que en los cálculos hechos a
# mano?

ejercicio_1_34 <- read.csv("ejercicio_1_34_fosfato.csv")

View(ejercicio_1_34)

mean(ejercicio_1_34$Valor_mg_dl)

sd(ejercicio_1_34$Valor_mg_dl)


# **************************************************
# PREGUNTA 1.35 - Ferenc Puskas (Goles por temporada)
# **************************************************

# El gran jugador de fútbol Ferenc Puskas, conocido popularmente como 
# Cañoncito Pum, jugó de la temporada 1948/49 a la 1956/57 en el Kispest de 
# Budapest. En 1956 huyó de Hungría cuando estalló la Revolución húngara y 
# estuvo dos temporadas sin jugar. En la temporada 1958/59 fichó por el Real 
# Madrid y estuvo en activo en este equipo como jugador de la liga española 
# hasta la temporada 1965/66.

# He aquí el número de goles que marcó por temporada:

# Temporada   Goles   Temporada   Goles
# -------------------------------------
# 1948/49     31      1957/58     0
# 1949/50     25      1958/59     21
# 1950/51     21      1959/60     25
# 1951/52     22      1960/61     27
# 1952/53     27      1961/62     20
# 1953/54     21      1962/63     26
# 1954/55     18      1963/64     20
# 1955/56     5       1964/65     11
# 1956/57     0       1965/66     4

# (a) Utiliza tu calculadora para hallar la media (¯x) y la desviación típica (s) del número
# de goles en la liga desde la temporada 1948/49 hasta la temporada 1965/66.

ejercicio_1_35 <- read.csv("ejercicio_1_35_goles_puskas.csv")

str(ejercicio_1_35)

View(ejercicio_1_35)

mean(ejercicio_1_35$Goles)

sd(ejercicio_1_35$Goles)

"
media = 18 goles
desviación = 9.72
"
# (b) Utiliza tu calculadora para hallar ¯x y s una vez eliminadas las temporadas
# 1956/57 y 1957/58. ¿Cómo afecta la eliminación de estas dos temporadas a los
# valores de ¯x y s?

excluir <- 0

mean(ejercicio_1_35$Goles[ejercicio_1_35$Goles != excluir])

sd(ejercicio_1_35$Goles[ejercicio_1_35$Goles != excluir])

"
media= 20.25
desviación = 7.64635

Al sacar los valores 0 aumenta la media, pero disminuye la dispersión
"

# **************************************************
# PREGUNTA 1.36 - PIB per cápita de Estados europeos
# **************************************************

# El ejercicio 1.33 proporciona resúmenes numéricos sobre el PIB per cápita de 
# los Estados europeos pertenecientes a la Unión Europea y sobre los Estados 
# que formaban parte del antiguo bloque soviético.

# (a) Ahora considera todos los Estados europeos conjuntamente. Calcula
# los cinco números resumen y dibuja el diagrama de caja correspondiente.

ejercicio_1_36 <- read.csv("ejercicio_1_24_estados_europeos.csv")

View(ejercicio_1_36)

summary(ejercicio_1_36$PIB_per_capita.dólares.)

boxplot(ejercicio_1_36$PIB_per_capita.dólares.,
        main = "PIB per cápita de Estados europeos",
        ylab = "Dolares")

"
Min. 1st Qu.  Median Mean    3rd Qu. Max.
-----------------------------------------
360  2245     8685   12591   23333   39850 
"


# (b) Estos resúmenes numéricos (y el diagrama de caja derivado del mismo) no muestran
# una de las características más importantes de esta distribución.Dibuja un diagrama 
# de tallos con todos los datos sobre el PIB per cápita de los Estados europeos de la tabla 1.6.

stem(ejercicio_1_36$PIB_per_capita.dólares.)

"
  0 | 011111112222233334
  0 | 7789
  1 | 34
  1 | 5899
  2 | 023344
  2 | 555668
  3 | 
  3 | 5
  4 | 0
"

# (d) ¿Cuál es la forma de la distribución? Recuerda que debes empezar
# siempre representando gráficamente tus datos —los resúmenes numéricos no son
# una descripción completa—

"El grafico presenta una forma asimétrica a la derecha, lo que significa que el PIB esta a valores pequeños. 
Su centro de ubica cercano a los 8000, ademas de haberlo calculado por computasora, lo podemos ver en la forma
, y si excluimos los valores de la cola la dispersión va desde los < 1000 hasta los  25000"

# **************************************************
# PREGUNTA 1.37 - Salarios en empresa de consultoría
# **************************************************

# El año pasado una pequeña empresa de consultoría pagó a cada uno de sus
# cinco administrativos 22.000 € y a los dos titulados universitarios, 50.000. Final
# mente, el propietario de la empresa cobró 270.000 €.

# (a) ¿Cuál es el salario medio pagado en esta empresa?

salarios_administrativos <- 22000 * 5
salarios_universitarios <- 50000 * 2
salario_propietario <- 270000

total_salarios <- sum(salario_propietario,salarios_universitarios,salarios_administrativos)
total_empleado <- 8

promedio_sueldos <- total_salarios / total_empleado

cat("El salario promedio de la empresa es de:",promedio_sueldos,"euros")

# (b) ¿Cuántos empleados ganan menos de la media?

"Con excepción del propietario, todos ganan menos de la media"

# (c) ¿Cuál es el salario mediano?

sueldos <-  c(22000,22000,22000,22000,22000,50000,50000,270000)

cat("El salario medio de la empresa es de:",median(sueldos),"euros")

# **************************************************
# PREGUNTA 1.38 - Elecciones presidenciales (EEUU)
# **************************************************

# El porcentaje de votos que obtuvo cada uno de los candidatos a la presidencia
# de EE UU que ganó las elecciones desde 1948 hasta 1996 es el siguiente:

# Año  Porcentaje   Año  Porcentaje
# --------------------------------
# 1948  49,6        1976  50,1
# 1952  55,1        1980  50,7
# 1956  57,4        1984  58,8
# 1960  49,7        1988  53,9
# 1964  61,1        1992  43,2
# 1968  43,4        1996  49,2
# 1972  60,7

# (a) Dibuja un diagrama de tallos correspondiente a estos porcentajes. (Redondea
# las cifras y utiliza un diagrama de tallos divididos.)

ejercicio_1_38 <- read.csv("ejercicio_1_38_elecciones.csv")

View(ejercicio_1_38)

stem(ejercicio_1_38$Porcentaje)


# (b) ¿Cuál es la mediana del porcentaje de votos obtenidos por los candidatos
# que tuvieron éxito en las elecciones presidenciales? (Trabaja con los datos sin
# redondear.)

median(ejercicio_1_38$Porcentaje)

cat("La mediana de los votos obtienidos es:", median(ejercicio_1_38$Porcentaje), "votos")

# (c) Consideraremos que fueron elecciones con victorias aplastantes aquellas
# en las que los porcentajes de votos se sitúan a partir del tercer cuartil. Hállalo.
# ¿En qué años se obtuvieron victorias aplastantes?

 q3 <- quantile(ejercicio_1_38$Porcentaje, 0.75)
 
 victorias_aplastantes <- ejercicio_1_38[ejercicio_1_38$Porcentaje >= q3, ]
 
 print(victorias_aplastantes)
 
"
Año        Porcentaje
1956       57.4
1964       61.1
1972       60.7
1984       58.8
"

# **************************************************
# PREGUNTA 1.39 - Calorías en salchichas
# **************************************************

# Análisis de contenido calórico de salchichas:

# SALCHICHAS DE TERNERA:
# Mean = 156.8   Standard deviation = 22.64
# Min = 111      Max = 190
# N = 20         Median = 152.5
# Quartiles = 140, 178.5

# SALCHICHAS DE CERDO:
# Mean = 158.7   Standard deviation = 25.24
# Min = 107      Max = 195
# N = 17         Median = 153
# Quartiles = 139, 179

# SALCHICHAS DE POLLO:
# Mean = 122.5   Standard deviation = 25.48
# Min = 87       Max = 170
# N = 17         Median = 129
# Quartiles = 102, 143

# (a) Utiliza esta información para dibujar, en un mismo gráfico, tres diagramas
# de caja con los recuentos de calorías de los tres tipos de salchichas.

informacion_salchichas <- list(
  "Ternera" = c(111, 140, 152.5, 178.5, 190),
  "Cerdo" = c(107,139,153,179,195),
  "Pollo" = c(87,102,129,143,170)
)


boxplot(informacion_salchichas,
        main = "Calorías en salchichas",
        xlab = "Tipo de salchicha",
        ylab = "Cantidad de calorias",
        col = c("lightblue", "lightgreen","red"),
        border = c("blue", "darkgreen","darkred"))

# (b) Describe brevemente las diferencias que observes en las tres distribuciones.

"Las salchichas de pollo son significativamente más bajas en calorías (mediana de 129) 
y más variables que las demás. Las de ternera y cerdo tienen contenidos calóricos muy 
similares (medianas de 152.5 y 153 respectivamente), pero las de cerdo muestran una 
ligera mayor dispersión. En general, el pollo es la opción más light, mientras que 
ternera y cerdo ofrecen valores energéticos comparables pero con cerdo siendo ligeramente 
más inconsistente."

# (c) Comer salchichas hechas con carne de pollo, ¿significa ingerir menos calorías
# que comer las hechas con carne de ternera o de cerdo?

"Las salchichas de pollo tienen una mediana de 129 calorías, mientras que las de ternera 
y cerdo tienen medianas de 152.5 y 153 calorías respectivamente. Esto significa que, en general, 
las salchichas de pollo aportan aproximadamente 20-25 calorías menos por porción que las de ternera o cerdo.

Además, el rango intercuartílico del pollo (102-143) es consistentemente más bajo que 
el de ternera (140-178.5) y cerdo (139-179), lo que confirma que la mayoría de las salchichas 
de pollo tienen menos calorías que la mayoría de las de ternera o cerdo.

Conclusión: Elegir salchichas de pollo significa ingerir menos calorías en la mayoría de los casos"

# **************************************************
# PREGUNTA 1.40 - % PIB destinado a educación
# **************************************************

# La columna "%PIB en educación pública" de la tabla 1.6 proporciona el porcentaje
# del PIB de los Estados europeos dedicado a educación pública.

# (a) Haz una lista (con los valores ordenados) de los datos del porcentaje del
# PIB destinado a educación pública de los Estados de la Unión Europea y otra
# lista con los datos de los Estados del Este.

Educacion_UE <- ejercicio_1_36 %>%
  filter(Región == "UE") %>%
  filter(!is.na(X.PIB_educacion)) %>%
  select(Estado,X.PIB_educacion) %>%
  arrange(desc(X.PIB_educacion))

Educacion_EE <- ejercicio_1_36 %>%
  filter(Región == "EE") %>%
  filter(!is.na(X.PIB_educacion)) %>%
  select(Estado,X.PIB_educacion) %>%
  arrange(desc(X.PIB_educacion))


# (b) Dibuja los gráficos y calcula resúmenes numéricos para comparar ambas
# distribuciones. Describe brevemente lo que observas.

summary(Educacion_UE$X.PIB_educacion)
summary(Educacion_EE$X.PIB_educacion)

boxplot(Educacion_UE$X.PIB_educacion,
        Educacion_EE$X.PIB_educacion,
        main = "% PIB destinado a educación",
        xlab = "Bloque de países",
        ylab = "PIB destinado",
        names = c("Unión Europea", "Europa del Este"),
        col = c("lightblue", "lightgreen"),
        border = c("blue", "darkgreen"))

"
Bloque Unión Europea:

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.100   5.125   5.800   5.957   6.950   8.400 

Bloque Europa del este:

   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
  3.600   5.150   5.800   5.573   6.150   6.700 


Aunque ambos bloques comparten la misma mediana (5.8%), la Unión Europea muestra 
una mayor inversión promedio (5.96% vs 5.57%) y una dispersión significativamente 
más amplia, con países que llegan a invertir hasta 8.4% del PIB. Europa del Este 
presenta una distribución más homogénea y compacta, con un rango máximo más bajo (6.7%), 
sugiriendo políticas educativas más estandarizadas en la región.
"

# **************************************************
# PREGUNTA 1.41 - Densidad de la Tierra (Cavendish)
# **************************************************

# En 1798 el científico inglés Henry Cavendish determinó la densidad de la Tierra
# con mucha precisión. He aquí los resultados que obtuvo (expresados como
# múltiplo de la densidad del agua):

# Datos: 5,50 5,61 4,88 5,07 5,26 5,55 5,36 5,29 5,58 5,65
#        5,57 5,53 5,62 5,29 5,44 5,34 5,79 5,10 5,27 5,39
#        5,42 5,47 5,63 5,34 5,46 5,30 5,75 5,68 5,85

# (a) Representa gráficamente los datos de la manera que consideres más conveniente.
# (b) La forma de la distribución, ¿permite utilizar ¯x y s para describirla?
# (c) Halla ¯x y s.
# (d) Teniendo en cuenta todo lo que acabas de hacer, ¿cuál es tu estimación de
#     la densidad de la Tierra a partir de estas mediciones?

# **************************************************
# PREGUNTA 1.42 - ¯x y s no son suficientes
# **************************************************

# Conjuntos de datos de distintas formas pueden tener la misma media y desviación
# típica.

# DatosA: 9,14 8,14 8,74 8,77 9,26 8,10 6,13 3,10 9,13 7,26 4,74
# DatosB: 6,58 5,76 7,71 8,84 8,47 7,04 5,25 5,56 7,91 6,89 12,50

# (a) Utiliza tu calculadora y halla ¯x y s de los siguientes conjuntos de datos.
# (b) A continuación dibuja un diagrama de tallos de cada uno de ellos.
# (c) Comenta la forma de cada distribución.

# **************************************************
# PREGUNTA 1.43 - Porcentaje de gente mayor de 65 años (EEUU)
# **************************************************

# La tabla 1.1 facilita datos sobre el porcentaje de gente con al menos 65 años
# en cada uno de los Estados de EE UU. La figura 1.2 es un histograma correspondiente
# a estos datos.

# (a) Como descripción numérica breve, ¿qué prefieres, los cinco números resumen o ¯x y s? ¿Por qué?
# (b) Calcula la descripción que prefieras.

# **************************************************
# PREGUNTA 1.44 - Rendimientos de acciones Philip Morris
# **************************************************

# La tabla 1.8 proporciona los rendimientos, expresados como porcentajes mensuales,
# de las acciones de Philip Morris en un periodo comprendido entre el mes de
# julio de 1990 y el mes de mayo de 1997.

# Datos: −5,7 1,2 4,1 3,2 7,3 7,5 18,6 3,7 −1,8 2,4
#        −6,5 6,7 9,4 −2 −2,8 −3,4 19,2 −4,8 0,5 −0,6
#        2,8 −0,5 −4,5 8,7 2,7 4,1 −10,3 4,8 −2,3 −3,1
#        −10,2 −3,7 −26,6 7,2 −2,9 −2,3 3,5 −4,6 17,2 4,2
#        0,5 8,3 −7,1 −8,4 7,7 −9,6 6 6,8 10,9 1,6
#        0,2 −2,4 −2,4 3,9 1,7 9 3,6 7,6 3,2 −3,7
#        4,2 13,2 0,9 4,2 4 2,8 6,7 −10,4 2,7 10,3
#        5,7 0,6 −14,2 1,3 2,9 11,8 10,6 5,2 13,8 −14,7
#        3,5 11,7 1,3

# (a) Dibuja un diagrama de tallos o un histograma con estos datos. ¿Cómo has
#     decidido qué representación gráfica utilizar?
# (b) Existe una clara observación atípica. ¿Cuál es el valor de esta observación?
# (c) Después de eliminar la observación atípica, describe la forma, el centro y
#     la dispersión de los datos.
# (d) Halla la media y la desviación típica de los rendimientos.
# (e) Si invirtieras 100€ en estas acciones al comienzo de un mes y obtuvieras
#     el rendimiento medio, ¿cuánto tendrías al final del mes?
# (f) Si invirtieras 100€ en estas acciones al comienzo del peor mes (la observación
#     atípica), ¿cuánto tendrías al final del mes?
# (g) Halla otra vez la media y la desviación típica, pero dejando fuera la observación
#     atípica. ¿En qué medida afecta esta observación atípica los valores de
#     la media y de la desviación típica?
# (h) La eliminación de esta observación atípica, ¿cambiaría el valor de la mediana?
#     ¿Y los cuartiles? Sin hacer los cálculos, ¿cómo lo puedes saber?

# **************************************************
# PREGUNTA 1.45 - Salarios de atletas (Baltimore Orioles)
# **************************************************

# Los jugadores del equipo de béisbol de los Orioles de Baltimore en EEUU
# fueron los mejor pagados durante la liga estadounidense de 1998.

# Salarios (en miles de dólares): 
# 6.495 6.486 6.300 6.269 5.442 5.391 3.600 3.600 3.583
# 3.089 2.850 2.500 1.950 1.663 1.367 1.333 1.150 900
# 856 800 800 665 650 450 450 170 170

# **************************************************
# PREGUNTA 1.46 - Valor neto de un patrimonio
# **************************************************

# En 1997 el valor medio y la mediana de los patrimonios europeos eran de
# 51.000 y 212.000 €, respectivamente.

# (a) ¿Cuál de estos valores corresponde a la media? ¿Y a la mediana?
# (b) Justifica tus respuestas.

# **************************************************
# PREGUNTA 1.47 - Salarios millonarios de la NBA
# **************************************************

# De los 411 jugadores de la NBA (National Basketball Association) sólo 139
# ganaban más de 2,36 millones de dólares.

# (a) ¿2,36 es la media o la mediana de las ganancias de los jugadores?
# (b) ¿Por qué?

# **************************************************
# PREGUNTA 1.48 - ¿Media o mediana?
# **************************************************

# En cada una de las situaciones siguientes, ¿qué medida de centro deberías utilizar,
# la media o la mediana?

# (a) El Ayuntamiento de Barcelona está considerando la posibilidad de aplicar
#     un nuevo impuesto sobre los ingresos de los hogares de la ciudad. Para ello,
#     quiere conocer los ingresos totales de los hogares.
# (b) En un estudio sobre el nivel de vida de los barceloneses, un sociólogo
#     quiere conocer la renta típica de un hogar de la ciudad.

# **************************************************
# PREGUNTA 1.49 - Ejercicio sobre desviación típica
# **************************************************

# Debes escoger cuatro números entre el 0 y el 10 (se pueden escoger números
# repetidos) de manera que:

# (a) La desviación típica de estos números sea la más pequeña posible.
# (b) La desviación típica de estos números sea la mayor posible.
# (c) ¿Hay más de una posibilidad en (a) y (b)? Justifica tu respuesta.

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::
