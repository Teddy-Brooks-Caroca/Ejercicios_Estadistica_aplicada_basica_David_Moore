# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# SECCIÓN 2.6 "Relaciones entre variables categóricas"

# **************************************************
# PREGUNTA 2.68 - Análisis de tablas de contingencia
# **************************************************

# Los recuentos de la columna "Total" situada a la derecha de la tabla 2.10
# son recuentos de gente en cada grupo educativo.

# (a) Explica por qué la suma de estos recuentos no es igual a 166.438, 
# el total que aparece a la derecha de la última fila de la tabla.

"La columna 'Total' representa la distribución MARGINAL por nivel educativo (suma de cada fila), 
mientras que 166,438 es el TOTAL GENERAL de personas en todos los grupos educativos y todas las 
edades combinadas."

# **************************************************
# PREGUNTA 2.69 - Distribución marginal de la edad
# **************************************************

# A partir de los recuentos de la tabla 2.10:

# (a) Halla, en forma de porcentajes, la distribución marginal de la edad 
# para la gente mayor de 25 años.

total_poblacion <- 166438

grupo_25_a_34 <- 41388 / total_poblacion

grupo_35_a_54 <- 73028 / total_poblacion

grupo_55_a_mas <- 52022 / total_poblacion

resultados <- data.frame(
  Grupo_etario = c("25 a 34", "35 a 54", "55 a más"),
  Porcentajes = c(paste0(round(grupo_25_a_34 * 100, 2), "%"),
                  paste0(round(grupo_35_a_54 * 100, 2), "%"),
                  paste0(round(grupo_55_a_mas * 100, 2), "%"))
)

print(resultados)

"
  Grupo_etario Porcentajes
1      25 a 34      24.87%
2      35 a 54      43.88%
3      55 a más     31.26%
"
# **************************************************
# PREGUNTA 2.70 - Hábitos fumadores de padres e hijos
# **************************************************

# Tenemos datos de ocho escuelas de secundaria sobre el consumo de tabaco 
# entre los estudiantes y entre sus padres.

# TABLA DE DATOS:
# Hábitos de los padres    Estudiantes fumadores    Estudiantes no fumadores
# -------------------------------------------------------------------------
# Los dos padres fuman                      400                       1.380
# Sólo uno de los padres fuma               416                       1.823  
# Ninguno de los dos padres fuma            188                       1.168

# (a) ¿A cuántos estudiantes describen estos datos?

consumo_tabaco_estudiantes <- data.frame(
  Habitos_de_los_padres = c("Los dos padres fuman","Solo uno de los padres fuma","Ninguno de los padres fuma"),
  Estudiantes_fumadores = c(400,416,188),
  Estudiantes_no_fumadores = c(1380,1823,1168)
)

print(consumo_tabaco_estudiantes)

total_estudiantes_fumadores <- sum(consumo_tabaco_estudiantes$Estudiantes_fumadores)
total_estudiantes_no_fumadores <- sum(consumo_tabaco_estudiantes$Estudiantes_no_fumadores)
total_de_alumno <- total_estudiantes_fumadores + total_estudiantes_no_fumadores

totales_alumnos <- data.frame(
  Tipo_estudiante = c("Estudiante fumador","Estudiante no fumador","TOTAL DE ALUMNOS"),
  Total = c(total_estudiantes_fumadores,total_estudiantes_no_fumadores, total_de_alumno)
)

print(totales_alumnos)

"
        Tipo_estudiante Total
1    Estudiante fumador  1004
2 Estudiante no fumador  4371
3      TOTAL DE ALUMNOS  5375
"

# (b) ¿Qué porcentaje de estos estudiantes son fumadores?

porcentaje <- round((total_estudiantes_fumadores / total_de_alumno) * 100,2)

print(paste0("El porcentaje de estudiantes que son fumadores es:",porcentaje,"%"))

"El porcentaje de estudiantes que son fumadores es:18.68%"

# (c) Halla la distribución marginal del consumo de tabaco de los padres de dos
# maneras, con recuentos y en porcentajes

consumo_tabaco_estudiantes$Total_por_habito_padres <- 
  consumo_tabaco_estudiantes$Estudiantes_fumadores + 
  consumo_tabaco_estudiantes$Estudiantes_no_fumadores

consumo_tabaco_estudiantes$Porcentaje_habito_padres <- 
  round((consumo_tabaco_estudiantes$Total_por_habito_padres / total_de_alumno) * 100, 2)

distribucion_marginal <- data.frame(
  Habitos_padres = consumo_tabaco_estudiantes$Habitos_de_los_padres,
  Recuento = consumo_tabaco_estudiantes$Total_por_habito_padres,
  Porcentaje = paste0(consumo_tabaco_estudiantes$Porcentaje_habito_padres, "%")
)

print(distribucion_marginal)

"
               Habitos_padres Recuento Porcentaje
1        Los dos padres fuman     1780     33.12%
2 Solo uno de los padres fuma     2239     41.66%
3  Ninguno de los padres fuma     1356     25.23%
"

# **************************************************
# PREGUNTA 2.71 - Análisis de educación por grupo de edad
# **************************************************

# Utilizando los recuentos de la tabla 2.10:

# (a) Halla el porcentaje de gente de cada grupo de edad que no terminó la secundaria.

datos_educacion <- data.frame(
  Educacion = c("No completaron secundaria", 
                "Completaron secundaria",
                "De 1 a 3 cursos en la universidad", 
                "4 o más cursos en la universidad",
                "Total"),
  Grupo_25_34 = c(5325,14061,11659,10342,41388),
  Grupo_35_54 = c(9152,24070,19926,19878,73028),
  Grupo_55_mas = c(16035,18320,9662,8005,52022),
  Total = c(30512,56451,41247,38225,166438)
)

write.csv(datos_educacion,"ejercicio_2_71_educacion.csv")

print(datos_educacion)

grupo_25_34_tramo1 <- datos_educacion[1,2] / datos_educacion[5,2]
grupo_35_54_tramo1 <- datos_educacion[1,3] / datos_educacion[5,3]
grupo_55_mas_tramo1 <- datos_educacion[1,4] / datos_educacion[5,4]

porcentaje_25_34_tramo1 <- paste0(round(grupo_25_34_tramo1 * 100,2),"%")
porcentaje_35_54_tramo1 <- paste0(round(grupo_35_54_tramo1 * 100,2),"%")
porcentaje_55_mas_tramo1 <- paste0(round(grupo_55_mas_tramo1 * 100,2),"%")

porcentajes_totales <- data.frame(
  Grupo_etario = c("25 a 34 años","35 a 54 años", "55 a más años"),
  Porcentaje = c(porcentaje_25_34_tramo1,porcentaje_35_54_tramo1,porcentaje_55_mas_tramo1)
)

print(porcentajes_totales)

"
Grupo_etario   Porcentaje
1 25 a 34 años     12.87%
2 35 a 54 años     12.53%
3 55 a más años    30.82%
"

# (b) Dibuja un diagrama de barras para comparar estos porcentajes.

porcentajes_numericos <- c(grupo_25_34_tramo1 * 100, 
                           grupo_35_54_tramo1 * 100, 
                           grupo_55_mas_tramo1 * 100)

nombres_grupos <- c("25 a 34", "35 a 54", "55 a más")

barplot(porcentajes_numericos, 
        names.arg = nombres_grupos,
        main = "Porcentaje que no completó secundaria por grupo de edad",
        xlab = "Grupo de edad", 
        ylab = "Porcentaje (%)",
        col = "lightblue",
        ylim = c(0, 35))

# (c) Explica lo que muestran los datos.

"El 30.82% de las personas de 55+ años no completó la secundaria, una proporción 
mucho mayor que en los grupos más jóvenes (12.87% en 25-34 años y 12.53% en 35-54 años). 
Esto muestra una dramática mejoria en el acceso a la educación a través de las generaciones."

# **************************************************
# PREGUNTA 2.72 - Huevos de serpientes de agua
# **************************************************

# ¿Cómo influye la temperatura sobre la eclosión de los huevos de serpiente de agua? 
# Unos investigadores distribuyeron huevos recién puestos a tres temperaturas: 
# caliente, templada y fría.

# DATOS:
# Temperatura   Número de huevos   Huevos eclosionados
# ---------------------------------------------------
# Fría          27                 16
# Templada      56                 38
# Caliente      104                75

# (a) Construye una tabla de contingencia con la temperatura y el resultado de
# la eclosión (sí o no).

matrix_huevos <- matrix(c(27,16,11,
                          56,38,18,
                          104,75,29),
                        nrow = 3,ncol = 3,
                        byrow = TRUE)
rownames(matrix_huevos) = c("Fría","Templada","Caliente")
colnames(matrix_huevos) = c("Número de huevos","Eclosionados","No eclosionados")

print(matrix_huevos)

"
         Número de huevos Eclosionados No eclosionados
Fría                   27           16              11
Templada               56           38              18
Caliente              104           75              29
"

# (b) Calcula el porcentaje de huevos de cada grupo que eclosionó. Los investigadores 
# opinaban que los huevos no eclosionarían en agua fría. Los datos, ¿apoyan esta opinión?

eclosion_fria <- matrix_huevos[1,2] / matrix_huevos[1, 1]

eclosion_templada <- matrix_huevos[2,2]/ matrix_huevos[2,1]

eclosion_caliente <- matrix_huevos[3,2] / matrix_huevos[3,1]

porcentaje_frio <- paste0(round(eclosion_fria * 100,2),"%")

porcentaje_templado <- paste0(round(eclosion_templada * 100,2),"%")

porcentaje_caliente <- paste0(round(eclosion_caliente * 100,2),"%")

resultados_eclosion <- data.frame(
  Temperatura = c("Fría","Templada","Caliente"),
  Porcentaje_eclosion = c(porcentaje_frio,porcentaje_templado,porcentaje_caliente)
)

print(resultados_eclosion)

"
  Temperatura Porcentaje_eclosion
1        Fría              59.26%
2    Templada              67.86%
3    Caliente              72.12%

===================================================

Los datos NO apoyan la opinión de los investigadores. Si bien la eclosión es significativamente 
menor en agua fría (59.26%) comparada con temperaturas más cálidas (67.86% templada, 72.12% caliente), 
más de la mitad de los huevos SÍ eclosionaron en agua fría. Esto contradice la hipótesis de que los 
huevos 'no eclosionarían' en agua fría, aunque confirma que la temperatura fría reduce la tasa de eclosión."

# **************************************************
# PREGUNTA 2.73 - Distribución condicional de la edad
# **************************************************

# Halla la distribución condicional de la edad entre la gente con al menos
# 4 cursos universitarios. Parte de los recuentos de la tabla 2.10. 
# (Para hacerlo, fíjate sólo en la fila de "4 o más cursos en la universidad" de la tabla.)

tramo_25_34 <- datos_educacion[4,2] / datos_educacion[4,5]
tramo_35_54 <- datos_educacion[4,3] / datos_educacion[4,5]
tramo_55_mas <- datos_educacion[4,4] / datos_educacion[4,5]

porcentaje_25_34_tramo4 <- paste0(round(tramo_25_34 * 100,2),"%")
porcentaje_35_54_tramo4 <- paste0(round(tramo_35_54 * 100,2),"%")
porcentaje_55_mas_tramo4 <- paste0(round(tramo_55_mas * 100,2),"%")

porcentajes_totales <- data.frame(
  Grupo_etario = c("25 a 34 años","35 a 54 años", "55 a más años"),
  Porcentaje = c(porcentaje_25_34_tramo4,porcentaje_35_54_tramo4,porcentaje_55_mas_tramo4)
)

print(porcentajes_totales)

"
   Grupo_etario  Porcentaje
1  25 a 34 años      27.06%
2  35 a 54 años         52%
3 55 a más años      20.94%
"
# **************************************************
# PREGUNTA 2.74 - Planes profesionales de hombres y mujeres
# **************************************************

# Un estudio sobre los planes profesionales de mujeres y hombres jóvenes envió 
# cuestionarios a los 722 alumnos de una clase de último curso de Administración 
# de Empresas de la University of Illinois.

# TABLA DE DATOS:
# Especialidad      Mujeres   Hombres
# ----------------------------------
# Contabilidad      68        56
# Administración    91        40
# Economía          5         61
# Finanzas          6         59

# (a) Halla la distribución de la especialidad condicionada al sexo de los estudiantes. 
# A partir de tus resultados describe las diferencias entre hombres y mujer con un gráfico 
# y con palabras.

tabla_especialidad <- data.frame(
  Especialidad = c("Contabilidad","Administración","Economía","Finanzas"),
  Mujeres = c(68,91,5,6),
  Hombres = c(56,40,61,59)
)

print(tabla_especialidad)

total_contabilidad <- tabla_especialidad[1,2] + tabla_especialidad[1,3]
total_administracion <- tabla_especialidad[2,2] + tabla_especialidad[2,3]
total_economia <- tabla_especialidad[3,2] + tabla_especialidad[3,3]
total_finanzas <- tabla_especialidad[4,2] + tabla_especialidad[4,3]

mujeres_contabilidad <- tabla_especialidad[1,2] / total_contabilidad
mujeres_administracion <- tabla_especialidad[2,2] / total_administracion
mujeres_economia <- tabla_especialidad[3,2] / total_economia
mujeres_finanzas <- tabla_especialidad[4,2] / total_finanzas

hombres_contabilidad <- tabla_especialidad[1,3] / total_contabilidad
hombres_administracion <- tabla_especialidad[2,3] / total_administracion
hombres_economia <- tabla_especialidad[3,3] / total_economia
hombres_finanzas <- tabla_especialidad[4,3] / total_finanzas

porcentaje_mujeres_contabilidad <- paste0(round(mujeres_contabilidad * 100,2),"%")
porcentaje_mujeres_administracion <- paste0(round(mujeres_administracion * 100,2),"%")
porcentaje_mujeres_economia <- paste0(round(mujeres_economia * 100,2),"%")
porcentaje_mujeres_finanzas <- paste0(round(mujeres_finanzas * 100,2),"%")

porcentaje_hombres_contabilidad <- paste0(round(hombres_contabilidad * 100,2),"%")
porcentaje_hombres_administracion <- paste0(round(hombres_administracion * 100,2),"%")
porcentaje_hombres_economia <- paste0(round(hombres_economia * 100,2),"%")
porcentaje_hombres_finanzas <- paste0(round(hombres_finanzas * 100,2),"%")

tabla_especialidad_porcentajes <- data.frame(
  Especialidad = c("Contabilidad","Administración","Economía","Finanzas"),
  Mujeres = c(porcentaje_mujeres_contabilidad,porcentaje_mujeres_administracion,
              porcentaje_mujeres_economia,porcentaje_mujeres_finanzas),
  Hombres = c(porcentaje_hombres_contabilidad,porcentaje_hombres_administracion,
              porcentaje_hombres_economia,porcentaje_hombres_finanzas)
)

print(tabla_especialidad_porcentajes)

porcentajes_mujeres <- c(54.84, 69.47, 7.58, 9.23)
porcentajes_hombres <- c(45.16, 30.53, 92.42, 90.77)

barplot(rbind(porcentajes_mujeres, porcentajes_hombres),
        beside = TRUE,
        names.arg = tabla_especialidad$Especialidad,
        col = c("pink", "lightblue"),
        main = "Distribución de especialidades por sexo",
        ylab = "Porcentaje (%)",
        legend.text = c("Mujeres", "Hombres"))

"
    Especialidad Mujeres Hombres
1   Contabilidad  54.84%  45.16%
2 Administración  69.47%  30.53%
3       Economía   7.58%  92.42%
4       Finanzas   9.23%  90.77%

====================================================

Las mujeres predominan en Administración (69.47%) y Contabilidad (54.84%), mientras 
los hombres son mayoría abrumadora en Economía (92.42%) y Finanzas (90.77%), mostrando 
una clara segregación por género en las especialidades de negocios.
"

# (b) ¿Qué porcentaje de estudiantes no respondió el cuestionario? La falta de
# respuesta debilita los resultados obtenidos.

total_alumnado <- 722

total_respondieron <- total_contabilidad + total_administracion + total_economia + total_finanzas

total_no_respondieron <- total_alumnado - total_respondieron

porcentaje_respondio <- paste0(round((total_respondieron / total_alumnado) * 100,2),"%")

porcentaje_no_respondio <- paste0(round((total_no_respondieron / total_alumnado) * 100,2),"%")

porcentajes <- data.frame(
  Estudiantes = c("Respondieron", "No respondieron"),
  Porcentajes = c(porcentaje_respondio,porcentaje_no_respondio)
)

print(porcentajes)

"
     Estudiantes  Porcentajes
1    Respondieron      53.46%
2 No respondieron      46.54%
"

# **************************************************
# PREGUNTA 2.75 - Tablas de contingencia 2x2
# **************************************************

# He aquí los totales de filas y columnas de una tabla de contingencia con dos
# filas y dos columnas.

#         Col1   Col2   Total
# Fila1    a      b      50
# Fila2    c      d      50
# Total    60     40     100

# (a) Halla dos diferentes conjuntos de recuentos a, b, c y d que den los mismos totales. 
# Este ejercicio muestra que la relación entre dos variables no se puede obtener a partir 
# de las distribuciones individuales de las variables.

# Conjunto 1
df1 <- data.frame(
  Fila = c("Fila1", "Fila2", "Total"),
  Col1 = c(30, 30, 60),
  Col2 = c(20, 20, 40),
  Total = c(50, 50, 100)
)

# Conjunto 2  
df2 <- data.frame(
  Fila = c("Fila1", "Fila2", "Total"),
  Col1 = c(40, 20, 60),
  Col2 = c(10, 30, 40),
  Total = c(50, 50, 100)
)

print("CONJUNTO 1:")
print(df1)
print("CONJUNTO 2:")
print(df2)  

# **************************************************
# PREGUNTA 2.76 - Retrasos en los aeropuertos
# **************************************************

# He aquí el número de vuelos que llegaron a la hora prevista y el número de vuelos 
# que llegaron con retraso de dos compañías aéreas en cinco aeropuertos de EE UU en 
# un determinado mes. A menudo,los medios de comunicación dan a conocer los porcentajes 
# de vuelos,de las distintas compañías, que llegan a la hora. El aeropuerto de procedencia 
# es una variable latente que puede hacer que los datos que dan los medios de comunicación sean
# engañosos.

# TABLA DE DATOS:
# Aeropuerto      Alaska Airlines       America West
#                 A hora   Retraso     A hora   Retraso
# -----------------------------------------------------
# Los Angeles     497      62          694      117
# Phoenix         221      12          4840     415
# San Diego       212      20          383      65
# San Francisco   503      102         320      129
# Seattle         1841     305         201      61

# (a) ¿Qué porcentaje de vuelos de Alaska Airlines llegan con retraso? ¿Qué
# porcentaje de vuelos de America West llegan con retraso? Estos son los datos
# que, en general, dan a conocer los medios de comunicación.

tabla_vuelos <- data.frame(
  Aeropuerto = c("Los Angeles","Phoenix","San Diego","San Francisco","Seattle"),
  AA_a_la_hora = c(497,221,212,503,1841),
  AA_retraso = c(62,12,20,102,305),
  AW_a_la_hora = c(694,4840,383,320,201),
  AW_retraso = c(117,415,65,129,61)
)

print(tabla_vuelos)

total_retrasados_AA <- sum(tabla_vuelos$AA_retraso) 
total_retrasados_AW <- sum(tabla_vuelos$AW_retraso) 

total_vuelos_AA <- sum(tabla_vuelos$AA_a_la_hora) + total_retrasados_AA
total_vuelos_AW <- sum(tabla_vuelos$AW_a_la_hora) + total_retrasados_AW

porcentaje_retraso_AA <- (total_retrasados_AA / total_vuelos_AA) * 100
porcentaje_retraso_AW <- (total_retrasados_AW / total_vuelos_AW) * 100

porcentajes_retrasos <- data.frame(
  Linea_aerea = c("Alaska Airlines","America West"),
  Porcentaje_retraso = c(paste0(round(porcentaje_retraso_AA, 2), "%"),
                         paste0(round(porcentaje_retraso_AW, 2), "%"))
)

print(porcentajes_retrasos)

"
      Linea_aerea Porcentaje_retraso
1 Alaska Airlines             13.27%
2    America West             10.89%
"

# (b) Ahora considera los datos de cada aeropuerto por separado, ¿qué porcen
# taje de vuelos de Alaska Airlines llegan con retraso? ¿Y de America West?

porcentaje_AA_por_aeropuerto <- (tabla_vuelos$AA_retraso / (tabla_vuelos$AA_a_la_hora + tabla_vuelos$AA_retraso)) * 100
porcentaje_AW_por_aeropuerto <- (tabla_vuelos$AW_retraso / (tabla_vuelos$AW_a_la_hora + tabla_vuelos$AW_retraso)) * 100

resultados_por_aeropuerto <- data.frame(
  Aeropuerto = tabla_vuelos$Aeropuerto,
  AA_retraso = paste0(round(porcentaje_AA_por_aeropuerto, 2), "%"),
  AW_retraso = paste0(round(porcentaje_AW_por_aeropuerto, 2), "%")
)

print(resultados_por_aeropuerto)

"
     Aeropuerto AA_retraso AW_retraso
1   Los Angeles     11.09%     14.43%
2       Phoenix      5.15%       7.9%
3     San Diego      8.62%     14.51%
4 San Francisco     16.86%     28.73%
5       Seattle     14.21%     23.28%
"

# (c) Considerando los aeropuertos por separado, America West es la peor com
# pañía. Sin embargo, considerando todos los aeropuertos conjuntamente es la me
# jor. Parece una contradicción. Explica cuidadosamente, basándote en los datos,
# cómo se puede explicar. (Los climas de Phoenix y Seattle pueden explicar este
# ejemplo de paradoja de Simpson.)

"America West concentra sus operaciones en Phoenix (5,255 vuelos), donde las condiciones 
climáticas son favorables y los retrasos son menores. En cambio, Alaska Airlines opera 
principalmente desde Seattle (2,146 vuelos), donde el clima es más adverso y causa más retrasos. 
Aunque Alaska tiene mejor desempeño en cada aeropuerto individual, el hecho de que America West 
opere predominantemente en aeropuertos 'fáciles' hace que su promedio general sea mejor."

# **************************************************
# PREGUNTA 2.77 - Raza y condena a muerte
# **************************************************

# El hecho de que un acusado de asesinato sea condenado o no a muerte parece 
# estar influenciado por la raza de la víctima. Tenemos datos de 326 casos en 
# los que el acusado fue declarado culpable de asesinato:

# TABLA DE DATOS:
#                 Acusado blanco                      Acusado negro
#                 Pena muerte                         Pena muerte  
#
#                  Si      No                          Si     No
# ----------------------------------------------------------------
# Víctima blanca: 19      132          Víctima blanca: 11    52
# Víctima negra:  0       9            Víctima negra:  6     97

# (a) Utiliza estos datos para construir una tabla de contingencia que relacione
# la raza del acusado (blanco o negro) con la pena de muerte (sí o no).

datos_acusados <- data.frame(
  Acusado = c(rep("Blanco", 4), rep("Negro", 4)),
  Victima = rep(c("Blanca", "Blanca", "Negra", "Negra"), 2),
  Pena = rep(c("Si", "No", "Si", "No"), 2),
  Conteo = c(19, 132, 0, 9, 11, 52, 6, 97)
)

tabla_contingencia <- with(datos_acusados, 
                           tapply(Conteo, list(Acusado, Pena), sum)
)

print(tabla_contingencia)

"
        No Si
Blanco 141 19
Negro  149 17
"
# (b) Constata que se cumple la paradoja de Simpson: en conjunto, un mayor
# porcentaje de acusados blancos son condenados a pena de muerte; en cambio,
# considerando de manera independiente a las víctimas blancas y a las negras, el
# porcentaje de acusados negros condenados a muerte es mayor que el de blancos.

# --- Porcentaje total de pena de muerte ---
total_por_acusado <- rowSums(tabla_contingencia)
porcentaje_total <- tabla_contingencia[, "Si"] / total_por_acusado * 100

# --- Porcentaje según raza de víctima ---
# Creamos una tabla 3D (Acusado x Victima x Pena)
tabla_victima <- with(datos_acusados, tapply(Conteo, list(Acusado, Victima, Pena), sum))

# Víctima blanca
porc_victima_blanca <- tabla_victima[, "Blanca", "Si"] / 
  (tabla_victima[, "Blanca", "Si"] + tabla_victima[, "Blanca", "No"]) * 100

# Víctima negra
porc_victima_negra <- tabla_victima[, "Negra", "Si"] / 
  (tabla_victima[, "Negra", "Si"] + tabla_victima[, "Negra", "No"]) * 100

# --- Unimos todos los porcentajes en un solo data frame ---
resumen <- data.frame(
  Analisis = c("Total", "Víctima blanca", "Víctima negra"),
  Blanco = round(c(porcentaje_total["Blanco"], porc_victima_blanca["Blanco"], porc_victima_negra["Blanco"]), 1),
  Negro  = round(c(porcentaje_total["Negro"],  porc_victima_blanca["Negro"],  porc_victima_negra["Negro"]), 1)
)

print(resumen)

"
        Analisis Blanco Negro
1          Total   11.9  10.2
2 Víctima blanca   12.6  17.5
3  Víctima negra    0.0   5.8
"

# (c) Utiliza los datos para explicar, en un lenguaje que pueda entender un juez,
# por qué se da la paradoja.

"Aunque en total parece que los acusados blancos reciben más condenas a muerte, al separar 
los casos según la raza de la víctima se observa lo contrario: los acusados negros son más 
condenados en ambos grupos. Esto ocurre porque la mayoría de los casos con víctimas blancas, 
donde la pena es más probable, involucran a acusados blancos, lo que distorsiona la comparación 
global."

# **************************************************
# PREGUNTA 2.78 - Análisis general de estudiantes universitarios
# **************************************************

# Los ejercicios 2.78 a 2.82 se basan en la tabla 2.11. Esta tabla de contingencia 
# proporciona datos sobre los estudiantes matriculados en el otoño de 1995, tanto 
# en universidades estadounidenses que ofrecen sólo primer ciclo, como en universidades 
# que ofrecen primer y segundo ciclo.

# (a) ¿Cuántos estudiantes están matriculados en primer o en segundo ciclo?
# (b) ¿Qué porcentaje de estudiantes de entre 18 y 24 años se matricularon?
# (c) Halla los porcentajes de los estudiantes con edades entre 18 y 24 años
#     que están matriculados en las opciones que aparecen en la tabla 2.11. Dibuja un
#     diagrama de barras para comparar estos porcentajes.
# (d) El grupo de estudiantes con edades entre 18 y 24 años es el grupo de
#     edad tradicional para estudiantes universitarios. Resume brevemente lo que has
#     aprendido a partir de los datos sobre el predominio de este tipo de estudiantes
#     en los distintos estudios universitarios.

# **************************************************
# PREGUNTA 2.79 - Preguntas específicas sobre estudiantes
# **************************************************

# (a) Una asociación de alumnos de primer ciclo pregunta: "¿Qué porcentaje
#     de estudiantes de primer ciclo a tiempo parcial, tiene entre 25 y 39 años?"
# (b) Un banco que proporciona préstamos a adultos para estudios pregunta:
#     "¿Qué porcentaje de estudiantes que tienen entre 25 y 39 años están matriculados
#     en primer ciclo?"

# **************************************************
# PREGUNTA 2.80 - Distribuciones de edad
# **************************************************

# (a) Halla la distribución marginal de la edad entre todos los estudiantes; pri
#     mero, en forma de recuentos y luego en forma de porcentajes. Dibuja un diagrama
#     de barras con estos porcentajes.
# (b) Halla la distribución condicional de la edad (en porcentajes) entre los
#     estudiantes matriculados a tiempo parcial en primer ciclo.
# (c) Describe brevemente las diferencias más importantes entre las dos distri
#     buciones de edad.
# (d) La suma de todos los valores de la columna "Primer ciclo. Tiempo par
#     cial" no es la misma que el total que aparece en la tabla. ¿Por qué?

# **************************************************
# PREGUNTA 2.81 - Estudiantes mayores (40+ años)
# **************************************************

# Llama a los estudiantes de 40 o más años "estudiantes mayores". Compara
# la presencia de estos estudiantes en los 4 tipos de matriculación, de forma numé
# rica y con un gráfico. Resume tus hallazgos.

# **************************************************
# PREGUNTA 2.82 - Análisis adicional de la tabla
# **************************************************

# Pensando un poco puedes obtener más información de la tabla 2.11 que
# las distribuciones marginales y las distribuciones condicionales. En general, la
# mayoría de estudiantes universitarios tienen entre 18 y 24 años.

# (a) ¿Qué porcentaje de universitarios se encuentran en este grupo de edad?
# (b) ¿Qué porcentaje de estudiantes de primer ciclo se hallan en ese grupo?
# (c) ¿Y de estudiantes a tiempo parcial?

# **************************************************
# PREGUNTA 2.83 - Muertes por armas de fuego en EE UU
# **************************************************

# Después de los accidentes de tráfico, las muertes por armas de fuego constituyen 
# la segunda causa de mortalidad no debida a enfermedades en EE UU.

# TABLA DE DATOS:
# Tipo de arma    Homicidios   Suicidios
# --------------------------------------
# Pistola         468          124
# Escopeta        28           22
# Rifle           15           24
# Desconocido     13           5
# Total           524          175

# (a) Compara con un diagrama de barras el tipo de armas utilizadas en suicidios 
#     y homicidios. ¿Qué diferencia existe entre las armas utilizadas para cazar 
#     (escopetas y rifles) y las pistolas?

# **************************************************
# PREGUNTA 2.84 - No-respuesta en una encuesta
# **************************************************

# Una escuela de empresariales realizó una encuesta sobre las empresas de su 
# entorno geográfico.

# TABLA DE DATOS:
# Tamaño empresa   Respuesta   No-respuesta   Total
# ------------------------------------------------
# Pequeñas         125         75             200
# Medianas         81          119            200
# Grandes          40          160            200

# (a) ¿Cuál fue el porcentaje global de no-respuesta?
# (b) Describe la relación que existe entre las no-respuestas y el tamaño de la
#     empresa. (Utiliza los porcentajes para que tu descripción sea precisa.)
# (c) Haz un diagrama de barras para comparar los porcentajes de no-respuesta
#     en los tres tipos de empresas.

# **************************************************
# PREGUNTA 2.85 - Ayuda a adictos a la cocaína
# **************************************************

# Estudio sobre la efectividad de tratamientos para adictos a la cocaína:

# TABLA DE DATOS:
# Tratamiento    Reincidencia Sí   Reincidencia No
# -----------------------------------------------
# Desipramina    10                14
# Litio          18                6
# Placebo        20                4

# (a) Compara la efectividad de cada uno de los tratamientos para prevenir la
#     reincidencia en el hábito. Utiliza porcentajes y dibuja un diagrama de barras.
# (b) ¿Crees que este estudio proporciona una evidencia sólida de que la desi
#     pramina causa realmente una reducción de la reincidencia?

# **************************************************
# PREGUNTA 2.86 - Edad y estado civil de las mujeres
# **************************************************

# Tabla de contingencia que describe la edad y el estado civil de las mujeres
# adultas estadounidenses en 1995 (valores en miles de mujeres).

# (a) Calcula la suma de los valores de la columna "Casada". ¿Por qué difiere
#     esta suma del valor que aparece en la columna de totales?
# (b) Halla la distribución marginal del estado civil de las mujeres adultas (uti
#     liza porcentajes). Dibuja un diagrama de barras para mostrar la distribución.
# (c) Compara las distribuciones condicionales del estado civil de las mujeres
#     con edades entre 18 y 24 años, y de las mujeres entre 40 y 64. Describe brevemente
#     las principales diferencias entre estos dos grupos de mujeres apoyándote en los
#     valores porcentuales.
# (d) Imagínate que quieres publicar una revista dirigida a mujeres solteras.
#     Halla la distribución condicional de las edades entre las mujeres solteras. Mues
#     tra esta distribución mediante un diagrama de barras. ¿A qué grupo o grupos de
#     edad se debería dirigir tu revista?