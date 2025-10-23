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


total_por_acusado <- rowSums(tabla_contingencia)
porcentaje_total <- tabla_contingencia[, "Si"] / total_por_acusado * 100

tabla_victima <- with(datos_acusados, tapply(Conteo, list(Acusado, Victima, Pena), sum))

porc_victima_blanca <- tabla_victima[, "Blanca", "Si"] / 
  (tabla_victima[, "Blanca", "Si"] + tabla_victima[, "Blanca", "No"]) * 100

porc_victima_negra <- tabla_victima[, "Negra", "Si"] / 
  (tabla_victima[, "Negra", "Si"] + tabla_victima[, "Negra", "No"]) * 100

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

# ---------------------------------------------------------------
# Edad          | Primer ciclo | Primer ciclo | Segundo ciclo | Segundo ciclo |
#                T. completo  | T. parcial   | T. completo   | T. parcial    |
# ---------------------------------------------------------------
# Menor de 18   |      41      |     125      |       75      |       45      |
# Entre 18 a 24 |     1378     |    1198      |      4607     |      588      |
# Entre 25 a 39 |     428      |    1427      |      1212     |     1321      |
# Mayor de 40   |     119      |     723      |      225      |      605      |
# Total         |     1966     |    3472      |      6119     |     2559      |

# (a) ¿Cuántos estudiantes están matriculados en primer o en segundo ciclo?

matriculados_primer_ciclo <- 1966 + 3472
matriculados_segundo_ciclo <- 6119 + 2559

totalidad_matriculados <- data.frame(
  Ciclo = c("Primer ciclo", "Segundo ciclo"),
  Matriculados = c(matriculados_primer_ciclo,matriculados_segundo_ciclo)
)

print(totalidad_matriculados)

"
          Ciclo Matriculados
1  Primer ciclo         5438
2 Segundo ciclo         8678
"
# (b) ¿Qué porcentaje de estudiantes de entre 18 y 24 años se matricularon?

total_18_24 <- 1378 + 1198 + 4607 + 588
total_matriculados <- matriculados_primer_ciclo + matriculados_segundo_ciclo

porcentaje <- (total_18_24 / total_matriculados) * 100

resultados_18_24 <- data.frame(
  Resumen = c("Totalidad de matriculados","Totalidad de 18 a 24 años","Porcentaje del grupo"),
  Valores = c(total_matriculados,total_18_24,paste0(round(porcentaje,2),"%"))
)

print(resultados_18_24)

"
                   Resumen  Valores
1 Totalidad de matriculados   14116
2 Totalidad de 18 a 24 años    7771
3      Porcentaje del grupo  55.05%
"
# (c) Halla los porcentajes de los estudiantes con edades entre 18 y 24 años
# que están matriculados en las opciones que aparecen en la tabla 2.11. Dibuja un
# diagrama de barras para comparar estos porcentajes.

primer_ciclo_completo <- (1378 / total_18_24) * 100
primer_ciclo_parcial <- (1198 / total_18_24) * 100
segundo_ciclo_completo <- (4607 / total_18_24) * 100
segundo_ciclo_parcial <- (588 / total_18_24) * 100


porcentajes_18_24 <- data.frame(
  Nivel_educativo = c("Primer ciclo completo", "Primer ciclo parcial",
                      "Segundo ciclo completo", "Segundo ciclo parcial"),
  Porcentaje = c(primer_ciclo_completo, primer_ciclo_parcial,
                 segundo_ciclo_completo, segundo_ciclo_parcial)
)

print(porcentajes_18_24)

barplot(porcentajes_18_24$Porcentaje,
        names.arg = porcentajes_18_24$Nivel_educativo,
        main = "Estudiantes 18-24 años por nivel educativo",
        ylab = "Porcentaje (%)",
        col = "lightblue",
        ylim = c(0, max_ylim),
        las = 2)
text(x = 1:4, y = porcentajes_18_24$Porcentaje + 1,
     labels = paste0(round(porcentajes_18_24$Porcentaje, 1), "%"))
grid()

"
         Nivel_educativo Porcentaje
1  Primer ciclo completo  17.732596
2   Primer ciclo parcial  15.416291
3 Segundo ciclo completo  59.284519
4  Segundo ciclo parcial   7.566594
5   Universidad completo  21.696049
6    Universidad parcial  26.920602
"

# (d) El grupo de estudiantes con edades entre 18 y 24 años es el grupo de edad tradicional 
# para estudiantes universitarios. Resume brevemente lo que has aprendido a partir de los 
# datos sobre el predominio de este tipo de estudiantes en los distintos estudios universitarios.

"El análisis revela que, aunque el grupo de 18-24 años representa el 55% del total de matriculados, 
su distribución por tipos de estudio es muy desigual:
  
- Predominan abrumadoramente en segundo ciclo completo (59.3%) - el formato universitario tradicional

- Tienen baja presencia en segundo ciclo parcial (7.6%) - estudios más compatibles con trabajo

- Participación moderada en primer ciclo (33.1% combinado) - estudios técnicos/cortos

- Los formatos parciales (trabajo-estudio) son minoritarios en este grupo etario

Conclusión: Los estudiantes tradicionales (18-24 años) se concentran en el modelo educativo clásico 
de dedicación exclusiva, mientras que los formatos flexibles son más utilizados por estudiantes mayores 
que combinan trabajo y estudio."

# **************************************************
# PREGUNTA 2.79 - Preguntas específicas sobre estudiantes
# **************************************************

# (a) Una asociación de alumnos de primer ciclo pregunta: "¿Qué porcentaje
# de estudiantes de primer ciclo a tiempo parcial, tiene entre 25 y 39 años?"

total_primer_ciclo_tiempo_parcial <- 3472 

primer_ciclo_parcial <- (1427 / total_primer_ciclo_tiempo_parcial) * 100

resultados_25_39 <- data.frame(
  Resumen = c("Totalidad tiempo parcial","Primer ciclo parcial de 25 a 39 años","Porcentaje del grupo"),
  Valores = c(3472,1427,paste0(round(primer_ciclo_parcial,2),"%"))
)

print(resultados_25_39)

"
                               Resumen Valores
1             Totalidad tiempo parcial    3472
2 Primer ciclo parcial de 25 a 39 años    1427
3                 Porcentaje del grupo   41.1%
"
# (b) Un banco que proporciona préstamos a adultos para estudios pregunta:
# ¿Qué porcentaje de estudiantes que tienen entre 25 y 39 años están matriculados
# en primer ciclo?"

total_primer_ciclo_25_39 <- 428 + 1427

total_primer_ciclo <- 1966 + 3472

porcentaje_primer_ciclo_25_39_restringuido <- (total_primer_ciclo_25_39 / total_25_39) * 100
porcentaje_primer_ciclo_25_39_ampliado <- (total_primer_ciclo_25_39 / total_primer_ciclo) * 100
porcentaje_primer_ciclo_25_39_super_ampliado <- (total_primer_ciclo_25_39 / total_matriculados) * 100

resultados_primer_ciclo_25_39 <- data.frame(
  Medida = c("Alumnado 25 a 39 años del primer ciclo",
             "Porcentaje en relación al grupo etario",
             "Porcentaje en relación al primer ciclo",
             "Porcentaje en relación a la población total"),
  Valores = c(total_primer_ciclo_25_39,
              paste0(round(porcentaje_primer_ciclo_25_39_restringuido,2),"%"),
              paste0(round(porcentaje_primer_ciclo_25_39_ampliado,2),"%"),
              paste0(round(porcentaje_primer_ciclo_25_39_super_ampliado,2),"%"))
)

print(resultados_primer_ciclo_25_39)

"
                                         Medida Valores
1        Alumnado 25 a 39 años del primer ciclo    1855
2        Porcentaje en relación al grupo etario  42.27%
3        Porcentaje en relación al primer ciclo  34.11%
4   Porcentaje en relación a la población total  13.14%
"
# **************************************************
# PREGUNTA 2.80 - Distribuciones de edad
# **************************************************

# (a) Halla la distribución marginal de la edad entre todos los estudiantes; primero, 
# en forma de recuentos y luego en forma de porcentajes. Dibuja un diagrama de barras 
# con estos porcentajes.

total_menor_18 <-  41 + 125 + 75 + 45
total_25_39 <-  428 +  1427 + 1212 +1321
total_mayor_40 <- 119 + 723 + 225 + 605 

total_alumnos <- total_menor_18 + total_18_24 + total_25_39 + total_mayor_40

porcentaje_menor_18 <- round((total_menor_18 / total_alumnos) * 100,2)
porcentaje_18_24 <- round((total_18_24 / total_alumnos) * 100,2)
porcentaje_25_34 <- round((total_25_39 / total_alumnos) * 100,2)
porcentaje_mayor_40 <- round((total_mayor_40 / total_alumnos) * 100,2)

recuentos_marginales <- data.frame(
  Grupo = c("Menor de 18 años","Entre 18 a 24 años","Entre 25 a 39 años","Mayor a 40 años"),
  Recuentos = c(total_menor_18,total_18_24,total_25_39,total_mayor_40),
  Porcentajes = c(porcentaje_menor_18,porcentaje_18_24,porcentaje_25_34,porcentaje_mayor_40)
)

print(recuentos_marginales)

barplot(recuentos_marginales$Porcentajes,
        names.arg = recuentos_marginales$Grupo,
        main = "Distribución marginal etaria de los estudiantes",
        ylab = "Porcentaje (%)",
        col = "lightblue",
        ylim = c(0, 60))
grid()

"
               Grupo Recuentos Porcentajes
1   Menor de 18 años       286        2.03
2 Entre 18 a 24 años      7771       55.05
3 Entre 25 a 39 años      4388       31.08
4    Mayor a 40 años      1672       11.84
"
# (b) Halla la distribución condicional de la edad (en porcentajes) entre los
# estudiantes matriculados a tiempo parcial en primer ciclo.

por_menor_18_primer_pacial <- 125 / 3472
por_19_24_primer_pacial <- 1198 / 3472
por_25_39_primer_pacial <- 1427 / 3472
por_mayor_40_primer_pacial <- 723 / 3472

porcentajes <- data.frame(
  Menor_a_18 = paste0(round(por_menor_18_primer_pacial * 100,2),"%"),
  De_19_a_24 = paste0(round(por_19_24_primer_pacial * 100,2),"%"),
  De_25_a_39 = paste0(round(por_25_39_primer_pacial * 100,2),"%"),
  Mayor_a_40 = paste0(round(por_mayor_40_primer_pacial * 100,2),"%")
)

print(porcentajes)

"
  Menor_a_18 De_19_a_24 De_25_a_39 Mayor_a_40
1       3.6%      34.5%      41.1%     20.82%
"

# (c) Describe brevemente las diferencias más importantes entre las dos distribuciones 
# de edad.

"La distribución marginal muestra que la mayoría de estudiantes son de 18–24 años (55%). 
Pero la distribución condicional para primer ciclo parcial revela un sesgo hacia estudiantes 
mayores: 41.1% son 25–39 y 20.8% >40. Es decir, las modalidades parciales atraen proporcionalmente 
más a adultos."

# (d) La suma de todos los valores de la columna "Primer ciclo. Tiempo parcial" 
# no es la misma que el total que aparece en la tabla. ¿Por qué?

"Se observa una discrepancia de 1 unidad entre la suma de las celdas de la columna Primer ciclo. 
Tiempo parcial (3473) y el total reportado en la tabla (3472). Esta diferencia mínima puede atribuirse 
a errores de redondeo o ajuste manual en los datos originales, sin impacto significativo en los porcentajes 
o las conclusiones del análisis."

# **************************************************
# PREGUNTA 2.81 - Estudiantes mayores (40+ años)
# **************************************************

# Llama a los estudiantes de 40 o más años "estudiantes mayores". Compara
# la presencia de estos estudiantes en los 4 tipos de matriculación, de forma numérica 
# y con un gráfico. Resume tus hallazgos.

est_40_tramo1 <- paste0(round(119 / total_mayor_40 * 100,2),"%")
est_40_tramo2 <- paste0(round(723 / total_mayor_40 * 100,2),"%")
est_40_tramo3 <- paste0(round(225 / total_mayor_40 * 100,2),"%")
est_40_tramo4 <- paste0(round(605 / total_mayor_40 * 100,2),"%")

por_general_tramo1 <- paste0(round(1966 / total_matriculados * 100,2),"%")
por_general_tramo2 <- paste0(round(3472 / total_matriculados * 100,2),"%")
por_general_tramo3 <- paste0(round(6119 / total_matriculados * 100,2),"%")
por_general_tramo4 <- paste0(round(2559 / total_matriculados * 100,2),"%")

resultados_estudiantes_40 <- data.frame(
  Tipo_matricula = c("Primer ciclo completo","Primer ciclo parcial",
                     "Segundo ciclo completo","Segundo ciclo parcial"),
  Recuento_mas_40 = c(119,723,225,605),
  Porcentajes_grupo = c(est_40_tramo1,est_40_tramo2,est_40_tramo3,est_40_tramo4),
  Porcentajes_general = c(por_general_tramo1,por_general_tramo2,por_general_tramo3,por_general_tramo4)
)

print(resultados_estudiantes_40)

"
          Tipo_matricula Recuento_mas_40 Porcentajes_grupo Porcentajes_general
1  Primer ciclo completo             119             7.12%              13.93%
2   Primer ciclo parcial             723            43.24%               24.6%
3 Segundo ciclo completo             225            13.46%              43.35%
4  Segundo ciclo parcial             605            36.18%              18.13%

=====================================================

Los estudiantes mayores (40 años o más) representan una proporción importante dentro de las modalidades 
a tiempo parcial, especialmente en el primer ciclo parcial (43,2%) y en el segundo ciclo parcial (36,2%).
En cambio, su presencia en las modalidades a tiempo completo es mucho menor (7,1% en primer ciclo completo 
y 13,5% en segundo ciclo completo).

Comparando con la distribución general del alumnado (13,9%, 24,6%, 43,3% y 18,1%), se observa que los estudiantes 
mayores están fuertemente sobrerrepresentados en las modalidades parciales y subrepresentados en las completas.
"

# **************************************************
# PREGUNTA 2.82 - Análisis adicional de la tabla
# **************************************************

# Pensando un poco puedes obtener más información de la tabla 2.11 que
# las distribuciones marginales y las distribuciones condicionales. En general, 
# la mayoría de estudiantes universitarios tienen entre 18 y 24 años.

# (a) ¿Qué porcentaje de universitarios se encuentran en este grupo de edad?

por_18 <- round(total_18_24 / total_alumnos * 100,2)
print(por_18)

"Es 55.05%"

# (b) ¿Qué porcentaje de estudiantes de primer ciclo se hallan en ese grupo?

total_primer_ciclo <-1966 + 3472

por_18_primer_ciclo <- ((1378 + 1198) / total_primer_ciclo) * 100

print(por_18_primer_ciclo)

" Es 47.37%"

# (c) ¿Y de estudiantes a tiempo parcial?

total_parcial <-2559 + 3472

por_18_parcial_ciclo <- ((588 + 1198) / total_parcial) * 100 

print(por_18_parcial_ciclo)

"Es 29.61%"

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

# Compara con un diagrama de barras el tipo de armas utilizadas en suicidios 
# y homicidios. ¿Qué diferencia existe entre las armas utilizadas para cazar 
# (escopetas y rifles) y las pistolas?

matrix_armas <- matrix(c(468,124,
                         28,22,
                         15,24,
                         13,5),
                       nrow = 4, ncol = 2,
                       byrow = TRUE)
rownames(matrix_armas) = c("Pistola","Escopeta","Rifle","Desconocido")
colnames(matrix_armas) = c("Homicidios","Suicidios")

print(matrix_armas)

barplot(matrix_armas,
        beside = TRUE,
        col = c("red", "blue", "green", "gray"),
        main = "Mortalidad por armas de fuego en EE.UU.",
        xlab = "Tipo de muerte",
        ylab = "Número de casos",
        ylim = c(0, 500),
        legend.text = rownames(matrix_armas),
        args.legend = list(x = "topright",
                           bty = "n",           
                           cex = 0.7,           
                           pt.cex = 1.8,        
                           y.intersp = 0.4))
grid()

pistolas_homicidios <- matrix_armas[1,1]  
pistolas_suicidios <- matrix_armas[1,2]   

armas_caza_homicidios <- sum(matrix_armas[2:3,1])  
armas_caza_suicidios <- sum(matrix_armas[2:3,2])   

total_homicidios <- sum(matrix_armas[,1])  
total_suicidios <- sum(matrix_armas[,2])   

porc_pistolas_homicidios <- (pistolas_homicidios / total_homicidios) * 100  
porc_armas_caza_homicidios <- (armas_caza_homicidios / total_homicidios) * 100  

"Las pistolas son abrumadoramente predominantes en muertes por armas de fuego, 
representando el 89% de los homicidios frente a solo 8% de las armas de caza. 
Mientras las escopetas y rifles tienen uso similar en suicidios y homicidios, 
las pistolas muestran una clara preferencia para violencia interpersonal, siendo 11 veces 
más utilizadas en homicidios que las armas de caza, lo que refleja su diseño para defensa 
personal versus uso deportivo."

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

matrix_empresas <- matrix(c(125,75,
                            81,119,
                            40,160),
                          nrow = 3,ncol = 2,
                          byrow = TRUE)
rownames(matrix_empresas) = c("Pequeñas","Medianas","Grandes")
colnames(matrix_empresas) = c("Respuesta","No-respuesta")

print(matrix_empresas)

total_no_respuesta <- sum(matrix_empresas[1:3,2])

total_empresas <- sum(matrix_empresas)

porc_no_respuesta <- (total_no_respuesta / total_empresas) * 100

resultados_no_respuesta <- data.frame(
  Anotaciones = c("Total de empresas","Total No respuesta","Porcentaje No respuesta"),
  Valores = c(total_empresas,total_no_respuesta,paste0(round(porc_no_respuesta,2),"%"))
)

print(resultados_no_respuesta)

"
              Anotaciones Valores
1       Total de empresas     600
2      Total No respuesta     354
3 Porcentaje No respuesta     59%
"

# (b) Describe la relación que existe entre las no-respuestas y el tamaño de la
# empresa. (Utiliza los porcentajes para que tu descripción sea precisa.)

total_pequenas <- sum(matrix_empresas[1,1:2])
total_medianas <- sum(matrix_empresas[2,1:2])
total_grande <- sum(matrix_empresas[3,1:2])

pequena_no_respuesta <- (matrix_empresas[1,2] / total_pequenas)
mediana_no_respuesta <- (matrix_empresas[2,2] / total_medianas)
grande_no_respuesta <- (matrix_empresas[3,2] / total_grande)

porcentajes_empresas <- data.frame(
  Tamannio = c("Pequeña","Mediana","Grande"),
  Porcentaje_no_respuesta = c(pequena_no_respuesta,mediana_no_respuesta,grande_no_respuesta)
)

print(porcentajes_empresas)

"
  Tamannio Porcentaje_no_respuesta
1  Pequeña                   0.375
2  Mediana                   0.595
3   Grande                   0.800

==================================================

Existe una relación positiva fuerte entre el tamaño de la empresa y la tasa de no-respuesta:

Empresas pequeñas: 37.5% no respondieron

Empresas medianas: 59.5% no respondieron

Empresas grandes: 80.0% no respondieron

La tasa de no-respuesta se más que duplica desde las empresas más pequeñas hasta las más grandes, 
sugiriendo que las empresas más grandes son menos propensas a participar en este tipo de encuestas.
"

# (c) Haz un diagrama de barras para comparar los porcentajes de no-respuesta
#     en los tres tipos de empresas.

barplot(porcentajes_empresas$Porcentaje_no_respuesta * 100,
        names.arg = porcentajes_empresas$Tamannio,
        main = "Tasa de no-respuesta por tamaño de empresa",
        ylab = "Porcentaje de no-respuesta (%)",
        xlab = "Tamaño de la empresa",
        col = c("lightblue", "lightgreen", "lightcoral"),
        ylim = c(0, 100))
text(x = 1:3, 
     y = porcentajes_empresas$Porcentaje_no_respuesta * 100 + 5,
     labels = paste0(round(porcentajes_empresas$Porcentaje_no_respuesta * 100, 1), "%"),
     col = "darkblue",
     font = 2)
grid()

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
# reincidencia en el hábito. Utiliza porcentajes y dibuja un diagrama de barras.

matrix_tratamiento <- matrix(c(10,14,
                               18,6,
                               20,4),
                             nrow = 3, ncol = 2,
                             byrow = TRUE)
rownames(matrix_tratamiento) = c("Desipramina","Litio","Placebo")
colnames(matrix_tratamiento) = c("Reincidencia SI","Reincidencia NO")

print(matrix_tratamiento)

total_desipramina <- sum(matrix_tratamiento[1,1:2])
total_litio <- sum(matrix_tratamiento[2,1:2])
total_placebo <- sum(matrix_tratamiento[3,1:2])

porc_desipramina_no <- (matrix_tratamiento[1,2] / total_desipramina) * 100
porc_litio_no <- (matrix_tratamiento[2,2] / total_litio) * 100
porc_placebo_no <- (matrix_tratamiento[3,2] / total_placebo) * 100

porcentajes_reincidencia_no <- data.frame(
  Tratamiento = c("Desipramina","Litio","Placebo"),
  Porcentajes = c(porc_desipramina_no,porc_litio_no,porc_placebo_no)
)

print(porcentajes_reincidencia_no)

barplot(c(porc_desipramina_no, porc_litio_no, porc_placebo_no),
        names.arg = c("Desipramina", "Litio", "Placebo"),
        main = "Efectividad de tratamientos\n(% sin reincidencia)",
        ylab = "Porcentaje de éxito (%)",
        col = c("green", "yellow", "red"),
        ylim = c(0, 70))
grid()

"
  Tratamiento Porcentajes
1 Desipramina    58.33333
2       Litio    25.00000
3     Placebo    16.66667
"
# (b) ¿Crees que este estudio proporciona una evidencia sólida de que la desipramina 
# causa realmente una reducción de la reincidencia?

"No necesariamente - aunque la desipramina muestra mejor resultado (58.3% vs 16.7% del placebo), 
el estudio por sí solo no prueba causalidad. Podría haber variables de confusión como:

Asignación no aleatoria a tratamientos

Características basales diferentes entre grupos

Efecto placebo diferenciado

Se necesitaría un ensayo aleatorizado controlado para establecer causalidad."

# **************************************************
# PREGUNTA 2.86 - Edad y estado civil de las mujeres
# **************************************************

# La siguiente tabla de contingencia describe la edad y el estado civil 
# de las mujeres adultas estadounidenses en 1995. Los valores de la tabla 
# se expresan en miles de mujeres.

# TABLA DE DATOS:
# ---------------------------------------------------------------
# Edad(años)   | Soltera   | Casada    | Viuda    | Divorciada | Total     |
# ---------------------------------------------------------------
# 18a24        | 9.289     | 3.046     | 19       | 260        | 12.613   |
# 25a39        | 6.948     | 21.437    | 206      | 3.408      | 32.000   |
# 40a64        | 2.307     | 26.679    | 2.219    | 5.508      | 36.713   |
# ≥65          | 768       | 7.767     | 8.636    | 1.091      | 18.264   |
# Total        | 19.312    | 58.931    | 11.080   | 10.266     | 99.588   |

# (a) Calcula la suma de los valores de la columna "Casada". ¿Por qué difiere
# esta suma del valor que aparece en la columna de totales?

matrix_mujeres <- matrix(c(
  9289, 3046, 19, 260, 12613,
  6948, 21437, 206, 3408, 32000,
  2307, 26679, 2219, 5508, 36713,
  768, 7767, 8636, 1091, 18264,
  19312, 58931, 11080, 10266, 99588), 
  nrow = 5, ncol = 5, byrow = TRUE)

rownames(matrix_mujeres) <- c("18 a 24", "25 a 39", "40 a 64", "≥ 65", "Total")
colnames(matrix_mujeres) <- c("Soltera", "Casada", "Viuda", "Divorciada", "Total")

print(matrix_mujeres)

total_casadas <- sum(matrix_mujeres[1:4,2])

print(total_casadas)

comparativa_casadas <- data.frame(
  Tipo = c("Valor otorgado","Valor calculado"),
  Resultado = c(matrix_mujeres[5,2],total_casadas)
)

print(comparativa_casadas)

"
             Tipo Resultado
1  Valor otorgado     58931
2 Valor calculado     58929

===============================================

Se puede deber a un error de redondeo
"

# (b) Halla la distribución marginal del estado civil de las mujeres adultas (utiliza porcentajes). 
# Dibuja un diagrama de barras para mostrar la distribución.

pct_estado_civil <- round(matrix_mujeres["Total",1:4]/ matrix_mujeres[5,5] * 100,2)

print(pct_estado_civil)

barplot(pct_estado_civil,
        main = "Distribución marginal del estado civil (1995)",
        ylab = "Porcentaje (%)",
        col = c("lightblue", "lightgreen", "lightpink", "lightgray"),
        ylim = c(0, 70))
grid()

"
Soltera  Casada   Viuda  Divorciada 
  19.39   59.17   11.13       10.31 
"

# (c) Compara las distribuciones condicionales del estado civil de las mujeres
# con edades entre 18 y 24 años, y de las mujeres entre 40 y 64. Describe brevemente
# las principales diferencias entre estos dos grupos de mujeres apoyándote en los
# valores porcentuales.

matrix_4x4 <- matrix_mujeres[1:4,1:4]

pct_por_edad <- round(prop.table(matrix_4x4, margin = 1) * 100, 2)

pct_por_edad[c("18 a 24", "40 a 64"), ]

"
        Soltera Casada Viuda Divorciada
18 a 24   73.64  24.15  0.15       2.06
40 a 64    6.28  72.67  6.04      15.00

================================================

Entre las mujeres de 18 a 24 años, la gran mayoría son solteras (≈74%), mientras que una 
proporción menor está casada (≈24%) y solo un porcentaje mínimo se encuentra viuda o divorciada 
(menos del 3%).
Esto refleja una etapa vital temprana, donde el matrimonio y la disolución de uniones aún son 
poco frecuentes.

En cambio, entre las mujeres de 40 a 64 años, la distribución cambia significativamente: el grupo 
predominante es el de casadas (≈73%), seguido por un aumento considerable de las divorciadas (≈15%) 
y viudas (≈6%), mientras que las solteras representan solo una pequeña fracción (≈6%).
Esto indica que, con el paso de la edad, la mayoría de las mujeres han estado o están casadas, y crecen 
los casos de separación o viudez.
"
# (d) Imagínate que quieres publicar una revista dirigida a mujeres solteras.
# Halla la distribución condicional de las edades entre las mujeres solteras. Muestra 
# esta distribución mediante un diagrama de barras. ¿A qué grupo o grupos de edad se 
# debería dirigir tu revista?

pct_solteras <- round(matrix_mujeres[1:4,1]/ matrix_mujeres[5,1] * 100,2)

print(pct_solteras)

barplot(pct_solteras,
        main = "Distribución de solteras por grupo etario",
        ylab = "Porcentaje (%)",
        col = c("lightblue", "lightgreen", "lightpink", "lightgray"),
        ylim = c(0, 50))
grid()
        
"
18 a 24  25 a 39  40 a 64  ≥ 65 
  48.10    35.98    11.95  3.98
  
===========================================

La mayor proporción de mujeres solteras se ubica en las edades 18–24 (48.1%) y 25–39 (36.0%). 
Una revista dirigida a solteras debería centrarse principalmente en el rango 18–39 años, con 
énfasis en 18–24.
"

# **************************************************
# PREGUNTA 2.87 - ¿Discriminación?
# **************************************************

# Un Instituto Superior de Empresariales imparte dos titulaciones: 
# una de Dirección de Empresas y otra de Derecho. Los aspirantes a cursar
# estudios en dicho centro deben superar una prueba de admisión.

# TABLAS DE CONTINGENCIA POR TITULACIÓN:
# ---------------------------------------------------------------
# Dirección de Empresas | Admitido | No admitido | Total       |
# ---------------------------------------------------------------
# Hombre                | 480      | 120         | 600         |
# Mujer                 | 180      | 20          | 200         |
# Total                 | 660      | 140         | 800         |
# ---------------------------------------------------------------
#
# Derecho               | Admitido | No admitido | Total       |
# ---------------------------------------------------------------
# Hombre                | 10       | 90          | 100         |
# Mujer                 | 100      | 200         | 300         |
# Total                 | 110      | 290         | 400         |
# ---------------------------------------------------------------

# (a) Construye una tabla de contingencia con el sexo y el resultado de la prueba
#     de admisión para las dos titulaciones conjuntamente, sumando los recuentos de
#     cada tabla.

matrix_dir_empresas <- matrix(c(480,120,
                                180,20),
                              nrow = 2, ncol = 2,
                              byrow = TRUE)
colnames(matrix_dir_empresas) = c("Admitido","No admitido")
rownames(matrix_dir_empresas) = c("Hombre","Mujer")

matrix_derecho <- matrix(c(10,90,
                           100,200),
                         nrow = 2, ncol = 2,
                         byrow = TRUE)
colnames(matrix_derecho) = c("Admitido","No admitido")
rownames(matrix_derecho) = c("Hombre","Mujer")

matrix_empresas_derecho = matrix_dir_empresas + matrix_derecho

print(matrix_empresas_derecho)

"
       Admitido No admitido
Hombre      490         210
Mujer       280         220
"
# (b) A partir de la tabla anterior, calcula el porcentaje de hombres y de mujeres
# admitidos. El porcentaje de hombres admitidos es superior al de mujeres.

pct_admitidos <- round(prop.table(matrix_empresas_derecho, margin = 1) * 100, 2)

pct_sexo_admitidos <- data.frame(Sexo = rownames(pct_admitidos),
                                 Porcentaje_Admitidos = paste0(pct_admitidos[,"Admitido"], "%"))

print(pct_sexo_admitidos)

"
    Sexo Porcentaje_Admitidos
1 Hombre                  70%
2  Mujer                  56%
"

# (c) Calcula de forma independiente el porcentaje de mujeres y de hombres
# admitidos según se trate de aspirantes a Dirección de Empresas o a Derecho.
# En ambas titulaciones la proporción de mujeres admitidas es superior a la de
# hombres.

pct_admitidos_empresas <- round(matrix_dir_empresas[,"Admitido"]/ rowSums(matrix_dir_empresas) * 100,2)
pct_admitidos_derecho <- round(matrix_derecho[,"Admitido"]/ rowSums(matrix_derecho) * 100,2)

df_pct_admitidos <- data.frame(
  Sexo = rownames(matrix_dir_empresas),
  Empresas = paste0(pct_admitidos_empresas, "%"),
  Derecho = paste0(pct_admitidos_derecho, "%")
)

print(df_pct_admitidos)

"    
    Sexo Empresas Derecho
1 Hombre      80%     10%
2  Mujer      90%  33.33%
"

# (d) Se cumple la paradoja de Simpson: en cada una de las dos titulaciones el
# porcentaje de mujeres admitidas es superior al de hombres. Sin embargo, consi-
# derando a todos los alumnos conjuntamente, el porcentaje de hombres admitidos
# es superior al de mujeres. Explica esta paradoja en un lenguaje sencillo, para que
# lo pueda entender una persona que no tenga una especial formación estadística.

"
Aunque en cada carrera individualmente la proporción de mujeres admitidas es más alta 
que la de hombres, cuando miramos a todos los estudiantes juntos, resulta que más hombres 
son admitidos en porcentaje. Esto puede parecer contradictorio, pero se debe a cómo se distribuyen 
los estudiantes entre las carreras:

Por ejemplo, si la mayoría de las mujeres postula a una carrera muy competitiva (con pocas admisiones)
y la mayoría de los hombres postula a una carrera menos competitiva (con más admisiones), entonces, al 
combinar los datos de ambas carreras, los hombres terminan con un porcentaje global de admisión mayor.

Es decir, la composición del grupo importa: los porcentajes dentro de subgrupos (cada carrera) no siempre
coinciden con el porcentaje del grupo total.

En palabras simples: mirar solo los totales puede engañar, porque la proporción de admisión depende tanto 
de quién postula a qué carrera como del resultado en cada carrera."

# **************************************************
# PREGUNTA 2.88 - Obesidad y salud
# **************************************************

# Estudios recientes han puesto de manifiesto que los primeros trabajos sobre 
# obesidad subestimaron los riesgos para la salud asociados con el sobrepeso. 
# El error se debía a no tener en cuenta determinadas variables latentes.
# Con esta variable latente, ilustra de forma simplificada la paradoja de Simpson.
# Es decir, construye dos tablas de contingencia, una para fumadores y otra para 
# no fumadores, con las variables sobrepeso (Sí o No) y muerte temprana (Sí o No). 
# De manera que:
# • Tanto los fumadores como los no fumadores con sobrepeso tiendan a morir 
#   antes que los que no tienen sobrepeso.
# • Pero que cuando se combinen los fumadores y los no fumadores en una
#   sola tabla de contingencia con las variables sobrepeso y muerte temprana,
#   las personas sin sobrepeso tiendan a morir más tempranamente.

tabla_fumadores <- matrix(c(30, 70,
                            20, 80),
                          nrow = 2, byrow = TRUE)
rownames(tabla_fumadores) <- c("Sobrepeso", "No sobrepeso")
colnames(tabla_fumadores) <- c("Muerte Sí", "Muerte No")

tabla_no_fumadores <- matrix(c(10, 90,
                               5, 95),
                             nrow = 2, byrow = TRUE)
rownames(tabla_no_fumadores) <- c("Sobrepeso", "No sobrepeso")
colnames(tabla_no_fumadores) <- c("Muerte Sí", "Muerte No")

tabla_fumadores
tabla_no_fumadores

pct_fumadores <- round(prop.table(tabla_fumadores, 1) * 100, 1)
pct_no_fumadores <- round(prop.table(tabla_no_fumadores, 1) * 100, 1)

pct_fumadores
pct_no_fumadores

tabla_combinada <- tabla_fumadores + tabla_no_fumadores

pct_combinada <- round(prop.table(tabla_combinada, 1) * 100, 1)

tabla_combinada
pct_combinada

par(mfrow=c(1,3)) 

barplot(pct_fumadores, beside = TRUE,
        main = "Fumadores",
        ylab = "% Muerte temprana",
        col = c("red","green"))

barplot(pct_no_fumadores, beside = TRUE,
        main = "No fumadores",
        ylab = "% Muerte temprana",
        col = c("red","green"))

barplot(pct_combinada, beside = TRUE,
        main = "Combinados",
        ylab = "% Muerte temprana",
        col = c("red","green"))

"En los subgrupos (fumadores y no fumadores), las personas con sobrepeso tienen mayor 
riesgo de muerte temprana que las personas sin sobrepeso. Sin embargo, cuando se combinan 
ambos grupos, la tabla global puede mostrar lo contrario: las personas sin sobrepeso parecen 
tener mayor riesgo que las personas con sobrepeso. Esto ejemplifica la paradoja de Simpson, 
donde una tendencia presente en los subgrupos se invierte al mirar los datos agregados."

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::