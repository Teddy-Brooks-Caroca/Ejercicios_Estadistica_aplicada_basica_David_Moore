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

