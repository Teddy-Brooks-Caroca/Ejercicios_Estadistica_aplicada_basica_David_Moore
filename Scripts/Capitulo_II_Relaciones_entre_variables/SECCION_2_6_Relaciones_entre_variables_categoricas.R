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
