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