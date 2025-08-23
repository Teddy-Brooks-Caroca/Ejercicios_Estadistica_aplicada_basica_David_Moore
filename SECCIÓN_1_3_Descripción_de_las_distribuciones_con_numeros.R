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
