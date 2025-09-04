# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO I "Análisis de distribuciones"

# SECCIÓN 1.4 "Distribuciones normales"

# **************************************************
# PREGUNTA 1.50 - Curvas de densidad
# **************************************************

# (a) Dibuja una curva de densidad que sea asimétrica pero que tenga una forma
# distinta a la curva de la figura 1.15(a).

par(mfrow=c(1,2)) #vemos los dos gráficos

x1 <- c(0,1,2,3,4,5,6,7,8,9,10)
y1 <- c(0,1,2,3,2,1.5,1,0.8,0.5,0.3,0.1)
plot(x1, y1, type="l", lwd=2, main="(a) Asimétrica",
     xlab="x", ylab="densidad")

# (b) Dibuja una curva de densidad que sea muy asimétrica hacia la izquierda.

x2 <- c(0,1,2,3,4,5,6,7,8,9,10)
y2 <- c(3,2.5,2,1.5,1,0.7,0.5,0.3,0.2,0.1,0)
plot(x2, y2, type="l", lwd=2, main="(b) Muy asimétrica izquierda",
     xlab="x", ylab="densidad")

dev.off()

# **************************************************
# PREGUNTA 1.51 - Distribución uniforme
# **************************************************

# La figura 1.17 muestra la curva de densidad de una distribución uniforme.
# La curva toma el valor constante 1 para todos los valores situados en el intervalo
# definido entre 0 y 1, y el valor 0 para los restantes valores. Esto significa que los
# datos descritos por esta distribución toman valores con una dispersión uniforme
# entre 0 y 1.

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

install.packages("magick")
library(magick)

figura_1_17 <- image_read("figura_1_17.png")
print(figura_1_17)

# (a) ¿Por qué el área total por debajo de la curva es igual a 1?

"Por que representa la probabilidad total de los datos"

# (b) ¿Qué porcentaje de las observaciones es mayor que 0,8?

"El 20%"

# (c) ¿Qué porcentaje de las observaciones es menor que 0,6?

"El 60%"

# (d) ¿Qué porcentaje de las observaciones queda entre 0,25 y 0,75?

"El 50%"

# (e) ¿Cuál es la media µ de esta distribución?

"la media es 0.5"

# **************************************************
# PREGUNTA 1.52 - Identificación de media y mediana en curvas de densidad
# **************************************************

# La figura 1.18 muestra tres curvas de densidad. En cada una de ellas se han
# señalado tres puntos.

figura_1_18 <- image_read("figura_1_18.png")
print(figura_1_18)

# (a) ¿Cuáles corresponden a la media y cuáles a la mediana?

"
(a) 

Mediana = B (punto que divide el área en dos).

Media = C (está más desplazada hacia la cola derecha).
(El modo sería A, el pico.)

(b) 

Media = Mediana = A (el centro / pico; todos coinciden).

(c) 

Media = A (la media se desplaza hacia la cola izquierda).

Mediana = B (queda entre media y modo).
(El modo sería C, en el pico a la derecha.)"

# (b) Justifica tu respuesta para cada una de las tres curvas.

"
(a) la curva tiene la cola hacia la derecha (peak a la izquierda.
(b) la curva es simétrica (campana).
(c) la curva tiene la cola hacia la izquierda (peak a la derecha)."

# **************************************************
# PREGUNTA 1.53 - Alturas de hombres (Distribución normal)
# **************************************************

# La distribución de alturas de los hombres adultos es aproximadamente normal, 
# con una media de 1,75 m y una desviación típica de 0,06 m.

# (a) Dibuja una curva normal en la que sitúes correctamente su media y su
# desviación típica. (Sugerencia: primero dibuja la curva, localiza sobre ella los puntos
# de inflexión y proyecta estos puntos sobre el eje de las abscisas.)

media <- 1.75
desviacion <- 0.06

x <- seq(media - 4*desviacion, media + 4*desviacion, length.out = 1000)

y <- dnorm(x, mean = media, sd = desviacion)

datos <- data.frame(x = x, y = y)

curve(dnorm(x, mean = media, sd = desviacion), 
      from = media - 4*desviacion, 
      to = media + 4*desviacion,
      main = "Distribución Normal de Alturas",
      xlab = "Altura (metros)",
      ylab = "Densidad",
      col = "blue",
      lwd = 2)
abline(v = media, col = "red", lwd = 2, lty = 2)
abline(v = c(media - desviacion, media + desviacion), 
       col = "green", lwd = 1.5, lty = 3)
legend("topright", 
       legend = c("Curva normal", "Media (μ = 1.75)", "Puntos de inflexión (μ ± σ)"),
       col = c("blue", "red", "green"), 
       lty = c(1, 2, 3), 
       lwd = c(2, 2, 1.5),
       cex = 0.5,
       inset = c(-0.3, 0),  
       bty = "n")

# **************************************************
# PREGUNTA 1.54 - Más sobre alturas de hombres (Regla 68-95-99,7)
# **************************************************

# La distribución de las alturas de los hombres adultos es aproximadamente normal 
# con una media de 1,75 m y desviación típica de 0,06 m.

# (a) ¿Qué porcentaje de hombres son más altos que 1,87 m?

pnorm(1.87, mean = 1.75, sd = 0.06, lower.tail = FALSE) *100

"El 2.5% de los hombres son mas altos que 1.87 mts"

# (b) ¿Entre qué alturas se encuentra el 95% central de la población de hombres?

qnorm(c(0.025, 0.975), 1.75, 0.06)

"Entre 1.63 mts y 1.87 mts"

# (c) ¿Qué porcentaje de hombres tiene una altura inferior a 1,69 m?

pnorm(1.69, mean = 1.75, sd = 0.06) *100

"El 16% tiene una altura inferior al 1.69 mts."

# **************************************************
# PREGUNTA 1.55 - Coeficientes de inteligencia (Regla 68-95-99,7)
# **************************************************

# La distribución de los coeficientes de inteligencia de hombres entre 20 y 34 años 
# tiene aproximadamente una distribución normal de media µ = 110 y desviación 
# típica σ = 25.

# (a) De los hombres entre 20 y 34 años, ¿qué porcentaje tiene un coeficiente
#     intelectual superior a 110?

pnorm(110, 110, 25, lower.tail = FALSE)

"El 50% tiene un coeficiente superior a 110 pts"

# (b) ¿Qué porcentaje tiene un coeficiente intelectual superior a 160?

pnorm(160,mean = 110, sd = 25, lower.tail = FALSE)*100

"El 2.5% tiene un coeficiente superior a 160 pts"

# (c) ¿En qué intervalo se encuentra el 95% central de la población?

qnorm(c(0.025, 0.975), 110, 25)

"Entre 60 pts y 160 pts"

# **************************************************
# PREGUNTA 1.56 - SAT versus ACT (Puntuaciones estandarizadas)
# **************************************************

# Meritxell obtuvo 680 puntos en el examen de Matemáticas de la prueba SAT 
# (Scholastic Assessment Test) de acceso a la universidad en EE UU.

# Clara obtuvo 27 puntos en el examen de Matemáticas de la prueba ACT 
# (American College Testing) de acceso a la universidad en EE UU.

# Distribuciones:
# - SAT: Media = 500, Desviación típica = 100
# - ACT: Media = 18, Desviación típica = 6

# (a) Halla las notas estandarizadas (puntuaciones z) de ambas estudiantes.

puntuacion_Meritxell <- (680 - 500) / 100

puntuacion_Clara <- (27 - 18) / 6

resultados_pruebas <- data.frame(
  Estudiantes = c("Meritxell","Clara"),
  Puntuaciones = c(puntuacion_Meritxell,puntuacion_Clara)
)

print(resultados_pruebas)

"
  Estudiantes Puntuaciones
1   Meritxell          1.8
2       Clara          1.5
"

# (b) Suponiendo que los dos exámenes sean similares, ¿qué estudiante obtuvo 
# mayor puntuación relativa en comparación con su distribución?

"Meritxell obtuvo 1.8 pts versus Clara con 1.5 pts"

# **************************************************
# PREGUNTA 1.57 - Distribución normal estandarizada (Tabla A)
# **************************************************

# Utiliza la tabla A para hallar proporciones de observaciones a partir de la
# distribución normal estandarizada que satisfagan cada una de las afirmaciones
# siguientes. En cada caso, dibuja la curva normal estandarizada y sombrea el área
# por debajo de la curva que corresponda.

# (a) z < 2,85

pnorm(2.85)
"0.997814"

# (b) z > 2,85

1 - pnorm(2.85)
"0.002185961"

# (c) z > −1,66

1 - pnorm(-1.66)
"0.9515428"

# (d) −1,66 < z < 2,85

pnorm(2.85) - pnorm(-1.66)
"0.9493568"

# **************************************************
# PREGUNTA 1.58 - Fuerza de tiro de una locomotora
# **************************************************

# La adherencia de un determinado modelo de locomotora Diesel de 4.400 caballos 
# de potencia varía según una distribución normal de media µ = 0,37 y desviación 
# típica σ = 0,04.

# (a) ¿Qué proporción de adhesiones son mayores de 0,40?

z = (0.40 - 0.37)/0.04

1- pnorm(z)

"La proporción de adhseiones mayores a 0.40 es: 0.2266274 (22.66%)"

# (b) ¿Qué proporción de adhesiones se hallan entre 0,40 y 0,50?

x = (0.50 - 0.37) / 0.04

pnorm(x) - pnorm(z)

"Un 23% se haya entre 0.40 y un 0.50"

# Las mejoras en el control informático de las locomotoras cambian la 
# distribución normal de manera que µ = 0,41 y σ = 0,02.

# (c) Teniendo en cuenta estas mejoras, halla las proporciones en (a) y (b).

z_corregido <- (0.40 - 0.41) / 0.02
x_corregido <- (0.50 - 0.41) / 0.02

1 - pnorm(z_corregido)
"La proporcion de adhseiones corregidas es ahora de: 0.6914625 (69%)"

pnorm(x_corregido) - pnorm(z_corregido)
"Ahora 69% se encuentra entre 0.40 y 0.50"

"Esto se debe porque aumento la media y disminuyó la desviación estandar"

# **************************************************
# PREGUNTA 1.59 - Valores z usando Tabla A
# **************************************************

# Utiliza la tabla A para hallar el valor de z de una distribución normal 
# estandarizada que cumpla cada una de las siguientes condiciones.


# (a) El valor z tal que el 25% de las observaciones sean menores.

tabla_a1 <- image_read("Tabla_A1_Probabilidades_normales_estandarizadas.png")
tabla_a2 <- image_read("Tabla_A2_Probabilidades_normales_estandarizadas.png")

print(tabla_a1)

qnorm(0.25) #comprobamos con la función cuantil

"-0.67"

# (b) El valor z tal que el 40% de las observaciones sean mayores.

print(tabla_a2)

qnorm(0.60)

"0.25"

# **************************************************
# PREGUNTA 1.60 - Coeficientes de inteligencia
# **************************************************

# Los coeficientes de una prueba de inteligencia (Wechsler Adult Intelligence Scale) 
# para un grupo de adultos entre 20 y 34 años tienen una distribución aproximadamente 
# normal de media µ = 110 y desviación típica σ = 25

# (a) ¿Qué porcentaje de personas entre 20 y 34 años tiene un coeficiente de
# inteligencia mayor que 100?

z =(100 - 110) / 25

1 - pnorm(z)

"Un 66% tiene un coeficiente mayor que 100 puntos"

# (b) ¿Qué valor del coeficiente de inteligencia es necesario para estar entre el
# 25% que obtiene peores resultados?

z = qnorm(0.25)

x = 110 + z * 25

print(x)

"Hay que tener 93 puntos para estar en los peores resultados"

# (c) ¿Qué valor del coeficiente de inteligencia es necesario para estar entre el
# 5% que obtienen mejores resultados?

z = qnorm(0.95)

x = 110 + z * 25

print(x)

"Hay que tener 151 puntos para estar en el 5% de los mejores resultados"

# **************************************************
# PREGUNTA 1.61 - Curvas normales con media 0
# **************************************************

# La figura 1.26 muestra dos curvas normales, ambas con media 0.

# (a) ¿Podrías decir cuánto valen aproximadamente las desviaciones típicas de estas curvas?

figura_1_26 <- image_read("figura_1_26.png")

print(figura_1_26)

"
Curva más alta: σ = 0,2 
Curva más baja: σ = 0,5.
"

# **************************************************
# PREGUNTA 1.62 - Perímetros craneales de soldados
# **************************************************

# Los perímetros craneales de los soldados tienen una distribución normal con
# media 57,9 cm y desviación típica 2,8 cm.Utiliza la regla del 68-95-99,7 para 
# responder a las siguientes preguntas:

# (a) ¿Qué porcentaje de soldados tiene un perímetro craneal mayor de 60,7 cm?

una_desviacion_positiva <- 57.9 + 2.8

print(una_desviacion)

"
1 - 0.68 = 0.32 coresponde a los datos fuera del rango

32 / 2 = 16     corresponde a las colas
"

"El 16% tiene un perimetro mayor o igual 60.7 cm "


# (b) ¿Qué porcentaje de soldados tiene un perímetro craneal situado entre 55,1
# y 60,7 cm?

una_desviacion_negativa <- 57.9 - 2.8

print(una_desviacion_negativa)

"El 68% tiene un perimetro situado ente 55.1 hasta 60.7 cm"

# **************************************************
# PREGUNTA 1.63 - Duración del embarazo
# **************************************************

# La duración del embarazo humano desde la fecundación del óvulo hasta el parto 
# varía de acuerdo con una distribución aproximadamente normal, con una media 
# de 266 días y una desviación típica de 16 días. Utilizala regla del 68-95-99,7 
# para responder a las siguientes preguntas.

# (a) ¿Entre qué valores se encuentra la duración del embarazo del 95% central
# de la población?

dos_desviaciones_positiva <- 266 + (16*2)
print(dos_desviaciones_positiva)

dos_desviaciones_negativa <- 266 - (16*2)
print(dos_desviaciones_negativa)

"El 95% se encuentra entre 234 hasta 298 días"

# (b) ¿Qué duración tiene el 2,5% de los embarazos más cortos?

"Tiene 234 días"

# **************************************************
# PREGUNTA 1.64 - Tres grandes récords de béisbol
# **************************************************

# Medias de bateo:
# - Ty Cobb (1911): 0.420
# - Ted Williams (1941): 0.406  
# - George Brett (1980): 0.390

# Estadísticas por década:
# Década        Media   Desviación típica
# Años diez     0.266   0.0371
# Años cuarenta 0.267   0.0326
# Años setenta  0.261   0.0317

# (a) Calcula los valores estandarizados de las medias de bateos de Cobb, Williams y Brett


z_cobb <- (0.420 - 0.266)/ 0.0371
z_williams <- (0.406 - 0.267) / 0.0326
z_brett <- (0.390-0.261) / 0.0317

resultados_bateos <- data.frame(
  Jugadores = c("Ty Cobb","Ted Williams", "George Brett"),
  Resultados = c(z_cobb,z_williams,z_brett)
)

print(resultados_bateos)

"
     Jugadores Resultados
1      Ty Cobb   4.150943
2 Ted Williams   4.263804
3 George Brett   4.069401
"

# **************************************************
# PREGUNTA 1.65 - Probabilidades normales
# **************************************************

# Utiliza la tabla A para hallar la proporción de observaciones de una distribución 
# normal estandarizada que se sitúan en cada una de las siguientes regiones.

# (a) z ≤ −2.25

pnorm(-2.25)

"0.01222447"

# (b) z ≥ 2.25

pnorm(2.25)

"0.9877755"

# (c) z > 1.77

1 - pnorm(1.77)

"0.03836357"

# (d) −2.25 < z < 1.77

pnorm(1.77) - pnorm(-2.55)

"0.9562503"

# **************************************************
# PREGUNTA 1.66 - Valores z específicos
# **************************************************

# (a) Halla el número z tal que la proporción de observaciones menores que z
# en una distribución normal estandarizada sea 0.8.

qnorm(0.8)

"0.8416212"

# (b) Halla el número z tal que un 35% de las observaciones de una distribución 
# normal estandarizada sea mayor que z.

z <- 1 - 0.35

qnorm(z)

"0.3853205"

# **************************************************
# PREGUNTA 1.67 - Mercado bursátil
# **************************************************

# La tasa de rendimiento anual de las acciones tiene una distribución aproximadamente normal.
# Media = 12%, Desviación típica = 16.5%

# (a) ¿En qué intervalo hallamos el 95% central de las tasas de rendimiento anuales?

dos_desviaciones_positiva <- 12 + (16.5*2)
dos_desviaciones_negativa <- 12 - (16.5*2)

print(dos_desviaciones_positiva)
print(dos_desviaciones_negativa)

"El 95% lo hallamos entres -21% y 45%"

# (b) ¿En qué porcentaje de años la Bolsa está en crisis (rendimiento < 0)?

z <- (0 - 12) / 16.5

pnorm(z)

"En un 23% de los años entra en crisis la bolsa"

# (c) ¿En qué porcentaje de años el índice gana al menos el 25%?

z <- (25 - 12) / 16.5

1 - pnorm(z)

"En un 22% de los años gana al menos un 25%"

# **************************************************
# PREGUNTA 1.68 - Duración del embarazo (continuación)
# **************************************************

# (a) ¿Qué porcentaje de embarazos dura menos de 240 días?

z <- (240 - 266) / 16

pnorm(z)

"Un 5.2% dura menos de 240 días"

# (b) ¿Qué porcentaje de embarazos tiene una duración entre 240 y 270 días?

z <- (240 - 266) / 16
y <- (270 - 266) / 16

pnorm(y) - pnorm(z)

"Un 55% de los embarazos dura entre 240 y 270 días"

# (c) ¿Qué duración tiene el 20% de los embarazos más largos?

 z <- qnorm(0.80)

 x <- 266 + z * 16
 
 print(x)
 
 "El 20% de los embarazos más largos dura al menos 279 días"

# **************************************************
# PREGUNTA 1.69 - Coeficientes de inteligencia a lo largo del tiempo
# **************************************************

# Prueba Stanford-Binet (1932): µ = 100, σ = 15
# Niños actuales: µ = 120, σ = 15

# (a) ¿Qué porcentaje de niños tienen coeficientes muy superiores (>130) en 1932?
 
 z <- (130 - 100) / 15
 
 1- pnorm(z)
 
 "Un 2.27% de los niños tiene un coeficiente >130"
 
# (b) ¿Qué porcentaje de niños actuales tendrían coeficientes muy superiores (>130) 
# si pasaran la prueba de 1932?
 
 z <- (130 - 120)/ 15
 
 1 - pnorm(z)
 
 "Un 25% de los niños serían calificados como 'muy inteligentes'"

# **************************************************
# PREGUNTA 1.70 - Cuartiles de distribuciones normales
# **************************************************

# La mediana de cualquier distribución normal es igual a su media. Podemos
# utilizar los cálculos normales para hallar los cuartiles de una distribución normal.
 
# (a) Halla el primer y tercer cuartil de una distribución normal estandarizada
 
  primer_cuartil <- qnorm(0.25)
  
  tercer_cuartil <- qnorm(0.75)
  
  resultados_cuartiles <- data.frame(
    Cuartil = c("Primer cuatril", "Tercer cuartil"),
    Resultados = c(primer_cuartil,tercer_cuartil)
  )
  
  print(resultados_cuartiles)
  
  "
           Cuartil Resultados
1 Primer cuatril -0.6744898
2 Tercer cuartil  0.6744898
  "
  
  
# (b) Halla los cuartiles de la duración del embarazo humano (µ = 266, σ = 16)
  
  x <- 266 + (primer_cuartil * 16)
  y <- 266 + (tercer_cuartil * 16)

  resultados_dias <- data.frame(
    Cuartil = c("Primer cuartil", "Tercer Cuartil"),
    Duracion_dias = c(x,y)
  )
  
  print(resultados_dias)
  
  "        Cuartil Duracion_dias
1 Primer cuartil      255.2082
2 Tercer Cuartil      276.7918
  "
# **************************************************
# PREGUNTA 1.71 - Deciles de distribuciones normales
# **************************************************

# Los deciles de cualquier distribución son los puntos que señalan el 10% de
# las observaciones menores y el 10% de las mayores. Los deciles de una curva
# de densidad son, por tanto, los puntos a la izquierda de los cuales hay un área de
# 0,1 y de 0,9 por debajo de la curva.
  
# (a) ¿Cuáles son los deciles de una distribución normal estandarizada?
  
  decil_1 <- qnorm(0.1)
  
  decil_9 <- qnorm(0.9)
  
  resultado_deciles <- data.frame(
    Deciles = c("Primer_decil", "Noveno_decil"),
    Resultados = c(decil_1,decil_9)
  )
  
  print(resultado_deciles)
  
  "       Deciles Resultados
1 Primer_decil  -1.281552
2 Noveno_decil   1.281552
  "
  
# (b) La altura de las mujeres tiene distribución normal con media 1.64 m y 
# desviación típica 0.06 m. ¿Cuáles son los deciles de esta distribución?
  
  x <- 1.64 + (decil_1 * 0.06)
  
  y <- 1.64 + (decil_9 * 0.06)
  
  resultado_aturas <- data.frame(
    Deciles = c("Primer decil", "Noveno decil"),
    Alturas = c(x,y)
  )
  
  print(resultado_aturas)
  
  "
        Deciles  Alturas
1 Primer decil 1.563107
2 Noveno decil 1.716893
  "

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::
