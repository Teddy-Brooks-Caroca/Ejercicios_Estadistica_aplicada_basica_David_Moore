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

