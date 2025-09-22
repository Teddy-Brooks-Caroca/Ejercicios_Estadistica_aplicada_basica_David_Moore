# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# SECCIÓN 2.2 "La correlación"

# **************************************************
# PREGUNTA 2.17 - Clasificación de fósiles (Archaeopteryx)
# **************************************************

# El Archaeopteryx es una especie extinguida que tenía plumas como un pájaro,
# pero que también tenía dientes y cola como un reptil. Sólo se conocen seis fósiles 
# de estas características. Como estos especímenes difieren mucho en su tamaño, 
# algunos científicos creen que pertenecen a especies distintas.

# Datos de las longitudes en centímetros del fémur y del húmero de cinco fósiles:
# Fémur:  38  56  59  64  74
# Húmero: 41  63  70  72  84

# (a) Dibuja un diagrama de dispersión. ¿Crees que los 5 fósiles pertenecen a la
# misma especie?

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

fosil_Archaeopteryx <- data.frame(
  Femur = c(38,56,59,64,74),
  Humero = c(41,63,70,72,84)
)

print(fosil_Archaeopteryx)

plot(fosil_Archaeopteryx$Femur,
     fosil_Archaeopteryx$Humero,
     main = "Clasificación de fósiles (Archaeopteryx)",
     xlab = "Fémur (cms)",
     ylab = "Húmero (cms)",
     col = "blue",
     pch = 16)
grid()

# (b) Halla la correlación r, paso a paso. Es decir, halla la media y la desviación
# típica de las longitudes de los fémures y de los húmeros. (Utiliza tu calculadora
# para calcular las medias y las desviaciones típicas.) Halla los valores estandariza
# dos de cada valor. Calcula r a partir de su fórmula.

media_femur <- mean(fosil_Archaeopteryx$Femur)
media_humero <- mean(fosil_Archaeopteryx$Humero)

desv_femur <- sd(fosil_Archaeopteryx$Femur)
desv_humero <- sd(fosil_Archaeopteryx$Humero)

z_femur <- (fosil_Archaeopteryx$Femur - media_femur)/desv_femur
z_humero <- (fosil_Archaeopteryx$Humero - media_humero)/desv_humero

n_muestras <- 5

r <- (1 / (n_muestras - 1)) * sum(z_femur * z_humero)

datos_fosiles <- data.frame(
  Tipo = c("Femur","Húmero"),
  Media = c(media_femur,media_humero),
  Desviación = c(desv_femur,desv_humero)
)

fosil_Archaeopteryx$z_femur <- round((fosil_Archaeopteryx$Femur - media_femur) / desv_femur, 3)
fosil_Archaeopteryx$z_humero <- round((fosil_Archaeopteryx$Humero - media_humero) / desv_humero, 3)
fosil_Archaeopteryx$producto_z <- round(fosil_Archaeopteryx$z_femur * fosil_Archaeopteryx$z_humero, 3)

print(datos_fosiles)

print(fosil_Archaeopteryx)

print(r)

"
    Tipo Media Desviación
1  Femur  58.2   13.19848
2 Húmero  66.0   15.89025

=======================================

  Femur Humero z_femur z_humero producto_z
1    38     41  -1.530   -1.573      2.407
2    56     63  -0.167   -0.189      0.032
3    59     70   0.061    0.252      0.015
4    64     72   0.439    0.378      0.166
5    74     84   1.197    1.133      1.356

=======================================

r = 0.9941486

"

# (c) Ahora entra los datos en tu calculadora y utiliza la función que permite
# calcular directamente r. Comprueba que obtienes el mismo valor que en (b).

r_directo <- cor(fosil_Archaeopteryx$Femur, fosil_Archaeopteryx$Humero)

comparativa_r <- data.frame(
  Tipo_metodo = c("Manual","Computadora"),
  Resultado = c(r,r_directo)
)

print(comparativa_r)

"
  Tipo_metodo Resultado
1      Manual 0.9941486
2 Computadora 0.9941486
"