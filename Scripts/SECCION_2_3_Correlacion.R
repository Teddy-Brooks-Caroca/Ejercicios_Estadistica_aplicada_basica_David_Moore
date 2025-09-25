# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# SECCIÓN 2.3 "La correlación"

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

# **************************************************
# PREGUNTA 2.18 - Reflexiones sobre la correlación
# **************************************************

# La figura 2.5 es un diagrama de dispersión que relaciona las notas medias 
# escolares y los coeficientes de inteligencia de 78 estudiantes de primero de bachillerato.

# (a) La correlación r de estos datos, ¿es próxima a −1, claramente negativa
# aunque no próxima a −1, próxima a 0, próxima a 1, claramente positiva pero no
# próxima a 1? Justifica tu respuesta.

install.packages("magick")
library(magick)

figura_2_05 <- image_read("figura_2_05.png")

print(figura_2_05)

"Es claramente positiva, pero no proxima a 1, esto es así debido a las observaciones atipicas  que 
hay en la parte inferior izquierda del grafico, sin embargo la fuerza de la misma nos puede hacer
hipotetizar que un coeficiente alto de iteligencia se expresa en mejores notas."

# (b) La figura 2.6, muestra las calorías y los contenidos de sodio de 17 marcas
# de salchichas. En esta ocasión, la correlación ¿es más próxima a 1 que la correla
# ción de la figura 2.5? ¿es más próxima a 0? Justifica tu respuesta.

figura_2_06 <- image_read("figura_2_06.png")

print(figura_2_06)

"En este caso es mas próxima a 1, lo cual se puede deber a la menor cantidad de observaciones,
no obstante vuelven a a aparecer observaciones atipicas(1) que pueden afectar la correlación entre variables"

# (c) Tanto la figura 2.5 como la figura 2.6 contienen observaciones atípicas. La
# eliminación de estas observaciones, ¿aumentará el coeficiente de correlación de
# una figura y lo disminuirá en la otra? ¿Qué ocurre en cada figura? ¿Por qué?

"En ambos casos la eliminción de las observaciones atípicas aumentará el coeficiente
de correlación de ambos estudios"

# **************************************************
# PREGUNTA 2.19 - Correlación entre edades de esposos
# **************************************************

# Si las mujeres siempre se casaran con hombres que fueran 2 años mayores
# que ellas, ¿cuál sería la correlación entre las edades de las esposas y las edades de
# sus maridos? (Sugerencia: dibuja un diagrama de dispersión con varias edades.)

edades_matrimonios <- data.frame(
  edades_esposas = c(25,23,45,19,36,33,41),
  edades_esposos = c(27,25,47,21,38,35,43)
)

plot(edades_matrimonios$edades_esposas,
     edades_matrimonios$edades_esposos,
     main = "Correlación entre edades de esposos",
     xlab = "Edades de las esposas (en años)",
     ylab = "Edades de los esposos (en años)",
     col= "blue",
     pch= 18)
grid()

media_mujeres <- mean(edades_matrimonios$edades_esposas)
media_hombres <- mean(edades_matrimonios$edades_esposos)

desv_mujeres <- sd(edades_matrimonios$edades_esposas)
desv_hombres <- sd(edades_matrimonios$edades_esposos)

z_mujeres <- (edades_matrimonios$edades_esposas - media_mujeres) / desv_mujeres
z_hombres <- (edades_matrimonios$edades_esposos - media_hombres) / desv_hombres

n_muestras <- 7

r <- (1 / (n_muestras - 1)) * sum(z_mujeres * z_hombres)

print(r)

"
r = 1
"

# **************************************************
# PREGUNTA 2.20 - Falta de correlación, pero asociación fuerte
# **************************************************

# A medida que aumenta la velocidad, el consumo de un automóvil disminuye 
# al principio y luego aumenta. Supón que esta relación es muy regular, tal como 
# muestran los siguientes datos de la velocidad (kilómetros por hora) y el consumo 
# (litros por 100 km).

# Velocidad (km/h):     30   45   55   70   85
# Consumo (litros/100 km): 9,8  8,4  7,8  8,4  9,8

# (a) Dibuja un diagrama de dispersión del consumo con relación a la velocidad.

desempeño_auto <- data.frame(
  Velocidad = c(30,45,55,70,85),
  Consumo = c(9.8,8.4,7.8,8.4,9.8)
)

print(desempeño_auto)

plot(desempeño_auto$Velocidad,
     desempeño_auto$Consumo,
     main = "Desempeño de automóvil",
     xlab = "Velocidad (km/h)",
     ylab = "Consumo (litros/100 km)",
     col = "red",
     pch = 19)
grid()

# (b) Muestra que la correlación es r = 0. 

media_velocidad <- mean(desempeño_auto$Velocidad)
media_consumo <- mean(desempeño_auto$Consumo)

desv_velocidad <- sd(desempeño_auto$Velocidad)
desv_consumo <- sd(desempeño_auto$Consumo)

z_velocidad <- (desempeño_auto$Velocidad - media_velocidad) / desv_velocidad
z_consumo <- (desempeño_auto$Consumo - media_consumo) / desv_consumo

n_muestras <- 5

r <- (1 / (n_muestras - 1)) * sum(z_velocidad * z_consumo)

print(r)

"
r = 0.03339663
"
# (c) Explica por qué r es 0, a pesar de que existe una fuerte relación entre la 
# velocidad y el consumo.

"Porque la correlación sirve para medir la relación entre variables lineales NO relaciones
curvas"

# **************************************************
# PREGUNTA 2.21 - El profesor Moore y la natación
# **************************************************

# El ejercicio 2.12 proporciona datos sobre el tiempo que el profesor Moore, 
# un hombre de mediana edad, tarda en nadar 1.800 metros y su ritmo cardíaco posterior.

# (a) Si no lo hiciste en el ejercicio 2.12, calcula el coeficiente de correlación r.
# Explica, después de analizar el diagrama de dispersión, por qué el valor de r es
# razonable.

ejercicio_2_21 <- read.csv("ejercicio_2_12_tiempos.csv",sep = ";")

head(ejercicio_2_21)

media_minutos <- mean(ejercicio_2_21$Minutos)
media_pulsaciones <- sd(ejercicio_2_21$Pulsaciones)

desv_minutos <- sd(ejercicio_2_21$Minutos)
desv_pulsaciones <- sd(ejercicio_2_21$Pulsaciones)

z_minutos <- (ejercicio_2_21$Minutos - media_minutos) / desv_minutos
z_pulsaciones <- (ejercicio_2_21$Pulsaciones - media_pulsaciones) / desv_pulsaciones

n_muestras <- nrow(ejercicio_2_21)

r <- (1 / (n_muestras - 1)) * sum(z_minutos * z_pulsaciones)

print(r)

"
r = -0.7459841

En este caso la correlación es negativa pero cercana a -1, lo que significa que es 
casi lineal, por lo tanto a mayor cantidad de minutos, menor es el ritmo cardíaco
registrado para el doctor Moore
"

# (b) Supón que los tiempos se hubieran medido en segundos. Por ejemplo,
# 34,12 minutos serían 2.047 segundos. ¿Cambiaría el valor de r?

"No, ya que la correlación no se restrigue a unidades de medición específicas"

# **************************************************
# PREGUNTA 2.22 - Peso corporal y nivel metabólico
# **************************************************

# La tabla 2.3 proporciona datos sobre el nivel metabólico y el peso magro de 
# 12 mujeres y 7 hombres.

# (a) Dibuja un diagrama de dispersión si no lo hiciste en el ejercicio 2.7. Utili
# za colores o símbolos distintos para las mujeres y para los hombres. ¿Crees que
# la correlación será aproximadamente igual para los hombres y las mujeres, o bas
# tante distinta para los dos grupos? ¿Por qué?

ejercicio_2_27 <- read.csv("ejercicio_2_07_peso.csv")

head(ejercicio_2_27)

str(ejercicio_2_27)

peso_mujeres <- ejercicio_2_27[ejercicio_2_27$Sexo == "M", ]
peso_hombres <- ejercicio_2_27[ejercicio_2_27$Sexo == "H", ]

plot(peso_mujeres$Peso_kg,
     peso_mujeres$Nivel_metabolico,
     main = "Relación entre peso y nivel metabólico",
     xlab = "Peso (kg)",
     ylab = "Nivel Metabolico",
     col = "green",
     pch = 16,
     cex = 1.5,
     xlim = range(ejercicio_2_27$Peso_kg),
     ylim = range(ejercicio_2_27$Nivel_metabolico))
points(peso_hombres$Peso_kg,
       peso_hombres$Nivel_metabolico,
       col = "blue",
       pch = 17,
       cex = 1.5)
grid()

"A simple vista, parece haber una correlación positiva, pero no cercana a 1, aunque en
hombre tiende a ser mas dispersa que en mujeres, lo cual puede deberse a ser un grupo
mas reducido de muestras."

# (b) Calcula r para el grupo de las mujeres y también para el grupo de los
#     hombres. (Utiliza la calculadora.)

correlacion_mujeres <- cor(peso_mujeres$Peso_kg,peso_mujeres$Nivel_metabolico)
correlacion_hombres <- cor(peso_hombres$Peso_kg,peso_hombres$Nivel_metabolico)

correlaciones_grupos <- data.frame(
  Grupo = c("Hombres", "Mujeres"),
  Correlación = c(correlacion_hombres,correlacion_mujeres)
)

print(correlaciones_grupos)

"
    Grupo Correlación
1 Hombres   0.5920653
2 Mujeres   0.8764527
"

# (c) Calcula el peso magro medio de las mujeres y de los hombres. El hecho de
# que, como media, los hombres sean más pesados que las mujeres, ¿influye en las
# correlaciones? Si es así, ¿por qué?

media_peso_hombres <- mean(peso_hombres$Peso_kg)
media_peso_mujeres <- mean(peso_mujeres$Peso_kg)

medias_pesos <- data.frame(
  Grupo = c("Hombres", "Mujeres"),
  Media = c(media_peso_hombres,media_peso_mujeres)
)

print(medias_pesos)

"
    Grupo    Media
1 Hombres 53.10000
2 Mujeres 43.03333

Sí, consideramos que la correlación se ve afectada por las observaciones atípicas
y que la misma se configura a partir de las medias de los muestras; un cambio en el
valor de las muestras puede afectar la correlación
"

# (d) El peso magro se midió en kilogramos. ¿Cuál sería la correlación si lo hu
# biéramos medido en libras? (2,2 libras equivalen a 1 kilogramo.)

ejercicio_2_27$Peso_lb <- ejercicio_2_27$Peso_kg * 2.2

n_cols <- ncol(ejercicio_2_27)

ejercicio_2_27 <- ejercicio_2_27[, c(1:2, n_cols, 3:(n_cols-1))]

print(ejercicio_2_27)

r_kilo <- cor(ejercicio_2_27$Peso_kg,ejercicio_2_27$Nivel_metabolico)
r_libra <- cor(ejercicio_2_27$Peso_lb,ejercicio_2_27$Nivel_metabolico)

resultados_peso <- data.frame(
  Medida = c("r_Kilo","r_Libra"),
  Valores = c(r_kilo,r_libra)
)

print(resultados_peso)

"
   Medida   Valores
1 r_Kilo  0.8647361
2 r_Libra 0.8647361

Tal como se aprecia la correlación no se ve afectada por unidades de medida específicas
"
# **************************************************
# PREGUNTA 2.23 - ¿Cuántas calorías?
# **************************************************

# Una industria agroalimentaria solicita a un grupo de 3.368 personas que 
# estimen el contenido en calorías de algunos alimentos. La tabla 2.5 muestra 
# las medias de sus estimaciones y el contenido real en calorías.

# Alimento                                 Calorías estimadas  Calorías reales
# ---------------------------------------------------------------------------
# 225 g de leche entera                    196                159
# 142 g de espaguetis con salsa de tomate  394                163
# 142 g de macarrones con queso            350                269
# Una rebanada de pan de trigo             117                61
# Una rebanada de pan blanco               136                76
# 57 g de caramelos                        364                260
# Una galleta salada                       74                 12
# Una manzana de tamaño medio              107                80
# Una patata de tamaño medio               160                88
# Una porción de pastel de crema           419                160

# (a) Creemos que el contenido real en calorías de los alimentos, puede ayudar a
# explicar las estimaciones de la gente. Teniendo esto presente, dibuja un diagrama
# de dispersión con estos datos.

calorias_estimadas_y_reales <- data.frame(
  Alimento = c("225 g de leche entera","142 g de espaguetis con salsa de tomate","142 g de macarrones con queso",
               "Una rebanada de pan trigo","Una rebanada de pan blanco","57 g de caramelos","Una galleta salada",
               "Una manzana de tamaño medio","Una patata de tamaño medio","Una porción de pastel de crema"),
  Calorias_estimadas = c(196,394,350,117,136,364,74,107,160,419),
  Calorias_reales = c(159,163,269,61,76,260,12,80,88,160)
)

print(calorias_estimadas_y_reales)

plot(calorias_estimadas_y_reales$Calorias_reales,
     calorias_estimadas_y_reales$Calorias_estimadas,
     main = "Estimacion calorica en alimentos",
     xlab = "Calorias reales",
     ylab = "Calorias estimadas",
     col = "blue",
     pch = 16,
     cex = 1.2,
     xlim = c(0, 300),
     ylim = c(0, 450))
text(calorias_estimadas_y_reales$Calorias_reales,
     calorias_estimadas_y_reales$Calorias_estimadas,
     labels = calorias_estimadas_y_reales$Alimento,
     pos = 3, cex = 0.7, col = "darkblue")
grid()

# (b) Calcula la correlación r (utiliza tu calculadora). Explica, basándote en el
# diagrama de dispersión, por qué r es razonable.

r <- cor(calorias_estimadas_y_reales$Calorias_reales,calorias_estimadas_y_reales$Calorias_estimadas)

print(r)

"
r = 0.8245016

En este caso la correlación es cercana a 1, con lo que podemos hipotetizar que las estiamciones
hechas por las personas, aunque mayores a las reales, tienden a mostrar una cantidad de acuerdo
a un alimento en especifico
"

# (c) Las estimaciones son todas mayores que los valores reales. Este hecho, ¿in
# fluye de alguna manera en la correlación? ¿Cómo cambiaría r si todos los valores
# estimados fuesen 100 calorías más altos?

calorias_estimadas_y_reales$Calorias_estimadas_aumentadas <- calorias_estimadas_y_reales$Calorias_estimadas + 100

print(calorias_estimadas_y_reales)

r_aumentada <- cor(calorias_estimadas_y_reales$Calorias_reales,calorias_estimadas_y_reales$Calorias_estimadas_aumentadas)

comparativa_r <- data.frame(
  Medidas = c("Calorias estimadas", "Calorias estimadas aumentadas"),
  Valores = c(r,r_aumentada)
)

print(comparativa_r)

write.csv(calorias_estimadas_y_reales, "ejercicio_2_23_calorias.csv", row.names = FALSE)

"
                        Medidas   Valores
1            Calorias estimadas 0.8245016
2 Calorias estimadas aumentadas 0.8245016
"

# (d) Las estimaciones son demasiado altas para los espaguetis y los pasteles.
# Señala estos puntos en el diagrama de dispersión. Calcula r para los ocho alimen
# tos restantes. Explica por qué r cambia en el sentido en que lo hace.

calorias_filtradas <- calorias_estimadas_y_reales[!(calorias_estimadas_y_reales$Alimento %in% 
                                                      c("142 g de espaguetis con salsa de tomate",
                                                        "Una porción de pastel de crema")), ]

puntos_fuera <- calorias_estimadas_y_reales$Alimento %in% 
  c("142 g de espaguetis con salsa de tomate","Una porción de pastel de crema")

print(calorias_filtradas)

r_filtrada <- cor(calorias_filtradas$Calorias_reales,
                  calorias_filtradas$Calorias_estimadas)

print(r_filtrada)

comparativa_r <- data.frame(
  Medidas = c("r original","r filtrado"),
  Valores = c(r,r_filtrada)
)

print(comparativa_r)

plot(calorias_estimadas_y_reales$Calorias_reales,
     calorias_estimadas_y_reales$Calorias_estimadas,
     main = "Estimacion calorica en alimentos",
     xlab = "Calorias reales",
     ylab = "Calorias estimadas",
     col = "blue",
     pch = 16,
     cex = 1.2,
     xlim = c(0, 300),
     ylim = c(0, 450))
text(calorias_estimadas_y_reales$Calorias_reales,
     calorias_estimadas_y_reales$Calorias_estimadas,
     labels = calorias_estimadas_y_reales$Alimento,
     pos = 3, cex = 0.7, col = "darkblue")
points(calorias_estimadas_y_reales$Calorias_reales[puntos_fuera],
       calorias_estimadas_y_reales$Calorias_estimadas[puntos_fuera],
       col = "red", pch = 17, cex = 1.5)
grid()

"    Medidas   Valores
1 r original 0.8245016
2 r filtrado 0.9837412

Esto ocurre porque esos dos valores actúan como outliers: se alejan de la tendencia 
lineal general.
Al eliminarlos, las estimaciones de los 8 alimentos restantes siguen mucho mejor 
la relación lineal, y por eso r aumenta notablemente.
"
# **************************************************
# PREGUNTA 2.24 - Peso del cerebro y coeficiente de inteligencia
# **************************************************

# La gente que tiene un cerebro mayor, ¿tiene también un coeficiente de inteligencia 
# mayor? Un estudio realizado con 40 sujetos voluntarios, 20 hombres y 20 mujeres, 
# proporciona una explicación. El peso del cerebro se determinó mediante una imagen 
# obtenida por resonancia magnética (IRM). El coeficiente de inteligencia (CI) se
# midió mediante la prueba Wechsler.

# HOMBRES:                          MUJERES:
# IRM        CI    IRM        CI    IRM        CI    IRM        CI
# ----------------------------------------------------------------
# 1.001.121  140   1.038.437  139   816.932    133   951.545    137
# 965.353    133   904.858    89    928.799    99    991.305    138
# 955.466    133   1.079.549  141   854.258    92    833.868    132
# 924.059    135   945.088    100   856.472    140   878.897    96
# 889.083    80    892.420    83    865.363    83    852.244    132
# 905.940    97    955.003    139   808.020    101   790.619    135
# 935.494    141   1.062.462  103   831.772    91    798.612    85
# 949.589    144   997.925    103   793.549    77    866.662    130
# 879.987    90    949.395    140   857.782    133   834.344    83
# 930.016    81    935.863    89    948.066    133   893.983    88

# (a) Haz un diagrama de dispersión para mostrar la relación entre el coeficien
# te de inteligencia y el recuento de IRM. Utiliza símbolos distintos para hombres
# y mujeres. Además, halla la correlación entre ambas variables para los 40 sujetos,
# para los hombres y para las mujeres.

ejercicio_2_24 <- read.csv("ejercicio_2_24_cerebro.csv")

View(ejercicio_2_24)

coeficiente_hombres <- ejercicio_2_24[ejercicio_2_24$Genero == "Hombre", ]

coeficiente_mujeres <- ejercicio_2_24[ejercicio_2_24$Genero == "Mujer", ]


plot(coeficiente_hombres$CI,
     coeficiente_hombres$IRM,
     main = "Comparativa por genero de IRM y CI",
     xlab = "CI",
     ylab = "IRM",
     col = "blue",
     pch = 16,
     cex = 1.5,
     xlim = range(ejercicio_2_24$CI),
     ylim = range(ejercicio_2_24$IRM))
points(coeficiente_mujeres$CI,
       coeficiente_mujeres$IRM,
       col = "red",
       pch = 17,
       cex = 1.5)
grid()

# (b) En general, los hombres son más corpulentos que las mujeres, por tanto
# sus cerebros suelen ser más grandes. ¿Cómo se muestra este efecto en tu dia
# grama? Halla la media del recuento de IRM para hombres y para mujeres para
# comprobar si existe diferencia.

media_IRM_hombres <- mean(coeficiente_hombres$IRM)
media_IRM_mujeres <- mean(coeficiente_mujeres$IRM)

comparativa_media_IRM <- data.frame(
  Grupo = c("Hombres", "Mujeres"),
  Media_IRM = c(media_IRM_hombres,media_IRM_mujeres)
)

print(comparativa_media_IRM)

"
    Grupo Media_IRM
1 Hombres  954855.4
2 Mujeres  862654.6
"
# (c) Tus resultados en (b) sugieren que para analizar la relación entre el coefi
# ciente de inteligencia y el peso del cerebro, es mejor separar hombres y mujeres.
# Utiliza tus resultados en (a) para comentar la naturaleza y la fuerza de esta rela
# ción para hombres y mujeres de forma separada.

"Al analizar hombres y mujeres por separado, la relación entre peso del cerebro (IRM) 
y coeficiente intelectual (CI) resulta positiva en ambos grupos, aunque con distinta fuerza: 
en los hombres la correlación es más clara y consistente, mientras que en las mujeres 
la asociación es más débil y dispersa. Esto confirma que separar por género es más adecuado, 
ya que la media de mayor tamaño cerebral en hombres podría sesgar la relación en el análisis conjunto."

# **************************************************
# PREGUNTA 2.25 - Un cambio en las unidades de medida
# **************************************************

# Considera los siguientes datos:
# x: -4 -4 -3 3 4 4
# y: 0,5 -0,6 -0,5 0,5 0,5 -0,6

# (a) Dibuja un diagrama de dispersión con los datos anteriores en el que la
# escala de las ordenadas y la de las abscisas vayan de -6 a 6.

puntos_coordenadas <- data.frame(
  x = c(-4,-4,-3,3,4,4),
  y = c(0.5,-0.6,-0.5,0.5,0.5,-0.6)
)

print(puntos_coordenadas)

plot(puntos_coordenadas$x,
     puntos_coordenadas$y,
     main = "Puntos coordenadas",
     xlab = "x (ordenadas)",
     ylab = "y (abscisas)",
     col = "blue",
     pch = 16,
     xlim = c(-6,6),
     ylim = c(-6,6))
grid()

# (b) Calcula, a partir de x e y, los valores de las nuevas variables: x* = x/10
# e y* = 10y. Dibuja y* en relación con x* en el mismo diagrama de dispersión
# utilizando otros símbolos. El aspecto de los dos diagramas es muy diferente.

puntos_coordenadas$x_star <- puntos_coordenadas$x / 10
puntos_coordenadas$y_star <- puntos_coordenadas$y * 10

plot(puntos_coordenadas$x,
     puntos_coordenadas$y,
     main = "Cambio de unidades de medida",
     xlab = "x / x*",
     ylab = "y / y*",
     col = "blue",
     pch = 16,
     xlim = c(-6,6),
     ylim = c(-6,6))
points(puntos_coordenadas$x_star,
       puntos_coordenadas$y_star,
       col = "red",
       pch = 17,
       cex = 1.2)
grid()

# (c) Utiliza una calculadora para hallar la correlación entre x e y. Luego, halla
# la correlación entre x* e y*. ¿Cuál es la relación entre las dos correlaciones? Expli
# ca por qué este resultado no es sorprendente.

r_original <- cor(puntos_coordenadas$x,puntos_coordenadas$y)
r_star <- cor(puntos_coordenadas$x_star,puntos_coordenadas$y_star)

relacion_r <- data.frame(
  Tipo_correlacion = c("r original","r star"),
  Valores = c(r_original,r_star)
)

print(relacion_r)
"
Tipo_correlacion   Valores
r original         0.2531007
r star             0.2531007

La correlación no cambia porque es invariante frente a cambios de escala y de unidades.
Al dividir x entre 10 o multiplicar y por 10, se modifican las magnitudes de los datos 
pero no la forma de la relación lineal entre ambas variables.
La correlación depende únicamente de la dirección y fuerza de la relación lineal, no de 
la escala de medida. Por eso, no es sorprendente que ambas correlaciones sean idénticas.
"
# **************************************************
# PREGUNTA 2.26 - Docencia e investigación
# **************************************************

# Un periódico universitario entrevista a un psicólogo a propósito de las evaluaciones 
# que hacen los estudiantes de sus profesores. El psicólogo afirma: "La evidencia 
# demuestra que la correlación entre la capacidad investigadora de los profesores 
# y la evaluación docente que hacen los estudiantes es próxima a cero". El titular 
# del periódico dice: "El profesor Cruz dice que los buenos investigadores tienden 
# a ser malos profesores y viceversa".

# (a) Explica por qué el titular del periódico no refleja el sentido de las palabras del pro
# fesor Cruz. 

"El titular del periódico no refleja correctamente las palabras del profesor Cruz porque:
Correlación cercana a cero significa que no hay relación entre ambas variables
El titular interpreta 'correlación cero' como una relación inversa ('buenos investigadores = malos profesores')
En realidad, correlación cero indica que ser buen investigador no predice si se es buen o mal profesor, y viceversa
El periódico convierte una ausencia de relación en una relación inversa, lo cual es un error estadístico grave"

# (b) Escribe en un lenguaje sencillo (no utilices la palabra "correlación")
# lo que quería decir el profesor Cruz.

"Lo que realmente quiso decir el profesor Cruz es:

'El hecho de que un profesor sea buen investigador no nos da ninguna pista sobre si 
será bueno o malo enseñando. De la misma manera, saber que un profesor es muy bueno 
en clase no nos dice nada sobre su capacidad para hacer investigaciones. Ambas habilidades 
parecen ser independientes.'

En términos más simples:
Los buenos investigadores pueden ser buenos profesores, malos profesores, o regulares

Los malos investigadores pueden ser buenos profesores, malos profesores, o regulares

No hay patrón: conocer una habilidad no ayuda a predecir la otra

Analogía útil:
'Es como saber si a alguien le gusta el fútbol: eso no nos dice si le gustará o no el cine. 
Puede gustarle ambos, ninguno, o solo uno de los dos. No hay relación entre estos gustos'"

# **************************************************
# PREGUNTA 2.27 - Diversificación de inversiones
# **************************************************

# Un artículo en una revista de una asociación dice: "Una cartera bien diversificada 
# incluye asientos con correlaciones bajas". El artículo incluye una tabla de correlaciones 
# entre los rendimientos de varios tipos de inversiones. Por ejemplo, la correlación 
# entre unos bonos municipales y acciones de grandes empresas es 0,50 y la 
# correlación entre los bonos municipales y acciones de pequeñas empresas es 0,21.

# (a) María invierte mucho en bonos municipales y quiere diversificar sus in
# versiones añadiendo unas acciones que tengan unos rendimientos que no sigan
# la misma tendencia que los rendimientos de sus bonos. Para conseguir su propó
# sito, ¿qué tipo de acciones debe escoger María, las acciones de grandes empresas
# o acciones de pequeñas empresas? Justifica tu respuesta.

"María debe escoger acciones de pequeñas empresas porque tienen una correlación más baja (0.21) 
con los bonos municipales que las acciones de grandes empresas (0.50). Una correlación de 0.21 
indica que los rendimientos de estas acciones siguen menos la misma tendencia que los bonos, lo 
que proporciona mejor diversificación al reducir el riesgo de que todas sus inversiones suban o 
bajen al mismo tiempo."

# (b) Si María quiere una inversión que tienda a aumentar cuando los rendimientos 
# de sus bonos tiendan a disminuir, ¿qué tipo de correlación debe buscar?

"María debe buscar una correlación negativa (valor entre -1 y 0). Una correlación negativa significa 
que cuando los rendimientos de sus bonos disminuyan, los rendimientos de la otra inversión tenderán a 
aumentar, protegiendo así su cartera contra pérdidas generalizadas."

# **************************************************
# PREGUNTA 2.28 - Velocidad y consumo de gasolina
# **************************************************

# Los datos del ejercicio 2.20 se presentaron para mostrar un ejemplo de una 
# relación curvilínea fuerte para la cual, sin embargo, r = 0. El ejercicio 2.6 
# proporciona datos sobre el consumo del Ford Escort con relación a la velocidad.

# (a) Dibuja un diagrama de dispersión si no lo hiciste en el ejercicio 2.6. 

ejercicio_2_28 <- read.csv("ejercicio_2_06_consumo_coche.csv")

head(ejercicio_2_28)

str(ejercicio_2_28)

plot(ejercicio_2_28$Velocidad_km_h,
     ejercicio_2_28$Consumo_litros_100km,
     main = "Consumo modelo Ford Escort",
     xlab = "Velocidad (km/h)",
     ylab = "Consumo (litros / 100 km)",
     col = "red",
     pch = 16,
     cex = 1.2)
grid()

# (b) Calcula la correlación y explica por qué r está cerca de 0 a pesar de la 
# fuerte relación entre la velocidad y el consumo.

r_ford <- cor(ejercicio_2_28$Velocidad_km_h,ejercicio_2_28$Consumo_litros_100km)

print(r_ford)

"
r = -0.1716216

La correlación r = -0.17 está cerca de cero porque la relación entre velocidad y consumo no es lineal, 
sino curvilínea (en forma de U). El coeficiente de correlación lineal (r) solo mide relaciones lineales, 
pero en este caso:

A bajas velocidades (ej: 20-40 km/h) el consumo es alto

A velocidades medias (ej: 60-80 km/h) el consumo es mínimo

A altas velocidades (ej: 100-120 km/h) el consumo vuelve a aumentar

Esta relación curvilínea hace que la línea recta que mejor se ajusta tenga pendiente cercana a cero, 
dando una correlación lineal baja, a pesar de que existe una relación muy fuerte pero no lineal entre 
las variables.

En resumen: r mide solo relaciones lineales, pero esta relación es curvilínea, por lo que r no captura 
la verdadera fuerza de la asociación."

# **************************************************
# PREGUNTA 2.29 - ¿Dónde está el error?
# **************************************************

# Cada una de las siguiente afirmaciones contiene un error. Explica en cada 
# caso dónde está la incorrección.

# (a) "Hay una correlación alta entre el sexo de los trabajadores y sus ingresos."

"El error está en calcular correlación con una variable categórica (sexo). El coeficiente de correlación (r) 
solo puede calcularse entre dos variables numéricas continuas. El 'sexo' es una variable categórica (hombre/mujer), 
no numérica, por lo que no tiene sentido matemático calcular una correlación con los ingresos."

# (b) "Hallamos una correlación alta (r = 1,09) entre las evaluaciones de los
# profesores hechas por los estudiantes y las hechas por otros profesores."

"El error está en que r = 1.09 es imposible. El coeficiente de correlación r siempre debe estar entre -1 y 1 (-1 ≤ r ≤ 1). 
Un valor de 1.09 está fuera de este rango posible, lo que indica un error en el cálculo o en la interpretación."

# (c) "La correlación hallada entre la densidad de siembra y el rendimiento del
#     maíz fue de r = 0,23 hectolitros."

"El error está en añadir unidades ('hectolitros') al coeficiente de correlación. La correlación r es un número adimensional 
que no tiene unidades. Mide la fuerza y dirección de una relación, pero no está expresada en unidades de medida como 
hectolitros, metros o kilogramos."

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::
