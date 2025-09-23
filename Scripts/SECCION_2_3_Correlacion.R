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
#     explicar las estimaciones de la gente. Teniendo esto presente, dibuja un diagrama
#     de dispersión con estos datos.

# (b) Calcula la correlación r (utiliza tu calculadora). Explica, basándote en el
#     diagrama de dispersión, por qué r es razonable.

# (c) Las estimaciones son todas mayores que los valores reales. Este hecho, ¿in
#     fl uye de alguna manera en la correlación? ¿Cómo cambiaría r si todos los valores
#     estimados fuesen 100 calorías más altos?

# (d) Las estimaciones son demasiado altas para los espaguetis y los pasteles.
#     Señala estos puntos en el diagrama de dispersión. Calcula r para los ocho alimen
#     tos restantes. Explica por qué r cambia en el sentido en que lo hace.

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
#     te de inteligencia y el recuento de IRM. Utiliza símbolos distintos para hombres
#     y mujeres. Además, halla la correlación entre ambas variables para los 40 sujetos,
#     para los hombres y para las mujeres.

# (b) En general, los hombres son más corpulentos que las mujeres, por tanto
#     sus cerebros suelen ser más grandes. ¿Cómo se muestra este efecto en tu dia
#     grama? Halla la media del recuento de IRM para hombres y para mujeres para
#     comprobar si existe diferencia.

# (c) Tus resultados en (b) sugieren que para analizar la relación entre el coefi
#     ciente de inteligencia y el peso del cerebro, es mejor separar hombres y mujeres.
#     Utiliza tus resultados en (a) para comentar la naturaleza y la fuerza de esta rela
#     ción para hombres y mujeres de forma separada.

# **************************************************
# PREGUNTA 2.25 - Un cambio en las unidades de medida
# **************************************************

# Considera los siguientes datos:
# x: -4 -4 -3 3 4 4
# y: 0,5 -0,6 -0,5 0,5 0,5 -0,6

# (a) Dibuja un diagrama de dispersión con los datos anteriores en el que la
#     escala de las ordenadas y la de las abscisas vayan de -6 a 6.

# (b) Calcula, a partir de x e y, los valores de las nuevas variables: x* = x/10
#     e y* = 10y. Dibuja y* en relación con x* en el mismo diagrama de dispersión
#     utilizando otros símbolos. El aspecto de los dos diagramas es muy diferente.

# (c) Utiliza una calculadora para hallar la correlación entre x e y. Luego, halla
#     la correlación entre x* e y*. ¿Cuál es la relación entre las dos correlaciones? Expli
#     ca por qué este resultado no es sorprendente.

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
#     fesor Cruz. 
# (b) Escribe en un lenguaje sencillo (no utilices la palabra "correlación")
#     lo que quería decir el profesor Cruz.

# **************************************************
# PREGUNTA 2.27 - Diversificación de inversiones
# **************************************************

# Un artículo en una revista de una asociación dice: "Una cartera bien diversificada 
# incluye asientos con correlaciones bajas". El artículo incluye una tabla de correlaciones 
# entre los rendimientos de varios tipos de inversiones. Por ejemplo, la correlación 
# entre unos bonos municipales y acciones de grandes empresas es 0,50 y la 
# correlación entre los bonos municipales y acciones de pequeñas empresas es 0,21.

# (a) María invierte mucho en bonos municipales y quiere diversificar sus in
#     versiones añadiendo unas acciones que tengan unos rendimientos que no sigan
#     la misma tendencia que los rendimientos de sus bonos. Para conseguir su propó
#     sito, ¿qué tipo de acciones debe escoger María, las acciones de grandes empresas
#     o acciones de pequeñas empresas? Justifica tu respuesta.

# (b) Si María quiere una inversión que tienda a aumentar cuando los rendi
#     mientos de sus bonos tiendan a disminuir, ¿qué tipo de correlación debe buscar?

# **************************************************
# PREGUNTA 2.28 - Velocidad y consumo de gasolina
# **************************************************

# Los datos del ejercicio 2.20 se presentaron para mostrar un ejemplo de una 
# relación curvilínea fuerte para la cual, sin embargo, r = 0. El ejercicio 2.6 
# proporciona datos sobre el consumo del Ford Escort con relación a la velocidad.

# (a) Dibuja un diagrama de dispersión si no lo hiciste en el ejercicio 2.6. 
# (b) Calcula la correlación y explica por qué r está cerca de 0 a pesar de la 
#     fuerte relación entre la velocidad y el consumo.

# **************************************************
# PREGUNTA 2.29 - ¿Dónde está el error?
# **************************************************

# Cada una de las siguiente afirmaciones contiene un error. Explica en cada 
# caso dónde está la incorrección.

# (a) "Hay una correlación alta entre el sexo de los trabajadores y sus ingresos."

# (b) "Hallamos una correlación alta (r = 1,09) entre las evaluaciones de los
#     profesores hechas por los estudiantes y las hechas por otros profesores."

# (c) "La correlación hallada entre la densidad de siembra y el rendimiento del
#     maíz fue de r = 0,23 hectolitros."