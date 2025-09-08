# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO I "Análisis de distribuciones"

# REPASO CAPÍTULO I

getwd()

setwd("C://Users//brook//OneDrive//Escritorio//Portafolio//Ejercicios_Estadistica_aplicada_b-sica_David_Moore//Archivos")

# **************************************************
# PREGUNTA 1.72 - Preferencias en la votación
# **************************************************

# Las preferencias políticas de los españoles dependen de la edad, de los ingresos y del sexo de los votantes. 
# Una investigadora selecciona una amplia muestra de votantes. De cada uno de ellos, la investigadora 
# registra el sexo, la edad, los ingresos y si votó al Partido Popular, al Partido Socialista o a otro partido 
# en las últimas elecciones. 

# (a) De estas variables, ¿cuáles son categóricas y cuáles son cuantitativas?

"
CATEGÓRICAS: sexo
CUANTITATIVAS: ingresos, edad
"

# **************************************************
# PREGUNTA 1.73 - Armas asesinas
# **************************************************

# El Anuario Estadístico de 1997 de los Estados Unidos, proporciona datos del FBI sobre asesinatos en 1995. 
# En ese año, el 55,8% de todos los asesinatos se cometieron con pistolas, el 12,4% con otras armas de fuego, 
# el 12,6% con armas blancas, el 5,9% con alguna parte del cuerpo (en general las manos y los pies) y el 4,5% 
# con algún objeto contundente. 

# (a) Representa gráficamente estos datos. 

datos_asesinatos <- data.frame(
  Tipos_asesinatos = c("Pistolas","Otra arma de fuego","Arma blanca","Parte del cuerpo","Objeto contundente"),
  Porcentajes = c(55.8,12.4,12.6,5.9,4.5)
)

print(datos_asesinatos)

pie(datos_asesinatos$Porcentajes,
    main = "Datos del FBI sobre asesinatos en 1995",
    labels = datos_asesinatos$Tipos_asesinatos)

write.csv(datos_asesinatos,"ejercicio_1_73_asesinatos.csv")

# (b) ¿Necesitas la categoría de "otros métodos"?

sum(datos_asesinatos$Porcentajes)

datos_asesinatos_corregidos <- data.frame(
  Tipos_asesinatos = c("Pistolas","Otra arma de fuego","Arma blanca","Parte del cuerpo","Objeto contundente","Otros metodos"),
  Porcentajes = c(55.8,12.4,12.6,5.9,4.5,8.8)
)

print(datos_asesinatos_corregidos)

pie(datos_asesinatos_corregidos$Porcentajes,
    main = "Datos del FBI sobre asesinatos en 1995",
    labels = datos_asesinatos_corregidos$Tipos_asesinatos)

"La totalidad de los porcentajes tiene como resultado 91.2%, con lo cual falta una fracción
que puede suplir la categoria 'otros metodos'"

# **************************************************
# PREGUNTA 1.74 - Nunca en domingo
# **************************************************

# En la provincia canadiense de Ontario se ha realizado un estudio estadístico sobre el funcionamiento 
# del sistema de sanidad pública. Los diagramas de barras de la figura 1.27 proceden del estudio de los ingresos 
# y las altas de los hospitales de Ontario. Estos diagramas muestran el número de pacientes con problemas 
# de corazón que fueron ingresados y dados de alta cada día de la semana durante un periodo de 2 años.

# (a) Explica por qué no cabe esperar diferencias en el número de ingresos de pacientes con cardiopatías 
# en los distintos días de la semana. ¿Es ésta una deducción correcta a partir de los datos que se aportan?

install.packages("magick")
library(magick)

figura_1_27 <- image_read("figura_1_27.png")

print(figura_1_27)

"No esperamos diferencias marcadas porque los eventos cardiovasculares agudos no se programan y deberían 
ocurrir de forma aproximadamente uniforme a lo largo de la semana. El diagrama de ingresos es casi plano, 
lo que es compatible con esa idea. Sin embargo, el gráfico por sí solo no demuestra igualdad exacta entre días; 
solo sugiere que no hay diferencias sustanciales."

# (b) Describe la distribución de las altas. ¿Existe alguna diferencia con la distribución de ingresos? 
# ¿Cómo se puede explicar esta diferencia?

"Las altas muestran un patrón semanal: muchas de lunes a viernes (con pico el viernes) y muy pocas el fin de semana, 
especialmente el domingo. A diferencia de los ingresos, que son relativamente uniformes, las altas dependen de procesos 
organizativos: hay más recursos y trámites disponibles en días hábiles y se tiende a concentrar las altas antes del fin 
de semana. Por eso, la diferencia entre ambos gráficos responde a la planificación hospitalaria, no a cambios reales en 
la ocurrencia de cardiopatías."

# **************************************************
# PREGUNTA 1.75 - De casa a la universidad
# **************************************************

# El profesor Moore, autor de este libro, que vive a unos kilómetros del campus universitario de la 
# Universidad de Purdue, ha registrado durante 42 días el tiempo que tarda conduciendo desde su casa hasta 
# la universidad. He aquí los tiempos (en minutos) correspondientes a 42 días consecutivos:

# 8,25 7,83 8,30 8,42 8,50 8,67 8,17 9,00 9,00 8,17 7,92
# 9,00 8,50 9,00 7,75 7,92 8,00 8,08 8,42 8,75 8,08 9,75
# 8,33 7,83 7,92 8,58 7,83 8,42 7,75 7,42 6,75 7,42 8,50
# 8,67 10,17 8,75 8,58 8,67 9,17 9,08 8,83 8,67

# (a) Dibuja un diagrama de tallos correspondiente a estos datos. (Redondea a décimas de minuto y divide los tallos.) 
# La distribución de datos, ¿es aproximadamente simétrica, claramente asimétrica o ninguna de las dos cosas? 
# ¿Existen observaciones atípicas?

ejercicio_1_75 <- read.csv("ejercicio_1_75_tiempos.csv")

View(ejercicio_1_75)

stem(ejercicio_1_75$Valores, scale = 2)

"Si no dividimos  los tallos podemos ver una distribución aproximadamente simétrica, sin embargo al dividir los tallos
tenemos una curva asimetrica a la izquierda, lo que significa que el doctor demora mas en algunos días, sin embargo debemos considerar
las observaciones atípicas (10.17)"

# (b) Dibuja un diagrama temporal con estos datos. (Marca en el eje de abscisas los días consecutivos desde el 1 hasta el 42.) 
# El gráfico no deja entrever ninguna tendencia. De todas formas, se observa que hay un día en el que la duración del 
# trayecto fue muy corta y dos días en que fue muy larga. Señala estos valores en tu gráfico.

plot(ejercicio_1_75$Día,
     ejercicio_1_75$Valores,
     main = "Registro de tiempos",
     xlab = "Días (consecutivos)",
     ylab = "Tiempo (en minutos)",
     type = "o",
     xaxt = "n",
     yaxt = "n")
axis(1, at = ejercicio_1_75$Día, labels = ejercicio_1_75$Día, las = 2, cex.axis = 0.8)
axis(2, at = ejercicio_1_75$Valores, labels = ejercicio_1_75$Valores, las = 1, cex.axis = 0.8)
points(which.min(ejercicio_1_75$Valores),
       min(ejercicio_1_75$Valores), col = "red", pch = 19)
points(which.max(ejercicio_1_75$Valores),
       max(ejercicio_1_75$Valores), col = "blue", pch = 19)
grid()


# (c) Las tres observaciones que se salen un poco de la distribución general se pueden explicar. El día que el profesor Moore 
# tardó muy poco tiempo en llegar a la universidad corresponde al Día de Acción de Gracias (Thanksgiving Day), que en EEUU 
# es festivo. Los dos días que tardó mucho más de lo normal corresponden, por un lado, a un día en el que ocurrió un accidente 
# con las inevitables retenciones, y, por otro, a un día en el que se produjeron unas fuertes nevadas que dificultaron la conducción. 
# Elimina estas tres observaciones y calcula la media y la desviación típica de las restantes 39 observaciones.

ejercicio_1_75 %>%
  arrange(Valores)

excluir <- ejercicio_1_75$Valores > 9.16

prom_dias <- mean(ejercicio_1_75$Valores)
des_dias <- sd(ejercicio_1_75$Valores)

prom_dias_cor <- mean(ejercicio_1_75$Valores[!excluir])
des_dias_cor <- sd(ejercicio_1_75$Valores[!excluir])


resultados_dias <- data.frame(
  Medidas = c("Media", "Desviación típica"),
  Resultados_con_obs = c(prom_dias,des_dias),
  Resultados_sin_obs = c(prom_dias_cor,des_dias_cor)
)

print(resultados_dias)

"
            Medidas Resultados_con_obs Resultados_sin_obs
1             Media          8.4004762          8.3007692
2 Desviación típica          0.6233347          0.5129398
"
# (d) Haz un recuento del número de observaciones entre ¯x−s y ¯x+s, entre ¯x−2s y ¯x+2s, y finalmente entre ¯x−3s y ¯x+3s. 
# Halla el porcentaje de observaciones en cada uno de los intervalos anteriores. Compara estos porcentajes con los que 
# les correspondería de acuerdo con la regla del 68-95-99,7.

media_tiempos <- mean(ejercicio_1_75$Valores)
des_tiempos <- sd(ejercicio_1_75$Valores)

una_des_positiva <- media_tiempos + des_tiempos
una_des_negativa <- media_tiempos - des_tiempos

dos_des_positiva <- media_tiempos + (2 * des_tiempos)
dos_des_negativa <- media_tiempos - (2 * des_tiempos)

tres_des_positiva <- media_tiempos + (3 * des_tiempos)
tres_des_negativa <- media_tiempos - (3 * des_tiempos)

resultados_des_tiempos <- data.frame(
  Regla = c("68%","95%","99.7%"),
  Cola_Positiva = c(una_des_positiva,dos_des_positiva,tres_des_positiva),
  Cola_Negativa = c(una_des_negativa,dos_des_negativa,tres_des_negativa)
)

print(resultados_des_tiempos)

"
  Regla Cola_Positiva Cola_Negativa
1   68%      9.023811      7.777142
2   95%      9.647146      7.153807
3 99.7%     10.270480      6.530472
"

# **************************************************
# PREGUNTA 1.76 - Rendimiento de las acciones
# **************************************************

# La tabla 1.8 proporciona los rendimientos mensuales de las acciones de Philip Morris para el periodo que va de julio de 1990 
# a mayo de 1997. Los datos se presentan ordenados cronológicamente empezando con −5,7%, el rendimiento de julio de 1990.

# (a) Dibuja un diagrama temporal con estos datos. Este periodo corresponde a una época de movilizaciones crecientes en contra del tabaco. 
# Por tanto, cabe esperar una tendencia decreciente en los rendimientos de las acciones. Sin embargo, también aparece un periodo 
# en el cual el valor de las acciones crece de forma rápida. ¿Qué puede haber provocado esta tendencia creciente? 
# ¿Qué muestra tu diagrama temporal?

ejercicio_1_76 <- read.csv("ejercicio_1_44_rendimientos.csv")

head(ejercicio_1_76)

str(ejercicio_1_76)

class(ejercicio_1_76$Mes)

View(ejercicio_1_76)

fechas <- as.Date(paste0(ejercicio_1_76$Mes, "-01"))

plot(fechas,
     ejercicio_1_76$Rendimiento,
     main = "Rendimientos acciones",
     xlab = "Fecha",
     ylab = "Rendimiento de las acciones(%)",
     type = "o",
     col = "blue",
     lwd = 2,
     xaxt = "n")
axis.Date(1, at = fechas, labels = format(fechas, "%Y-%m"), cex.axis = 0.8)
grid()

"A pesar de la presión antitabaco, la empresa implementó estrategias efectivas que 
generaron valor para los accionistas durante este período específico, demostrando 
resiliencia empresarial."

# **************************************************
# PREGUNTA 1.77 - Nueva variedad de maíz
# **************************************************

# El maíz es un alimento importante para los animales. De todas formas, este alimento carece de algunos aminoácidos que son esenciales. 
# Un grupo de científicos desarrolló una nueva variedad que sí contenía dichos aminoácidos a niveles apreciables. 
# Para comprobar el valor de esta nueva variedad para la alimentación animal se llevó a cabo el siguiente experimento: 
# a un grupo de 20 pollos de un día se les suministró un pienso que contenía harina de maíz de la nueva variedad. 
# A otro grupo de 20 pollos (grupo de control) se le alimentó con un pienso idéntico al anterior, aunque no contenía harina de la 
# variedad mejorada de maíz. Los resultados que se obtuvieron sobre las ganancias de peso de los pollos (en gramos), al cabo de 
# 21 días de alimentación, fueron los siguientes:

# Variedad normal: 380 321 366 356 361 447 401 375 283 349 402 462 434 403 393 426 356 410 329 399
# Variedad mejorada: 406 318 467 407 350 384 316 272 427 420 477 392 345 455 360 431 430 339 410 326

# (a) Calcula los cinco números resumen correspondientes a la ganancia de peso de los dos grupos de pollos. 
# Para comparar las dos distribuciones, representa los dos diagramas de caja en un mismo gráfico. 
# ¿Qué se puede deducir de estos diagramas de caja?

ejercicio_1_77 <- read.csv("ejercicio_1_77_maiz.csv")

head(ejercicio_1_77)

str(ejercicio_1_77)

class(ejercicio_1_77)

View(ejercicio_1_77)

summary(ejercicio_1_77$Normal)
summary(ejercicio_1_77$Mejorada)

boxplot_datos_maiz <- list(
  Normal = c(283.0,356.0,386.5,382.6,404.8,462.0),
  Mejorada = c(272.0,343.5,399.0,386.6,427.8,477.0)
)

boxplot(boxplot_datos_maiz,
        main = "Comparación maíz",
        ylab = "Ganancia en gramos",
        col = c("lightblue", "lightgreen"),
        border = c("blue", "darkgreen"))

"
Normal:
Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
---------------------------------------------------
283.0   356.0    386.5     382.6   404.8      462.0 


Mejorada:
Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
---------------------------------------------------
272.0   343.5    399.0     386.6   427.8      477.0 
"

# (b) En el trabajo original donde aparecieron los datos, los autores calcularon las medias y las desviaciones típicas de cada grupo de pollos. 
# ¿Cuáles son sus valores? ¿Qué diferencia hay entre las medias de cada grupo?

media_normal <- mean(ejercicio_1_77$Normal)
media_mejorada <- mean(ejercicio_1_77$Mejorada)

des_normal <- sd(ejercicio_1_77$Normal)
des_mejorada <- sd(ejercicio_1_77$Mejorada)

comparativa <- data.frame(
  Tipo = c("Normal", "Mejorada"),
  Media = c(media_normal,media_mejorada),
  Desviación = c(des_normal,des_mejorada)
)

print(comparativa)

" Tipo     Media    Desviación
-------------------------------
1 Normal   382.65   44.23297
2 Mejorada 386.60   56.11680
"
# **************************************************
# PREGUNTA 1.78 - Alfredo Di Stefano
# **************************************************

# Antes de ir a España y fichar por el Real Madrid en la temporada 1952/53 y posteriormente por el Real Club Deportivo Español de 
# Barcelona en la temporada 1964/65, Alfredo Di Stefano jugó en varios equipos suramericanos: River Plate de Buenos Aires, 
# Huracán de Buenos Aires y Millonarios de Bogotá. 

# Mientras jugó en Suramérica el número de goles por temporada en la liga fue:
# Temporada 1944/45: 0 goles
# Temporada 1945/46: 11 goles
# Temporada 1946/47: 27 goles
# Temporada 1947/48: 14 goles
# Temporada 1948/49: 24 goles
# Temporada 1949/50: 23 goles
# Temporada 1950/51: 32 goles
# Temporada 1951/52: 19 goles

# Mientras jugó en España el número de goles por temporada en la liga fue:
# Temporada 1953/54: 28 goles
# Temporada 1954/55: 25 goles
# Temporada 1955/56: 24 goles
# Temporada 1956/57: 31 goles
# Temporada 1957/58: 19 goles
# Temporada 1958/59: 23 goles
# Temporada 1959/60: 12 goles
# Temporada 1960/61: 21 goles
# Temporada 1961/62: 10 goles
# Temporada 1962/63: 12 goles
# Temporada 1963/64: 11 goles
# Temporada 1964/65: 7 goles
# Temporada 1965/66: 4 goles

Di_Stefano_america <- data.frame(
  Temporada = c("1944/45", "1945/46","1946/47","1947/48","1948/49","1949/50","1950/51","1951/52"),
  Goles = c(0,11,27,14,24,23,32,19)
)

Di_Stefano_europa <- data.frame(
  Temporada = c("1953/54","1954/55","1955/56","1956/57","1957/58","1958/59","1959/60","1960/61",
                "1961/62","1962/63","1963/64","1964/65","1965/66"),
  Goles = c(28,25,24,31,19,23,12,21,10,12,11,7,4)
)

write.csv(Di_Stefano_america,"ejercicio_1_78_goles_america.csv")
write.csv(Di_Stefano_europa,"ejercicio_1_78_goles_europa.csv")

# (a) Calcula los cinco números resumen correspondientes al tiempo que Di Stefano jugó en Suramérica y al tiempo que jugó en España.

summary(Di_Stefano_america$Goles)
summary(Di_Stefano_europa$Goles)

"
En América:
Min.   1st Qu.  Median    Mean    3rd Qu.    Max.
----------------------------------------------------
0.00   13.25    21.00     18.75   24.75      32.00 

En España:
Min.   1st Qu.  Median    Mean    3rd Qu.    Max.
----------------------------------------------------
4.00   11.00    19.00     17.46   24.00      31.00 
"

# (b) Sitúa los dos diagramas de caja en un mismo gráfico y compara las dos distribuciones.

boxplot_datos_goles <- list(
  América = c(0.00,13.25,21.00,18.75,24.75,32.00),
  España = c(4.00, 11.00, 19.00, 17.46,24.00,31.00)
)

boxplot(boxplot_datos_goles,
        main = "Comparativa goles de DiStefano",
        ylab = "Cantidad de goles",
        col = c("lightblue","lightgreen"),
        border = c("blue","darkgreen"))

# **************************************************
# PREGUNTA 1.79 - Los todoterrenos, ¿desperdician combustible?
# **************************************************

# La tabla 1.2 da los consumos, en litros a los cien kilómetros, de 26 modelos de coches de tamaño medio de 1998. 
# Aquí presentamos los consumos de 19 modelos de todoterreno de ese año:

# Modelo              Consumo (litros/100km)
# Acura SLX           12,5
# Chevrolet Blazer    11,8
# Chevrolet Tahoe     12,5
# Dodge Durango       13,9
# Ford Expedition     13,1
# Ford Explorer       12,5
# Honda Passport      11,8
# Infiniti QX4        12,5
# Isuzu Trooper       12,5
# Jeep Grand Cherokee 13,1
# Jeep Wrangler       12,5
# Land Rover          14,8
# Mazda MPV           12,5
# Mercedes-Benz ML320 11,3
# Mitsubishi Montero  11,8
# Nissan Pathfinder   12,5
# Suzuki Sidekick      9,1
# Toyota RAV4          9,1
# Toyota 4Runner      10,8

ejercicio_1_79 <- read.csv("ejercicio_1_79_todoterrenos.csv")

head(ejercicio_1_79)

str(ejercicio_1_79)

class(ejercicio_1_79)

View(ejercicio_1_79)

# (a) Describe gráfica y numéricamente los consumos en carretera de los 4×4.
# ¿Cuáles son las principales características de esta distribución?

# 1. PARA VER POR MODELO

ejercicio_1_79_ordenado <- ejercicio_1_79[order(ejercicio_1_79$Consumo_litros_100km), ]

barplot(ejercicio_1_79_ordenado$Consumo_litros_100km,
        names.arg = ejercicio_1_79_ordenado$Modelo,
        main = "Consumo de combustible por modelo (1998)",
        xlab = "Litros por 100 km",
        ylab = "",
        horiz = TRUE,   
        las = 1,        
        col = "lightblue",
        xlim = c(0, 16))

# 2. PARA VER POR DISTRIBUCIÓN

hist(ejercicio_1_79$Consumo_litros_100km,
          main = "Distribución del consumo en todoterrenos",
          xlab = "Litros por 100 km",
          ylab = "Cantidad de modelos",
          col = "lightblue",
          border = "black")

# 3. PARA VER RESUMEN NUMÉRICO

summary(ejercicio_1_79$Consumo_litros_100km)

resumen_numerico <- summary(ejercicio_1_79$Consumo_litros_100km)

boxplot(resumen_numerico,
        main = "Resumen numérico del conjunto",
        ylab = "Consumo litros por 100 km",
        col = "lightblue",
        border = "blue")
"
Min.   1st Qu.  Median    Mean    3rd Qu.    Max.
---------------------------------------------------
9.10   11.80    12.50     12.14   12.50      14.80 

Los todoterrenos muestran consumos variables, con algunos modelos relativamente eficientes 
pero la mayoría en el rango de 12-13 L/100km, significativamente mayor que los coches medianos típicos."

# (b) Dibuja diagramas de caja para comparar la distribución de los automóviles medianos con la de los 4×4. 
# ¿Cuáles son las principales diferencias entre estas dos distribuciones?

ejercicio_1_5 <- read.csv("ejercicio_1_5_autos.csv")

head(ejercicio_1_5)
head(ejercicio_1_79)

str(ejercicio_1_5)
str(ejercicio_1_79)


modelos_4x4 <- c("Acura SLX", "Chevrolet Blazer", "Chevrolet Tahoe", "Dodge Durango",
                 "Ford Expedition", "Ford Explorer", "Honda Passport", "Infiniti QX4",
                 "Isuzu Trooper", "Jeep Grand Cherokee", "Jeep Wrangler", "Land Rover",
                 "Mazda MPV", "Mercedes-Benz ML320", "Mitsubishi Montero", 
                 "Nissan Pathfinder", "Suzuki Sidekick", "Toyota RAV4", "Toyota 4Runner")

autos_medianos_reales <- ejercicio_1_5[!ejercicio_1_5$Vehiculo %in% modelos_4x4, ]

consumo_medianos <- autos_medianos_reales$Consumo..l.100km.
consumo_4x4 <- ejercicio_1_79$Consumo_litros_100km

summary(consumo_medianos)
summary(consumo_4x4)

boxplot(consumo_medianos, consumo_4x4,
        names = c("Autos medianos", "4x4"),
        main = "Comparación de consumo: Autos medianos vs 4x4 (1998)",
        ylab = "Litros por 100 km",
        xlab = "Tipo de vehículo",
        col = c("lightblue", "lightgreen"),
        border = "darkblue")

"
Vehículos medianos:
Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
----------------------------------------------------
7.20    8.25     9.10      9.10    9.50       14.80 

Vehículos 4x4:
Min.   1st Qu.  Median    Mean    3rd Qu.    Max.
---------------------------------------------------
9.10   11.80    12.50     12.14   12.50      14.80 

Los 4x4 consumen significativamente más combustible que los autos medianos. Mientras 
los autos medianos tienen una mediana de 9.10 L/100km y un consumo típico entre 8.25-9.50 L, 
los 4x4 muestran una mediana de 12.50 L/100km con un rango intercuartílico de 11.80-12.50 L. 
Esta diferencia de aproximadamente 3.4 L/100km representa un 37% más de consumo en los 4x4, 
confirmando que estos vehículos son considerablemente menos eficientes en el uso de combustible."

# **************************************************
# PREGUNTA 1.80 - Supervivencia de conejillos de Indias
# **************************************************

# En la tabla 1.9 se presentan los tiempos de supervivencia, en días, de 72 conejillos de Indias después de que se les 
# inyectara el bacilo de la tuberculosis en un experimento médico. La distribución de los tiempos de supervivencia, 
# ya sea de máquinas (sobrecargadas), ya sea de personas enfermas (por ejemplo, personas que están bajo tratamiento oncológico), 
# se suele caracterizar por ser asimétricas hacia la derecha.

# Tiempos de supervivencia (días):
# 43 45 53 56 56 57 58 66 67 73
# 74 79 80 80 81 81 81 82 83 83
# 84 88 89 91 91 92 92 97 99 99
# 100 100 101 102 102 102 103 104 107 108
# 109 113 114 118 121 123 126 128 137 138
# 139 144 145 147 156 162 174 178 179 184
# 191 198 211 214 243 249 329 380 403 511
# 522 598

# (a) Representa gráficamente estos datos y describe sus características más destacables. 
# La distribución, ¿es asimétrica hacia la derecha?

ejercicio_1_80 <- read.csv("ejercicio_1_80_conejillos.csv")

head(ejercicio_1_80)

str(ejercicio_1_80)

class(ejercicio_1_80)

View(ejercicio_1_80)


stem(ejercicio_1_80$Dias_supervivencia,scale = 2)

hist(ejercicio_1_80$Dias_supervivencia,
     main = "Distribución de tiempos de supervivencia",
     xlab = "Días de supervivencia",
     ylab = "Cantidad de conejillos",
     col = "lightblue",
     breaks = 15,  
     border = "white")

"Tenemos un grafico asimetrico a la derecha, lo que significa que muchos conejillos no llegan a los 150 días, 
asimismo una dispersión que va desde los 40 días hasta los 598 días, siendo estos ultimos tomados como observaciones 
atipicas, por lo tanto merecen un exhaustivo estudio y vemos que su centro se ubica entre los 120 - 140 días
Esto sugiere variabilidad individual en la respuesta a la tuberculosis que merece mayor investigación"

# (b) He aquí los resultados del programa estadístico Data Desk correspondientes a estos datos:

#     Summary statistics for dias
#     Mean: 141.84722
#     Median: 102.50000
#     Cases: 72
#     StdDev: 109.20863
#     Min: 43
#     Max: 598
#     25th%ile: 82.250000
#     75th%ile: 153.75000

# Explica cómo la relación entre la media y la mediana refleja la asimetría de los datos.

"La media (141.8 días) es aproximadamente 39 días mayor que la mediana (102.5 días), lo que confirma visualmente 
la asimetría hacia la derecha observada en el histograma. Esta diferencia se debe a la presencia de valores excepcionalmente 
altos en los tiempos de supervivencia, que elevan el promedio pero no afectan la mediana, característica típica de 
distribuciones con cola larga hacia la derecha."

# (c) Calcula los cinco números resumen y explica brevemente cómo se puede detectar la asimetría de los datos a partir de ellos.

summary(ejercicio_1_80$Dias_supervivencia)

boxplot(summary(ejercicio_1_80$Dias_supervivencia),
        main = "Resumen estadistico",
        ylab = "Días de supervivencia")

"
Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
------------------------------------------------------
43.00   82.75    102.50    141.85  149.25     598.00 

Los cinco números resumen revelan asimetría hacia la derecha: la mayor distancia entre la mediana y el 
tercer quartil (46.75 días) comparada con la distancia al primer quartil (19.75 días), junto con la posición 
descentrada de la mediana hacia valores bajos y la presencia de valores extremos solo en el extremo superior, 
confirman la distribución asimétrica observada visualmente."

# **************************************************
# PREGUNTA 1.81 - Acciones calientes
# **************************************************

# La tasa de rendimiento de una acción se deriva de la variación de su precio y de los dividendos pagados, y normalmente se expresa 
# como un porcentaje respecto a su valor inicial. A continuación se presentan datos sobre las tasas de rendimiento mensuales de las 
# acciones de los almacenes Wal-Mart desde el año 1973 hasta el año 1991. Tenemos un total de 228 observaciones.La figura 1.28 muestra 
# los resultados de un programa estadístico que describe la distribución de estos datos. Fíjate en que el tallo está constituido por
# las decenas de los porcentajes. Las hojas están constituidas por las unidades. El diagrama de tallos divide los tallos para que la 
# representación sea mejor. El programa proporciona las observaciones atípicas mayores y las menores de formaseparada. 
# No las incluye en el diagrama de tallos.

# (a) Calcula los cinco números resumen de estos datos.
 
figura_1_28 <- image_read("figura_1_28.png")

print(figura_1_28)

resumen <- data.frame(
  Medidas = c("Mínimo", "Primer cuartil","Mediana","Tercer cuartil","Máximo"),
  Resultados = c(-34.04255,-2.950258,3.4691, 8.4511,58.67769)
)

print(resumen)

"
         Medidas Resultados
1         Mínimo -34.042550
2 Primer cuartil  -2.950258
3        Mediana   3.469100
4 Tercer cuartil   8.451100
5         Máximo  58.677690
"

# (b) Describe las principales características de la distribución.

"Razonablemente simétrica, con algunas observaciones atípicas a derecha
 e izquierda, sin embargo no es particularmente asimétrica."

# (c) Si tuvieras 1.000 dólares en acciones de Wal-Mart al inicio del mejor mes de los 19 años considerados, 
# ¿cuánto dinero habrías ganado al final del mes? Si tuvieras 1.000 dólares en acciones al comienzo del peor mes, 
# ¿cuánto valdría tu dinero al final de dicho mes?

inversion_inicial <- 1000

capital_final_mejor_mes <- inversion_inicial * (1 + 58.677690/100)
capital_final_peor_mes <- inversion_inicial * (1 + (-34.042550)/100)

resultados_acciones <- data.frame(
  Mes = c("Mejor mes", "Peor mes"),
  Resultado = c(capital_final_mejor_mes,capital_final_peor_mes)
)

print(resultados_acciones)

"
        Mes Resultado
1 Mejor mes 1586.7769
2  Peor mes  659.5745
"
# **************************************************
# PREGUNTA 1.82 - El criterio 1,5 × RI
# **************************************************

# Un criterio que puedes utilizar para detectar observaciones atípicas de un conjunto de datos es el siguiente:
# 1. Halla los cuartiles Q1 y Q3 y el recorrido intercuartílico RI = Q3 − Q1.
# 2. Califica como atípica una observación si se sitúa más a la izquierda de 1,5 × RI desde el primer cuartil o más a la derecha 
#    de 1,5 × RI a partir del tercer cuartil.

# (a) Halla el recorrido intercuartílico RI correspondiente a los datos del ejercicio anterior.

q1 <- - 2.950258

q3 <- 8.451100

ri <- q3 - q1

limite_inferior <- q1 - ri * 1.5
limite_superior <- q3 + ri * 1.5

resultados_ri <- data.frame(
  Medida = c("Primer cuartil (Q1)", "Tercer cuartil (Q3)", 
             "Recorrido intercuartílico (RI)", 
             "Límite inferior para outliers", 
             "Límite superior para outliers"),
  Valor = c(q1, q3, ri, limite_inferior, limite_superior)
)

print(resultados_ri)

"
                          Medida      Valor
1            Primer cuartil (Q1)  -2.950258
2            Tercer cuartil (Q3)   8.451100
3 Recorrido intercuartílico (RI)  11.401358
4  Límite inferior para outliers -20.052295
5  Límite superior para outliers  25.553137
"

# (b) De acuerdo con el criterio que acabamos de ver, ¿existe alguna observación atípica?

"Sí existen observaciones atípicas en ambos extremos de la distribución.

Límite inferior: -20.10%

Límite superior: 31.56%

Valores atípicos por la izquierda: -34.04%, -31.25%, -27.06% (menores que -20.10%)

Valores atípicos por la derecha: 32.02%, 41.81%, 42.06%, 57.89%, 58.68% (mayores que 31.56%)
"

# (c) ¿Crees que este criterio es el mismo que utiliza el programa estadístico para seleccionar las observaciones atípicas?

"SÍ, es muy probable que el programa estadístico utilice el mismo criterio o uno muy similar basado en el rango intercuartílico (1.5 × RI).

Evidencia:

Los valores reportados como 'Low' y 'High' en el output del programa coinciden exactamente con los que identificamos como atípicos 
usando el criterio de 1.5 × RI

El valor -26.61290% (del grupo 'Low') NO aparece como atípico en nuestro análisis, y efectivamente NO está listado separadamente por el programa

Todos los valores que superan nuestros límites calculados (-20.10% y 31.56%) son exactamente los mismos que el programa reporta como observaciones extremas"

# **************************************************
# PREGUNTA 1.83 - Rendimiento de acciones
# **************************************************

# ¿Crees que ha cambiado el rendimiento de las acciones de Wal-Mart en los 19 años que van desde 1973 hasta 1991? 
# En el ejercicio 1.81 vimos la distribución de los 228 rendimientos mensuales. Este tipo de descripción no puede responder 
# a preguntas sobre los cambios acaecidos a lo largo del tiempo. La figura 1.29 es un tipo de gráfico temporal. 
# En lugar de representar todas las observaciones, éstas se presentan agrupadas por años en forma dediagramas de caja. 
# Cada año tenemos 12 rendimientos mensuales.

# (a) ¿Se observa alguna tendencia en los rendimientos mensuales típicos a lo largo de estos años?

figura_1_29 <- image_read("figura_1_29.png")

print(figura_1_29)

"Si, se observa una tendencia. Los rendimientos típicos (medianas) muestran una tendencia decreciente a lo largo del tiempo, 
especialmente notable a partir de mediados de los años 80, donde los rendimientos medios se vuelven menos consistentes y 
en general más bajos."

# (b) ¿Se observa alguna tendencia en la dispersión anual de los datos?

"Si, se observa un cambio en la dispersión. La variabilidad de los rendimientos aumenta considerablemente con el tiempo:

Los primeros años (1973-1980) muestran diagramas de caja más compactos

A partir de 1981, la dispersión aumenta notablemente

Los años posteriores a 1985 muestran una variabilidad mucho mayor"

# (c) El diagrama de tallos de la figura 1.28 señala algunas observaciones atípicas. 
# ¿Cuáles de éstas se pueden detectar en los diagramas de caja? ¿En qué años ocurren? 
# ¿Refuerza esto las conclusiones que has obtenido en el apartado (b)?
# ¿Hay alguna observación atípica especialmente sorprendente después de tener en cuenta tu respuesta en (b)?

"Las observaciones atípicas se detectan claramente en los diagramas de caja como puntos individuales fuera de los bigotes

Años con outliers: Se observan valores atípicos principalmente en años de alta volatilidad (1987, 1990, 1991)

Refuerza las conclusiones: Sí, confirma que la dispersión aumenta con el tiempo, especialmente en años posteriores

Observación especialmente sorprendente: El outlier extremadamente bajo de 1973 (-34%) es particularmente notable dado que ocurrió en un período de relativa estabilidad"

# **************************************************
# PREGUNTA 1.84 - Edad de presidentes de EE UU
# **************************************************

# Julia dice: "La gente ahora vive más años que antes, por tanto, es probable que los nuevos presidentes de EE UU sean mayores 
# que los anteriores cuando acceden a la Casa Blanca". Juan responde: "No, a los votantes de ahora les gusta la juventud y no 
# respetan a la gente mayor, por tanto, es probable que los presidentes de EE UU sean más jóvenes que hace unos años".

# (a) Dibuja un gráfico temporal con la edad de los presidentes de EE UU que tienes en la tabla 1.7. 
# En el eje de las abscisas sitúalos desde el primero, que corresponde a Washington, hasta el que ocupa el lugar número 42, 
# que corresponde a Clinton.

ejercicio_1_84 <- read.csv("ejercicio_1_32_presidentes.csv")

head(ejercicio_1_84)

str(ejercicio_1_84)

nombres_presidentes <- 1:length(ejercicio_1_84$Presidente)

plot(nombres_presidentes,
     ejercicio_1_84$Edad,
     main = "Edades de los presidentes",
     xlab = "Presidentes",
     ylab = "Edades",
     type = "o",
     xaxt = "n")
axis(1, at = nombres_presidentes, labels = ejercicio_1_84$Presidente, las = 2,cex.axis = 0.7)
grid()
# (b) ¿Se observa alguna tendencia a lo largo del tiempo?

"Al parecer la edad de los presidentes en EE.UU se ha rejuvencido, aunque algunos de los casos pueden ser considerados
como OA. Lo cierto es que hasta J.Q Adams la variación en la edad era mínima"

# (c) ¿A quién da la razón los datos, a Julia o a Juan?

"Le doy la razón a Juan, pero debemos considrar que las edades mas extremas pueden considerarse como OA, haciendo que la dispersión
sea mayor que en los primeros tiempos del EEUU republicano."

# **************************************************
# PREGUNTA 1.85 - Coste de la capacidad de los ordenadores
# **************************************************

# Los usuarios de informática saben que el coste de la potencia de los ordenadores ha ido disminuyendo de forma muy rápida. 
# Por ejemplo, el coste de la capacidad, en megabytes, de los mayores discos duros del mercado de los ordenadores personales 
# para Macintosh es:

# Año     Coste (€)
# 1992    5,07
# 1993    2,40
# 1994    1,14
# 1995    0,53
# 1996    0,36

# Estos costes se han ajustado de acuerdo con la inflación de cada año para facilitar su comparación.

# (a) Dibuja un diagrama temporal con estos datos.

coste_ordenadores <- data.frame(
  Annio = c(1992,1993,1994,1995,1996),
  Coste = c(5.07,2.40,1.14,0.53,0.36)
)

print(coste_ordenadores)

write.csv(coste_ordenadores,"ejercicio_1_85_computadores")

plot(coste_ordenadores$Annio,
     coste_ordenadores$Coste,
     main = "Coste de la capacidad de los ordenadores",
     xlab = "Año",
     ylab = "Costo (en euros)",
     type = "b",
     col = "red",
     pch = 16,
     lwd = 2)
grid(col = "gray80", lty = 2)

# (b) Señala si observas alguna tendencia.

"Existe un rendencia a la baja, lo que afirma la hipotesis de que el costo se ha abaratado
aun cuando se hayan hehcos ajuste a la inflación de cada año."

# **************************************************
# PREGUNTA 1.86 - Grandes robles y pequeñas bellotas
# **************************************************

# De las 50 especies de roble de los EE UU, 28 crecen en la costa atlántica y 11 en la costa de California. 
# Estamos interesados en la distribución del tamaño de las bellotas de los robles. He aquí datos sobre el volumen de bellotas 
# (en centímetros cúbicos) de estas 39 especies de roble:

# Atlántico: 1.4, 3.4, 9.1, 1.6, 10.5, 2.5, 0.9, 6.8, 1.8, 0.3, 0.9, 0.8, 2.0, 
#            1.1, 0.6, 1.8, 4.8, 1.1, 3.0, 1.1, 1.1, 3.6, 8.1, 3.6, 1.8, 0.4, 
#            1.1, 1.2
# California: 4.1, 5.9, 17.1,1.6, 2.6, 0.4,2.0,6.0,7.1,5.5, 1.0

# (a) Dibuja un histograma con los 39 volúmenes de bellota. Describe la distribución. Incluye un resumen numérico adecuado.

bellotas_atlantico <- c(1.4, 3.4, 9.1, 1.6, 10.5, 2.5, 0.9, 6.8, 1.8, 0.3, 0.9, 0.8, 2.0, 
                        1.1, 0.6, 1.8, 4.8, 1.1, 3.0, 1.1, 1.1, 3.6, 8.1, 3.6, 1.8, 0.4, 
                        1.1, 1.2)

bellotas_california <- c(4.1, 5.9, 17.1, 1.6, 2.6, 0.4, 2.0, 6.0, 7.1, 5.5, 1.0)

bellotas_todas <- c(bellotas_atlantico, bellotas_california)

print(bellotas_todas)

write.csv(bellotas_todas,"ejercicio_1_86_bellotas.csv")

hist(bellotas_todas,
     main = "Distribución del volumen de bellotas (39 especies)",
     xlab = "Volumen (cm³)",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "darkgreen",
     breaks = 10,  
     xlim = c(0, 18))

summary(bellotas_todas)

boxplot(bellotas_todas,
        main = "Boxplot del volumen de bellotas",
        ylab = "Volumen (cm³)",
        col = "lightblue",
        horizontal = TRUE)

"
TOTAL GENERAL:

Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
-----------------------------------------------------
0.300   1.100    1.800     3.326   4.450      17.100 
"

# (b) Compara las distribuciones de las regiones atlántica y californiana con un gráfico y con resúmenes numéricos. 
# ¿Qué has hallado?

par(mfrow = c(1, 2))

hist(bellotas_atlantico,
     main = "Bellotas - Costa Atlántico",
     xlab = "Volumen (cm³)",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "darkblue",
     xlim = c(0, 18),
     ylim = c(0, 15))

hist(bellotas_california,
     main = "Bellotas - California",
     xlab = "Volumen (cm³)",
     ylab = "Frecuencia",
     col = "lightcoral",
     border = "darkred",
     xlim = c(0, 18),
     ylim = c(0, 15))

par(mfrow = c(1, 1))

summary(bellotas_atlantico)
summary(bellotas_california)

boxplot(bellotas_atlantico,
        bellotas_california,
        names = c("Bellotas California", "Bellotas Atlántico"),
        main = "Distribución del volumen de bellotas",
        ylab = "Volumen (cm³)",
        col = c("lightgreen","orange"),
        border = c("darkgreen","darkorange"))
"
Bellotas Atlántico:

Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
-----------------------------------------------------
0.300   1.100    1.700     2.729   3.450      10.500 

Bellotas Pacífico:
   
Min.    1st Qu.  Median    Mean    3rd Qu.    Max.
----------------------------------------------------
0.400   1.800    4.100     4.845   5.950      17.100

Las bellotas de California son significativamente más grandes y variables que las 
del Atlántico. La media en California (4.85 cm³) casi duplica la del Atlántico (2.73 cm³), 
y presenta mayor dispersión con valores extremos más pronunciados. Esta diferencia 
sugiere adaptaciones ecológicas distintas entre las especies de roble de ambas costas, 
posiblemente influenciadas por factores ambientales o evolutivos regionales."

# **************************************************
# PREGUNTA 1.87 - Datos sobre Estados europeos
# **************************************************

# La tabla 1.6 hace referencia a los Estados europeos. Existe mucha más información. 
# Entra en la página web de la Comisión Europea o acércate a la biblioteca de la Comisión Europea que tengas más próxima 
# y busca más datos estadísticos sobre los Estados europeos.

# (a) ¿Qué porcentaje representa la población ocupada en agricultura en cada Estado?
# (b) Compara la inflación de los distintos Estados europeos. Representa gráficamente la información que hayas encontrado. 
#     Calcula los resúmenes numéricos más adecuados. ¿Cuáles son tus conclusiones?

# **************************************************
# PREGUNTA 1.88 - Adopción de la cultura anglosajona
# **************************************************

# La prueba ARSMA (Acculturation Rating Scale for Mexican Americans) es una prueba psicológica que se utiliza para determinar 
# el nivel de integración cultural de los estadounidenses de origen mexicano. Las puntuaciones posibles van de 1,0 a 5,0. 
# Los valores más altos de esta escala corresponden a niveles más elevados de adaptación a la cultura anglosajona. 
# Cuando se efectuó esta prueba con una población experimental, se observó que la distribución de las puntuaciones era 
# aproximadamente normal con una media de 3,0 y una desviación típica de 0,8. 
# Un investigador cree que los mexicanos recién llegados a EE UU tienen una puntuación media próxima a 1,7 y que la de la 
# siguiente generación está próxima a 2,1.

# (a) ¿Qué proporción de la población experimental tiene puntuaciones menores de 1,7?

media <- 3.0
desviacion <- 0.8

pnorm(1.7,media,desviacion)*100

"Un 5.2% tiene una puntuación menor a 1.7 puntos"

# (b) ¿Qué proporción tiene puntuaciones entre 1,7 y 2,1?

(pnorm(2.1,media,desviacion) - pnorm(1.7,media,desviacion))*100

"Un 7.8% tiene un puntuación entre 1.7 hasta 2.1 puntos"

"Esto confirma la hipótesis del investigador: tanto recién llegados como segunda 
generación tienen puntuaciones significativamente inferiores a la media poblacional, 
mostrando menor adaptación a la cultura anglosajona."

# **************************************************
# PREGUNTA 1.89 - Perímetro craneal de soldados
# **************************************************

# Según datos del ejército de EE UU, la distribución del perímetro craneal entre sus soldados es aproximadamente normal con 
# una media de 57,9 cm y una desviación típica de 2,8 cm. Los cascos militares se producen de forma industrial excepto para 
# los soldados con perímetros craneales situados en el 5% superior o bien en el 5% inferior, para los cuales se hacen a medida.

# (a) ¿Para qué perímetros craneales se hacen estos cascos a medida?

media <- 57.9
desviacion <- 2.8

percentil_5 <- qnorm(0.05, media, desviacion)
percentil_95 <- qnorm(0.95, media, desviacion)

resultados <- data.frame(
  Medida = c("5% superior","5% inferior"),
  Resultados = c(percentil_95,percentil_5)
)
print(resultados)

"
       Medida Resultados
1 5% superior   62.50559
2 5% inferior   53.29441

Los perímetros craneales estan situados entre 53 cms y 63 cms."

# **************************************************
# PREGUNTA 1.90 - Adopción de la cultura anglosajona
# **************************************************

# El ejercicio 1.88 describió la prueba ARSMA.

# (a) ¿Cuál debe ser el resultado de un estadounidense de origen mexicano para pertenecer al 30% de la población experimental 
#     que obtuvo mejores resultados en la prueba?

media <- 3.0
desviacion <- 0.8

qnorm(0.7,media,desviacion)

"Debe tener una puntuación de 3.4 puntos para pertenecer al 30% de la población experimental"

# (b) ¿Qué resultados definen el 30% para los cuales la cultura mexicano-española tiene un mayor peso?

qnorm(0.3,media,desviacion)

"Debe tener 2.5 puntos puntos aprox. los que definen ese 30% con mayor peso de la cultura mexicano-española."
# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN REPASO ::::::::::::::::::::::::::::::::::::::::::::::::::::
