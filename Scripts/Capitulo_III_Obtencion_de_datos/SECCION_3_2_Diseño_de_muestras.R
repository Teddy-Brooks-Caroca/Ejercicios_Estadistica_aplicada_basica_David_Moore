# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# CAPÍTULO III "Obtención de datos"

# SECCIÓN 3.2 "Diseño de muestras"

# **************************************************
# PREGUNTA 3.4 - Muestreo de mujeres trabajadoras
# **************************************************

# Una socióloga quiere conocer la opinión de mujeres trabajadoras adultas sobre 
# una subvención a guarderías infantiles por parte del Estado. La socióloga 
# obtiene una lista de 520 miembros de una asociación de mujeres empresarias y 
# profesionales, y les envía un cuestionario a 100 de ellas seleccionadas al azar. 
# Sólo se reciben 48 cuestionarios contestados.

# (a) ¿Cuál es la población de este estudio?

"La población son las 520 miembros de una asociación de mujeres empresarias y 
profesionales."

# (b) ¿Cuál es la muestra?

"La muestra son las 100 mujeres seleccionadas al azar."

# (c) ¿Cuál es la proporción (porcentaje) de no-respuestas?

poblacion <- 520

muestra <- 100

respondieron <- 48

no_respondieron <- muestra - respondieron

prc_respondieron <- (respondieron / muestra) * 100

prc_no_respondieron <- (no_respondieron / muestra) * 100

prc_estudio <- data.frame(
  muestra_poblacional = c("Respondieron","No respondieron"),
  porcentajes = c(paste0(round(prc_respondieron,2),"%"),
                  paste0(round(prc_no_respondieron,2),"%"))
)

print(prc_estudio)

"
  muestra_poblacional porcentajes
1        Respondieron         48%
2     No respondieron         52%
"

# **************************************************
# PREGUNTA 3.5 - Identifica la población
# **************************************************

# En cada uno de los siguientes ejemplos de muestreo, identifica a la población 
# tan exactamente como sea posible. ¿Qué individuos forman parte de la población? 
# Si falta información, completa la descripción de la población de manera razonable.

# (a) Cada semana el Instituto Gallup interroga a una muestra de alrededor de
# 1.500 adultos residentes en EE UU con la finalidad de conocer la opinión nacional
# sobre una amplia variedad de temas.

"La población corresponde a los residentes adultos en EE.UU"

# (b) Cada 10 años, el censo intenta obtener información básica de todos los hogares del país. 
# Además, a una muestra de aproximadamente el 17% de los hogares, se les envió un "cuestionario 
# ampliado" en el que se solicitaba mucha información adicional.

"La población corresponde a todos los hogares del país"

# (c) Un fabricante de maquinaria adquiere reguladores de voltaje de un proveedor. Existen 
# informes de que la variación en el voltaje de salida de los reguladores está afectando al 
# funcionamiento de determinadas máquinas. Para evaluar la calidad de los reguladores de voltaje 
# suministrados por el proveedor, el fabricante envía una muestra de 5 reguladores de la última 
# entrega a un laboratorio para que los estudien.

"La población corresponde a los reguladores del proveedor (si consideramos que solo tienen un
proveedor)"

# **************************************************
# PREGUNTA 3.6 - Cartas dirigidas al Congreso
# **************************************************

# Formas parte del equipo de una diputada que está preparando una ley que
# proporcionaría asistencia gratuita de enfermeras en casa. Le dices a la
# diputada que se han recibido 1.128 cartas sobre el tema, de las cuales 871
# se oponen a la ley. "Me sorprende que la mayoría de los ciudadanos
# se opongan a esta ley. Pensaba que sería bastante popular", dice la diputada.

# (a) ¿Estás convencido de que la mayoría de los votantes se oponen a la ley?

"No podemos concluir que la mayoría de los votantes se oponga a la ley. Motivos principales:

- Respuesta voluntaria (voluntary response bias): las cartas son enviadas por quien quiere hacerlo; 
las opiniones extremas suelen sobre-representarse.

- Muestra no aleatoria: no hay evidencia de que las cartas provengan de una muestra representativa de 
los votantes (ni por demografía, ni por geografía, ni por edad, etc.).

- Posibles fuentes de sesgo adicionales: campañas organizadas, múltiples cartas de la misma persona, 
cartas de no-residentes, o cartas no verificadas.

Conclusión: los 871/1.128 reflejan la opinión de quienes escribieron, no la opinión de los votantes 
en general."

# (b) ¿Cómo justificarías desde un punto de vista estadístico estos resultados?

muestra_cartas <- 1128

oponen <- 871

aceptan <- muestra_cartas - oponen

prc_oponen <- (oponen / muestra_cartas) * 100

prc_aceptan <- (aceptan / muestra_cartas) * 100

resultados_muestrales <- data.frame(
  Cartas = c("Aceptan","Se oponen"),
  Porcentajes = c(paste0(round(prc_aceptan,2),"%"),
                  paste0(round(prc_oponen,2),"%"))
)

print(resultados_muestrales)

"
     Cartas Porcentajes
1   Aceptan      22.78%
2 Se oponen      77.22%

===========================================

Hemos recibido 1.128 cartas; 871 (77,2%) expresan oposición. Sin embargo, estas 
cartas provienen de quien voluntariamente decidió escribir, por lo que no constituyen 
una muestra representativa del electorado. Para saber la opinión del conjunto de votantes 
hacen falta métodos probabilísticos (encuesta representativa).”
"

# **************************************************
# PREGUNTA 3.7 - Selección de ejecutivos
# **************************************************

# Una empresa quiere conocer la opinión de sus ejecutivos extranjeros sobre el
# sistema de valoración de su rendimiento. A continuación encontrarás una lista
# con todos los ejecutivos extranjeros de la empresa. Utiliza la tabla B; sitúate en la
# fila 139 con el fin de escoger a 6 ejecutivos que serán entrevistados para conocer
# su opinión sobre el sistema de valoración de su rendimiento en la empresa.

# Lista de ejecutivos:
# Agarwal      Anderson    Baxter      Bowman      Brown
# Castillo     Cross       Dewald      Fernandez   Fleming
# Gates        Goel        Gomez       Hernandez   Huang
# Kim          Liao        Mourning    Naber       Peters
# Pliego       Puri        Richards    Rodriguez   Santiago
# Shen         Vega        Wang

ejecutivos <- c("Agarwal", "Anderson", "Baxter", "Bowman", "Brown",
                "Castillo", "Cross", "Dewald", "Fernandez", "Fleming",
                "Gates", "Goel", "Gomez", "Hernandez", "Huang",
                "Kim", "Liao", "Mourning", "Naber", "Peters",
                "Pliego", "Puri", "Richards", "Rodriguez", "Santiago",
                "Shen", "Vega", "Wang")


fila_139 <- "5558899404707084109843563569344839451719"

fila_140 <- "1297513258130484514472321819400036002428"


pares_139 <- c("55", "58", "89", "94", "04", "70", "70", "84", "10", "98", "43", "56", "35" , "69" , "34" , "48" , "39" , "45" , "17" , "19" )
pares_140 <- c("12", "97", "51", "32", "58", "13", "04", "84", "51", "44" , "72" , "32" , "18" , "19" , "40" , "00" , "36" , "00" , "24" , "28")

numeros <- as.numeric(pares)
numeros_140 <- as.numeric(pares_140)

numeros_validos <- numeros[numeros >= 1 & numeros <= 28]

numeros_validos_140 <- numeros_140[numeros_140 >= 1 & numeros_140 <= 28]
numeros_validos_140 <- numeros_validos_140[!numeros_validos_140 %in% numeros_validos]


todos_numeros <- c(numeros_validos, numeros_validos_140)
ejecutivos_finales <- ejecutivos[todos_numeros[1:6]]

ejecutivos_seleccionados <- data.frame(
  Numero = todos_numeros[1:6],
  Ejecutivo = ejecutivos_finales
)

print(ejecutivos_seleccionados)

"
  Numero Ejecutivo
1      4    Bowman
2     10   Fleming
3     12      Goel
4     13     Gomez
5     18  Mourning
6     19     Naber
"

# **************************************************
# PREGUNTA 3.8 - Selección de alumnos
# **************************************************

# No estáis contentos con la forma como se enseña la matemática financiera
# y queréis quejaros a la decana de la Facultad. La clase decide elegir al azar a
# 4 alumnos para que presenten la queja. A continuación encontrarás una lista de
# la clase. Escoge una muestra aleatoria simple de 4 alumnos utilizando la tabla
# de dígitos aleatorios y comenzando en la línea 145.

# Lista de alumnos:
# Amador      Arrabal     Belisario   Botella     Buruaga
# Castillo    Doreste     Estruch     González    Guerrero
# Guerrero T. Gutiérrez   Herrero     Hortensia   Jarabo
# Jover       Larrea      Leonardo    López       Olea
# Parra       Pichón      Ramones     Rato        Rodríguez
# Romero      Sosa        Toribio     Trueba      Velasco

alumnos <- c("Amador", "Arrabal", "Belisario", "Botella", "Buruaga",
             "Castillo", "Doreste", "Estruch", "González", "Guerrero",
             "Guerrero T.", "Gutiérrez", "Herrero", "Hortensia", "Jarabo",
             "Jover", "Larrea", "Leonardo", "López", "Olea",
             "Parra", "Pichón", "Ramones", "Rato", "Rodríguez",
             "Romero", "Sosa", "Toribio", "Trueba", "Velasco")

linea_145 <- "19687 12633 57857 95806 09931 02150 43163 58636"

pares_145 <- c("19","68","71","26","33","57","85","79","58","06","09","93","10","21","50","43","16","35","86","36")

numeros_145 <- as.numeric(pares_145)

numeros_validos_145 <- numeros_145[numeros_145 >= 1 & numeros_145 <= 30]

estudiantes_finales <- alumnos[1:4]

alumnoss_seleccionados <- data.frame(
  Numero = numeros_validos_145[1:4],
  Estudiante = estudiantes_finales
)

print(alumnoss_seleccionados)

"
  Numero Estudiante
1     19     Amador
2     26    Arrabal
3      6  Belisario
4      9    Botella
"

# **************************************************
# PREGUNTA 3.9 - Selección de tiendas
# **************************************************

# Debes escoger una muestra aleatoria simple de 10 de las 440 tiendas que venden
# los productos de tu empresa. ¿Cómo denominarías a esta población? Utiliza
# la tabla B, comenzando en la línea 105, para escoger tu muestra.

linea_105 <- "95592 94007 69971 91481 60779 53791 17297 59335"
linea_106 <- "68417 35013 15529 72765 85089 57067 50211 47487"
linea_107 <- "82739 57890 20807 47511 81676 55300 94383 14893"

num_3_105 <- c("955","929","400","769","971","914","816","077","953","791","172","975","933")
num_3_106 <- c("684","173","501","315","529","727","658","508","957","067","502","114","748")
num_3_107 <- c("827","395","789","020","807","475","118","167","655","300","943","831","489")

numeros_105 <- as.numeric(num_3_105)
numeros_106 <- as.numeric(num_3_106)
numeros_107 <- as.numeric(num_3_107)

numeros_validos_105 <- numeros_105[numeros_105 >= 1 & numeros_105 <= 440]
numeros_validos_106 <- numeros_106[numeros_106 >= 1 & numeros_106 <= 440]
numeros_validos_106 <- numeros_validos_106[!numeros_validos_106 %in% numeros_validos_105]
numeros_validos_107 <- numeros_107[numeros_107 >= 1 & numeros_107 <= 440]
numeros_validos_107 <- numeros_validos_107[!numeros_validos_107 %in% c(numeros_validos_105,numeros_validos_106)]

todos_numeros <- c(numeros_validos_105, numeros_validos_106,numeros_validos_107)

tiendas_finales <- unique(todos_numeros)[1:10]

cat("Las empresas seleccionadas son: ",tiendas_finales)

"Las empresas seleccionadas son:  400 77 172 173 315 67 114 395 20 118"

# **************************************************
# PREGUNTA 3.10 - Selección de asociación universitaria
# **************************************************

# Treinta alumnos y diez profesores de una universidad pertenecen a una
# asociación universitaria. 

# Los alumnos son:
# Abel        Flores      Cordón      Cuevas      García
# Huidobro    Melendres   Rodríguez   Jiménez     Gutiérrez
# Jordana     David       Homero      Lamas       Domènech
# Hoz         Elias       Huertas     Lerma       López
# Miranda     Nevia       Otero       Perales     Santos
# Suárez      Telias      Torla       Portabella  Varga

# Los profesores son:
# Artero      Estapé      Lezama      Moravia     Satorra
# Borrell     García      Lightman    Pericales   Yang

# La asociación puede enviar a 4 alumnos y 2 profesores a una convención, y se
# decide escoger al azar a los que irán. Utiliza la tabla B para escoger una muestra
# aleatoria estratificada de esos 4 alumnos y 2 profesores.

alumnos <- c("Abel","Flores","Cordón","Cuevas","García",
             "Huidobro","Melendres","Rodríguez","Jiménez","Gutiérrez",
             "Jordana","David","Homero","Lamas","Domènech",
             "Hoz","Elias","Huertas","Lerma","López",
             "Miranda","Nevia","Otero","Perales","Santos",
             "Suárez","Telias","Torla","Portabella","Varga")

profesores <- c("Artero","Estapé","Lezama","Moravia","Satorra",
                "Borrell","García","Lightman","Pericales","Yang")

tabla_b <- read.csv("Tabla_B_digitos_aleatorios.csv", 
                    colClasses = c("numeric", "character"))

str(tabla_b)

#PARA ALUMNOS
linea_valida_101 <- tabla_b$Bloques[tabla_b$Linea == 101]

pares <- substring(linea_valida_101, 
                   seq(1, nchar(linea_valida_101), 2),
                   seq(2, nchar(linea_valida_101), 2))

numeros <- as.numeric(pares)

numeros_validos_alumnos <- numeros[numeros >= 1 & numeros <= 30]

alumnos_seleccionados <- alumnos[unique(numeros_validos_alumnos)[1:4]]

print(alumnos_seleccionados)

#PARA PROFESORES
linea_103 <- tabla_b$Bloques[tabla_b$Linea == 103]

pares_103 <- substring(linea_103, 
                       seq(1, nchar(linea_103), 2),
                       seq(2, nchar(linea_103), 2))

numeros_profesores <- as.numeric(pares_103)

numeros_validos_profesores <- numeros_profesores[numeros_profesores >= 1 & numeros_profesores <= 10]

profesores_seleccionados <- profesores[unique(numeros_validos_profesores)[1:2]]

print(profesores_seleccionados)

delegacion <- data.frame(
  Tipo = c(rep("Alumno", 4), rep("Profesor", 2)),
  Apellido = c(alumnos_seleccionados, profesores_seleccionados)
)

print(delegacion)

"
      Tipo  Apellido
1   Alumno     Lerma
2   Alumno     Nevia
3   Alumno    García
4   Alumno     Torla
5 Profesor Pericales
6 Profesor   Borrell
"

# **************************************************
# PREGUNTA 3.11 - Auditoría
# **************************************************

# Los auditores suelen utilizar muestras estratificadas para examinar los archivos 
# de las empresas, por ejemplo, para comprobar las facturas pendientes. 
# La estratificación se basa en el importe en euros de las facturas y el
# muestreo incluye frecuentemente el 100% de las facturas con importes más altos.

# Una empresa presenta un informe con 5.000 facturas pendientes. De ellas, 
# 100 son de importes superiores a 50.000 €, 500 son de importes entre 1.000 
# y 50.000 € y las restantes 4.400 son de importes inferiores a los 1.000 €. 

# Utilizando estos grupos como estratos, decides examinar todas las facturas 
# de importes más altos, muestrear el 5% de las facturas de importes medios 
# y el 1% de las facturas de importes pequeños.

# (a) ¿Cómo denominarías los dos estratos en los que efectuarás el muestreo?

facturas_tramo_medio <- (500 * 5) / 100

facturas_tramo_bajo <- (4400 * 1 ) / 100

cantidad_auditoria <- data.frame(
  Tipo_tramo = c("Tramo medio","Tramo bajo"),
  Cantidad = c(facturas_tramo_medio,facturas_tramo_bajo)
)

print(cantidad_auditoria)

"
   Tipo_tramo Cantidad
1 Tramo medio       25
2  Tramo bajo       44

===========================

Estrato alto no requiere muestreo, porque se va a revisar en su totalidad
"

# (b) Utiliza la tabla B, comenzando en la línea 115, para seleccionar sólo
# las primeras 5 facturas de cada uno de estos estratos.

linea_115 <- tabla_b$Bloques[tabla_b$Linea == 115]

linea_116 <- tabla_b$Bloques[tabla_b$Linea == 116]

#FACTURAS TRAMO MEDIO
tripletes_115 <- substring(linea_115,
                           seq(1, nchar(linea_115), 3),
                           seq(3, nchar(linea_115), 3))

numeros_estrato_medio <- as.numeric(tripletes_115)

facturas_medias <- numeros_estrato_medio[numeros_estrato_medio >= 1 & numeros_estrato_medio <= 500]

facturas_medias <- unique(facturas_medias)[1:5]

#FACTURAS TRAMO BAJO
cuatro_115 <- substring(linea_115,
                           seq(1, nchar(linea_115), 4),
                           seq(4, nchar(linea_115), 4))

cuatro_116 <- substring(linea_116,
                        seq(1, nchar(linea_116), 4),
                        seq(4, nchar(linea_116), 4))

numeros_estrato_bajo <- as.numeric(cuatro_115)

numeros_extra_bajo <- as.numeric(cuatro_116)

facturas_bajas <- numeros_estrato_bajo[numeros_estrato_bajo >= 1 & numeros_estrato_bajo <= 4400]

numeros_validos_extra <- numeros_extra_bajo[numeros_extra_bajo >= 1 & numeros_extra_bajo <= 4400]

todos_bajos <- unique(c(facturas_bajas[!is.na(facturas_bajas)], numeros_validos_extra))[1:5]


resultados_auditoria <- data.frame(
  Estrato = c(rep("Medio (1.000-50.000 €)", 5), 
              rep("Bajo (<1.000 €)", 5)),
  Numero_Factura = c(facturas_medias, todos_bajos)
)

print(resultados_auditoria)

"
                  Estrato Numero_Factura
1  Medio (1.000-50.000 €)            417
2  Medio (1.000-50.000 €)            494
3  Medio (1.000-50.000 €)            322
4  Medio (1.000-50.000 €)            247
5  Medio (1.000-50.000 €)             97
6         Bajo (<1.000 €)           1776
7         Bajo (<1.000 €)           3222
8         Bajo (<1.000 €)           2631
9         Bajo (<1.000 €)           2592
10        Bajo (<1.000 €)           1445
"




