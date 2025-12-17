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


pares_139 <- c("55","58","89","94","04","70","70","84","10","98","43","56","35","69","34","48","39","45","17","19")
pares_140 <- c("12","97","51","32","58","13","04","84","51","44","72","32","18","19","40","00","36","00","24","28")

numeros_139 <- as.numeric(pares_139)
numeros_140 <- as.numeric(pares_140)

numeros_validos_139 <- numeros_139[numeros_139 >= 1 & numeros_139 <= 28]

numeros_validos_140 <- numeros_140[numeros_140 >= 1 & numeros_140 <= 28]
numeros_validos_140 <- numeros_validos_140[!numeros_validos_140 %in% numeros_validos_139]


todos_numeros <- c(numeros_validos_139, numeros_validos_140)
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
3     17      Liao
4     19     Naber
5     12      Goel
6     13     Gomez
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

# **************************************************
# PREGUNTA 3.12 - ¿Qué quieren los escolares?
# **************************************************

# ¿Cuáles son las inquietudes de los escolares? Las niñas y los niños, ¿tienen 
# inquietudes distintas? En áreas urbanas, suburbanas y rurales, ¿las inquietudes 
# son distintas? Para conocer las respuestas, unos investigadores preguntaron a 
# niñas y niños de quinto y sexto:

# ¿Qué es lo que te gusta más de la escuela?
# A. Sacar buenas notas.
# B. Los deportes
# C. Ser el líder de la clase

# Como la mayoría de niños viven en áreas urbanas muy pobladas o áreas
# suburbanas, una muestra aleatoria simple debe incluir pocos niños de zonas 
# rurales. Es más, es demasiado caro escoger al azar niños de una región muy extensa
# —es mejor empezar escogiendo escuelas que niños—.

# Describe un diseño adecuado para este estudio y explica las razones que 
# te han llevado al mismo.

"Se propone un diseño por conglomerados donde las escuelas son las unidades de muestreo inicial. 
Primero se estratificarían las escuelas según su ubicación geográfica (urbana, suburbana y rural). 
Luego, dentro de cada estrato, se seleccionarían aleatoriamante varias escuelas (conglomerados). 
Este método es eficiente en costes, ya que concentra el trabajo de campo en lugares específicos 
en lugar de tener que desplazarse a hogares dispersos de niños individuales por una región extensa.

Para garantizar representatividad, se usaría una afijación proporcional para decidir cuántas escuelas 
seleccionar de cada zona. Es decir, se seleccionarían más escuelas de zonas urbanas (donde hay más niños) 
y menos de las rurales. Dentro de cada escuela seleccionada, se encuestaría a todos los alumnos de quinto 
y sexto grado, o a un grupo aleatorio de ellos, registrando también su género.

Este diseño permite responder a las preguntas de investigación de manera válida. Al estratificar por zona, 
se pueden comparar directamente las inquietudes entre áreas urbanas, suburbanas y rurales. Al muestrear dentro 
de las escuelas y registrar el género, también se pueden analizar las posibles diferencias entre niñas y niños. 
La aleatorización en cada etapa ayuda a que los resultados sean generalizables."

# **************************************************
# PREGUNTA 3.13 - Muestras a partir de números telefónicos
# **************************************************

# El listado de individuos a partir del cual se selecciona una muestra se llama 
# marco de muestreo. Idealmente, este marco debería incluir a todos los individuos 
# de la población, pero en la práctica esto suele ser difícil. Un marco que deje 
# fuera parte de la población es una fuente habitual de falta de cobertura.

# (a) Supón que se selecciona aleatoriamente una muestra de hogares en una
# población a partir del listín telefónico. ¿Qué hogares se omiten en este marco?
# ¿Qué tipo de personas viven en estos hogares? Estas personas probablemente
# estarán insuficientemente representadas en la muestra.

"Se omiten todos los hogares que no cuentan con teléfono fijo, así como aquellos que solo 
utilizan teléfono móvil. También quedan fuera los hogares que han pedido no aparecer en 
el listín, los que cambiaron de número y aún no están actualizados, y los hogares nuevos 
que aún no han sido incorporados.

Las personas que suelen vivir en estos hogares son jóvenes, hogares de bajos ingresos, 
migrantes, arrendatarios recientes y personas con alta movilidad residencial. Por lo tanto, 
estos grupos quedan insuficientemente representados en la muestra."

# (b) En encuestas telefónicas es habitual utilizar aparatos que marcan aleato-
# riamente los cuatro últimos dígitos de un número de teléfono después de haber
# marcado el número del código territorial (los tres primeros dígitos). Utilizando
# este tipo de aparatos, ¿qué hogares de los que mencionaste en tu respuesta en (a)
# se incluirán en el marco de muestreo?

"Este método permite incluir hogares que no aparecen en el listín, como aquellos que tienen teléfono 
fijo pero han pedido reserva, los que han cambiado recientemente de número o los que aún no figuran 
en el directorio. En general, todos los hogares con una línea fija activa en la red pueden ser seleccionados, 
independientemente de si están en el listín o no.

Sin embargo, sigue excluyendo a los hogares sin teléfono fijo o que solo utilizan teléfono móvil."

# **************************************************
# PREGUNTA 3.14 - No contesta nadie
# **************************************************

# Una forma habitual de no-respuesta en encuestas telefónicas es la "llamada 
# sin respuesta". Es decir, se llama a un número de teléfono pero no contesta 
# nadie. El Instituto Nacional de Estadística de Italia examinó las no-respuestas 
# en una encuesta gubernamental con hogares italianos durante los periodos que 
# van del 1 de enero a Semana Santa y del 1 de julio al 31 de agosto.

# Todas las llamadas se efectuaron entre las 7 y las 10 de la noche. En el primer
# periodo, no se contestaron el 21,4% de las llamadas, mientras que en el segundo
# no se contestaron el 41,5% de las llamadas.

# (a) ¿Qué periodo crees que tuvo una proporción más elevada de no-respuestas?
# ¿Por qué?

"El segundo periodo (1 de julio a 31 de agosto) presenta una proporción más alta de no-respuestas 
porque coincide con los meses de verano en el hemisferio norte, en los que muchas familias se encuentran 
de vacaciones, pasan más tiempo fuera del hogar y modifican sus rutinas. Esto reduce la probabilidad de 
que estén en casa entre las 19:00 y las 22:00 para atender la llamada, aumentando el porcentaje de llamadas 
sin respuesta."

# (b) Explica por qué un índice elevado de no-respuestas hace que los resultados 
# de una muestra sean menos fiables.

"Un índice elevado de no-respuestas disminuye la fiabilidad porque las personas que no contestan 
suelen diferir sistemáticamente de las que sí responden. Esto introduce sesgo por no-respuesta, 
ya que ciertos grupos quedan subrepresentados.

Además, al reducirse el número efectivo de observaciones, aumenta la variabilidad de las estimaciones 
y se pierde precisión. En conjunto, los resultados se vuelven menos representativos y menos confiables 
para describir a toda la población."

# **************************************************
# PREGUNTA 3.15 - Contribución a campañas electorales
# **************************************************

# Aquí tienes dos enunciados de la misma pregunta:

# A. ¿Deberían aprobarse leyes para eliminar toda posibilidad de que determi-
#    nados grupos de presión donasen enormes cantidades de dinero a los can-
#    didatos a la Presidencia?

# B. ¿Deberían aprobarse leyes para prohibir que los grupos de presión contri-
#    buyesen a campañas electorales, o por el contrario, tienen estos grupos de
#    presión el derecho a contribuir a campañas de los candidatos que apoyan?

# Una de estas preguntas consiguió el 40% a favor de prohibir las contribucio-
# nes de los grupos de presión a campañas electorales. La otra consiguió el 80% a
# favor de esta opinión.

# (a) ¿Qué pregunta consiguió el 40% y cuál consiguió el 80%?

"
La pregunta A obtuvo el 80% ya que está formulada de manera más cargada emocionalmente, 
mientras que la pregunta B presenta la prohibición como una posible restricción de derechos.
"
# (b) Explica por qué los resultados fueron tan distintos.

"Los resultados difieren porque las preguntas están formuladas de manera que activan marcos 
mentales distintos.

La pregunta A utiliza lenguaje emocional y negativo (“enormes cantidades”, “eliminar toda posibilidad”), 
lo que predispone al encuestado a ver las donaciones como algo abusivo y, por tanto, a apoyar su prohibición.

En cambio, la pregunta B contrapone explícitamente la prohibición a la idea de un derecho (“tienen estos grupos 
el derecho a contribuir”), lo cual enmarca la misma acción como una cuestión de libertades. Este marco disminuye 
sustancialmente el apoyo a prohibir la contribución.

En resumen, la diferencia se debe al efecto de la formulación: el modo en que se plantea una pregunta puede 
orientar la respuesta incluso cuando trata el mismo tema."

# **************************************************
# PREGUNTA 3.16 - Pregunta a más gente
# **************************************************

# Justo antes de unas elecciones generales, una empresa de encuestas de opinión 
# aumenta el tamaño de su muestra semanal desde el tamaño habitual de 1.500 
# personas hasta 4.000 personas.

# ¿Por qué crees que la empresa hace esto?

"La empresa aumenta el tamaño de la muestra para reducir el margen de error y obtener estimaciones más precisas 
justo antes de las elecciones, cuando incluso pequeñas diferencias pueden ser relevantes para predecir el resultado.

En un contexto electoral cercano a la fecha de votación, las encuestas necesitan ser especialmente exactas y sensibles 
a variaciones mínimas, por lo que ampliar la muestra mejora la fiabilidad de las predicciones."

# **************************************************
# PREGUNTA 3.17 - Viviendas públicas y estabilidad familiar
# **************************************************

# Para estudiar el efecto de vivir en viviendas públicas sobre la estabilidad
# familiar de hogares con muy pocos ingresos, unos investigadores obtuvieron una
# lista de todos los solicitantes de vivienda pública durante un determinado año.
# Algunas solicitudes fueron aceptadas, mientras que otras no. Los investigadores
# entrevistaron a todos los solicitantes de cada grupo y compararon los resultados.

# (a) ¿Estamos ante un estudio observacional o un experimento? Justifica tu respuesta.

"Es un estudio observacional, porque los investigadores no asignan de manera aleatoria quién recibe vivienda pública. 
La asignación (aceptado o rechazado) ya ocurrió por los criterios del programa de vivienda, no por intervención experimental. 
Los investigadores solo observan y comparan los grupos tal como se formaron."

# (b) ¿Cuál es la variable explicativa?

"La variable explicativa es si el solicitante recibió vivienda pública o no (aceptado vs. rechazado)"

# (c) ¿Cuál es la variable respuesta?

"La variable respuesta es la estabilidad familiar de los hogares (medida según los criterios del estudio)."

# **************************************************
# PREGUNTA 3.18 - Longitud de palabras en novelas
# **************************************************

# A veces, los distintos estilos de redacción pueden distinguirse por la lon-
# gitud de las palabras utilizadas. Una persona interesada en este hecho quiere
# estudiar la longitud de las palabras utilizadas en las novelas de Camilo José Ce-
# la. Para ello, abre al azar una de sus novelas y toma nota de la longitud de las
# primeras 250 palabras de la página.

# (a) ¿Cuál es la población en este estudio?

"La población está formada por todas las palabras de todas las novelas de Camilo José Cela."

# (b) ¿Cuál es la muestra?

"La muestra está formada por las 250 primeras palabras de la página elegida al azar de una novela de Cela."

# (c) ¿Qué variable se ha medido?

"La variable medida es la longitud de cada palabra, normalmente expresada en número de letras."

# **************************************************
# PREGUNTA 3.19 - Identifica la población
# **************************************************

# En cada uno de los siguientes ejemplos de muestreo, identifica la población 
# tan exactamente como puedas. ¿Qué individuos forman la población? Si la 
# información que se da está incompleta, completa la descripción de la población 
# de una manera razonable.

# (a) Una investigadora quiere saber qué factores afectan a la supervivencia
# y al éxito de pequeñas empresas. La investigadora selecciona una muestra de
# 150 pequeñas empresas del sector "bares y restaurantes" del listado de las 
# Páginas Amarillas de la guía telefónica de una gran ciudad.

"
Individuo: una pequeña empresa del sector bares y restaurantes.
Población: todas las pequeñas empresas del sector bares y restaurantes de la gran 
ciudad incluidas en las Páginas Amarillas.
"

# (b) Un diputado quiere saber si los electores apoyan una propuesta legislativa
# sobre sanidad. Su equipo le informa de que se han recibido 228 cartas sobre el
# tema, de las cuales 193 se oponen a la nueva ley.

"
Individuo: una persona que escribió una carta sobre la propuesta legislativa.
Población: todas las personas que enviaron cartas al diputado expresando su opinión 
sobre esa propuesta (no todos los electores).
"

# (c) Una compañía de seguros quiere averiguar la calidad de sus servicios con
# relación a las reclamaciones de sus asegurados con pólizas de automóvil. Cada
# mes la compañía selecciona una muestra aleatoria simple de todas las reclamacio-
# nes relacionadas con sus seguros de automóvil, con el fin de evaluar la precisión
# y rapidez de los trámites efectuados.

"
Individuo: una reclamación presentada por un asegurado con póliza de automóvil.
Población: todas las reclamaciones vinculadas a pólizas de automóvil presentadas a la compañía 
(en el periodo que se desea evaluar, típicamente el mes correspondiente).
"

# **************************************************
# PREGUNTA 3.20 - La muestra de Ann Landers
# **************************************************

# En una ocasión la columnista Ann Landers preguntó a sus lectoras si les 
# gustaría tener el cariño de un hombre pero sin sexo. Respondieron más de 
# 90.000 mujeres, de las cuales el 72% respondieron afirmativamente.

# Muchas de las cartas que recibió esta periodista comentaban el desagradable 
# trato que recibían por parte de los hombres.

# (a) Explica por qué esta muestra está sesgada.

"La muestra está sesgada porque es una muestra voluntaria: solo respondieron las lectoras que 
sintieron la motivación de escribir una carta.

Este tipo de muestras tiende a atraer especialmente a personas con experiencias o emociones fuertes 
relacionadas con el tema. En este caso, muchas mujeres que respondieron mencionaron un trato desagradable 
de los hombres, lo cual las predispone a ver favorablemente la idea de “cariño sin sexo”.

Por lo tanto, las mujeres que respondieron no representan al conjunto de la población femenina, sino a un 
grupo particular con más probabilidad de tener experiencias negativas y, por ende, opiniones más extremas."

# (b) ¿En qué dirección se produce el sesgo? Es decir, ¿este 72% es mayor
# o menor que la verdadera proporción poblacional?

"El 72% es mayor que la verdadera proporción poblacional.

Dado que respondieron principalmente mujeres con experiencias negativas con el trato de los hombres, es 
más probable que ellas apoyen la idea presentada, lo que hace que el resultado final esté inflado respecto 
a la realidad del conjunto de mujeres."

# **************************************************
# PREGUNTA 3.21 - Encuestas basadas en llamadas de televidentes
# **************************************************

# Un conocido programa deportivo de un canal de televisión español planteó 
# la siguiente pregunta a los telespectadores: ¿se proporciona demasiada 
# información relacionada con el fútbol en España? 

# A continuación el presentador del programa dijo:
# "Si tu respuesta es afirmativa llama al 91 452 17 00 y si tu respuesta es 
# negativa llama al 91 452 17 01. Recuerda que el coste de la llamada es de 
# medio euro el primer minuto."

# Explica por qué esta encuesta de opinión casi seguro que está sesgada.

"La encuesta está sesgada porque depende de llamadas voluntarias, lo que hace que solo respondan personas con 
opiniones fuertes sobre el tema (sesgo de autoselección). Además, el costo de la llamada introduce sesgo de cobertura, 
ya que excluye a quienes no pueden o no quieren pagar por participar. También influye que el público del programa deportivo 
no representa a la población general. Todo esto provoca que los resultados no reflejen la verdadera opinión del conjunto de 
telespectadores."

# **************************************************
# PREGUNTA 3.22 - Conocimiento del presidente del Parlamento Europeo
# **************************************************

# Un artículo periodístico sobre el conocimiento de los ciudadanos europeos 
# de las instituciones de la Unión Europea afirma que el 87% de los europeos 
# no conoce el nombre del presidente del Parlamento Europeo. 

# Al final del artículo, se puede leer: "La encuesta se basa en 1.210 entrevistas 
# telefónicas realizadas a adultos de todos los países europeos".

# (a) ¿Qué variable mide esta encuesta?

"La variable medida es si la persona conoce o no el nombre del presidente del Parlamento Europeo"

# (b) ¿Cuál es la población sobre la que se quiere información?

"La población está formada por los adultos de los países miembros de la Unión Europea."

# (c) ¿Cuál es la muestra?

"La muestra son las 1.210 personas adultas entrevistadas telefónicamente en países de la Unión Europea."

# (d) El método de muestreo utilizado, ¿está sesgado?

"El método de muestreo puede presentar sesgo de cobertura, ya que solo incluye personas con acceso telefónico 
y disponibles para responder llamadas. Sin embargo, la exclusión de personas no europeas que viven en Europa no 
constituye un sesgo, porque la población de interés declarada son los adultos europeos. El tamaño de la muestra 
afecta a la precisión, pero no introduce sesgo por sí mismo."

# **************************************************
# PREGUNTA 3.23 - Opinión sobre la policía de Miami
# **************************************************

# El Departamento de Policía de Miami quiere saber cuál es la opinión que 
# tienen los residentes de Miami de raza negra sobre la policía. Se escoge al 
# azar una muestra de 300 hogares preferentemente de barrios donde predomina 
# la población negra. Posteriormente, un policía negro uniformado visita cada 
# uno de los hogares y entrevista a un adulto de cada uno de ellos.

# (a) ¿Cuál es la población?

"La población esta formada por los residentes de Miami de raza negra"

# (b) ¿Cuál es la muestra?

"La muestra corresponde a los 300 hogares seleccionados, uno en cada uno de los hogares 
seleccionados en barrios con predominio de población negra."

# (c) ¿Por qué los resultados de la encuesta seguramente estarán sesgados?

"Los resultados probablemente estarán sesgados por varias razones. En primer lugar, existe sesgo 
de cobertura, ya que la muestra se selecciona principalmente en barrios con predominio de población 
negra, lo que puede excluir a residentes negros que viven en otros tipos de barrios.

En segundo lugar, existe un importante sesgo de entrevistador, ya que las entrevistas son realizadas 
por un policía uniformado, lo que puede influir en las respuestas y llevar a que los entrevistados oculten 
opiniones negativas por temor o deseo de dar una respuesta socialmente aceptable.

Finalmente, hay sesgo de selección dentro del hogar, al entrevistarse solo a un adulto sin un procedimiento 
claro de selección."

# **************************************************
# PREGUNTA 3.24 - Muestreo de botellas químicas
# **************************************************

# Un fabricante de productos químicos escoge 3 botellas de cada lote de 25
# que contiene un determinado reactivo y comprueba su pureza y potencia. Los
# números de control de las botellas de uno de los lotes son los siguientes:

# A1096 A1097 A1098 A1101 A1108
# A1112 A1113 A1117 A2109 A2211
# A2220 B0986 B1011 B1096 B1101
# B1102 B1103 B1110 B1119 B1137
# B1189 B1223 B1277 B1286 B1299

# Utiliza la fila 111 de la tabla B para escoger una muestra aleatoria simple de
# 3 de esas botellas.

botellas <- c("A1096","A1097","A1098","A1101","A1108",
              "A1112","A1113","A1117","A2109","A2211",
              "A2220","B0986","B1011","B1096","B1101",
              "B1102","B1103","B1110","B1119","B1137",
              "B1189","B1223","B1277","B1286","B1299")

fila_111 <- "8148669487605130929700412712382764939950"

fila_112 <- "5963688804046347119719352730898489845785"

pares_111 <- substring(fila_111,
                       seq(1, nchar(fila_111), 2),
                       seq(2, nchar(fila_111), 2))

pares_112 <- substring(fila_112,
                       seq(1, nchar(fila_112), 2),
                       seq(2, nchar(fila_112), 2))

numeros_111 <- as.numeric(pares_111)

numeros_112 <- as.numeric(pares_112)

numeros_validos_111 <- unique(numeros_111[numeros_111 >= 1 & numeros_111 <= 25])

numeros_validos_112 <- unique(numeros_112[numeros_112 >= 1 & numeros_112 <= 25 & !numeros_112 %in% numeros_validos_111])

todos_numeros <- c(numeros_validos_111, numeros_validos_112)

botellas_finales <- botellas[todos_numeros[1:3]]

botellas_seleccionados <- data.frame(
  Numero = todos_numeros[1:3],
  Muestra = botellas_finales
)

print(botellas_seleccionados)

"
  Numero Muestra
1     12   B0986
2      4   A1101
3     11   A2220
"

# **************************************************
# PREGUNTA 3.25 - Muestreo de barrios
# **************************************************

# La figura 3.2 es un mapa ficticio de una zona del censo. Las zonas del censo 
# son áreas pequeñas y homogéneas con una media de población de 4.000 habitantes. 
# En el mapa, cada barrio está marcado con un número identificativo. 

# Una muestra aleatoria simple de barrios obtenida de una zona del censo es 
# a menudo la penúltima etapa de una muestra en etapas múltiples.

# Utiliza la tabla B comenzando en la fila 125 para escoger una muestra aleatoria
# simple de 5 barrios en esta zona del censo.

install.packages("magick")
library(magick)

figura_3_2 <- image_read("figura_3_2.png")
print(figura_3_2)

barrios <- 101:510

fila_125 <- "9674612149378237186818442351196210339244"
fila_126 <- "9692719931368097419277567887414840941903"


triplete_125 <- substring(
  fila_125,
  seq(1, nchar(fila_125), 3),
  seq(3, nchar(fila_125), 3)
)

triplete_126 <- substring(
  fila_126,
  seq(1, nchar(fila_126), 3),
  seq(3, nchar(fila_126), 3)
)


numeros_125 <- as.numeric(triplete_125)
numeros_126 <- as.numeric(triplete_126)

numeros_validos_125 <- unique(
  numeros_125[numeros_125 >= 101 & numeros_125 <= 510]
)

numeros_validos_126 <- unique(
  numeros_126[numeros_126 >= 101 & numeros_126 <= 510 &
                !numeros_126 %in% numeros_validos_125]
)

todos_numeros <- c(numeros_validos_125, numeros_validos_126)

muestra_barrios <- todos_numeros[!is.na(todos_numeros)][1:5]

cat("Los barrios escogidos son:", muestra_barrios)

"Los barrios escogidos son: 461 214 235 119 271"

# **************************************************
# PREGUNTA 3.26 - Dígitos aleatorios
# **************************************************

# De las siguientes afirmaciones sobre una tabla de dígitos aleatorios, 
# ¿cuáles son ciertas y cuáles son falsas? Justifica brevemente tus respuestas.

# (a) Hay exactamente cuatro ceros en cada fila de 40 dígitos.

"FALSA, En una tabla de dígitos aleatorios, cada dígito (0–9) tiene la misma probabilidad de aparecer, 
pero no hay garantía de que en una fila concreta aparezca un número exacto de ceros. En promedio habría 
4 ceros en 40 dígitos, pero el número real puede variar por azar."

# (b) Cada par de dígitos tiene una probabilidad de 1/100 de ser 00.

"VERDADERA, Si los dígitos son independientes y cada uno tiene probabilidad 1/10, entonces la probabilidad 
de obtener un 0 seguido de otro 0 es: 1/10 x 1/10 = 1/100"

# (c) Los dígitos 0000 nunca pueden aparecer como un grupo, porque este grupo 
# no es aleatorio.

"FALSA, La secuencia 0000 sí puede aparecer en una tabla de dígitos aleatorios. Aunque es poco frecuente, 
sigue siendo un resultado posible y aleatorio. Pensar que no puede aparecer es una confusión común entre 
“aleatorio” y “regular”."

# **************************************************
# PREGUNTA 3.27 - Muestras aleatorias sistemáticas
# **************************************************

# La última etapa de la Encuesta de Población Activa consiste en escoger 
# direcciones dentro de pequeñas áreas llamadas bloques. El método utilizado 
# es el muestreo aleatorio sistemático. Ilustraremos la idea de la muestra 
# aleatoria sistemática con un ejemplo. 

# Supón que hemos de seleccionar 4 direcciones de 100. Como 100/4 = 25, 
# podemos imaginarnos la lista formada por cuatro listas de 25 direcciones. 
# Escoge al azar una de las primeras 25 direcciones utilizando la tabla B. 
# La muestra aleatoria sistemática contiene esta dirección y las situadas en 
# la misma posición en la segunda, la tercera y la cuarta lista. Si la tabla 
# de números aleatorios da 13, por ejemplo, entonces la muestra aleatoria 
# sistemática consiste en las direcciones etiquetadas como 13, 38, 63 y 88.

# (a) Utiliza la tabla B para seleccionar una muestra aleatoria sistemática de
# 5 direcciones de una lista de 200. Entra en la tabla por la línea 120.

fila_120 <- "3547655972394216585004266354354374211937"

pares_120 <- substring(
  fila_120,
  seq(1, nchar(fila_120), 2),
  seq(2, nchar(fila_120), 2)
)

numeros_120 <- as.numeric(pares_120)

m_sistematico <- numeros_120[numeros_120 >= 1 & numeros_120 <= 40][1]

muestra_direcciones <- m_sistematico + 40 * 0:4

cat("Las direcciones escogidas son: ",muestra_direcciones)

"Las direcciones escogidas son:  35 75 115 155 195"

# (b) Al igual que una muestra aleatoria simple, una muestra aleatoria sistemática 
# hace que todos los individuos tengan las mismas posibilidades de ser escogidos. 
# Explica por qué esto es cierto. Luego, explica detalladamente por qué una muestra 
# sistemática no es, sin embargo, una muestra aleatoria simple.

"Una muestra aleatoria sistemática garantiza que todos los individuos tengan la misma probabilidad 
de ser seleccionados porque el punto de inicio se elige al azar. Sin embargo, no es una muestra aleatoria 
simple porque no todas las posibles combinaciones de cinco direcciones pueden formar parte de la muestra, 
ya que los elementos seleccionados están determinados por un intervalo fijo."

# **************************************************
# PREGUNTA 3.28 - Muestreo estratificado en universidad
# **************************************************

# El profesorado de una universidad está constituido por 2.000 hombres y
# 500 mujeres. Una muestra aleatoria estratificada de 50 profesoras y 200 profesores 
# le da a cada profesor (hombre o mujer) una posibilidad entre diez de ser escogido. 
# Este diseño muestral da a todos los individuos de la población las mismas posibilidades 
# de pertenecer a la muestra.

# Esta muestra aleatoria estratificada, ¿es también una muestra aleatoria simple?
# Justifica tu respuesta.

"Aunque todos los individuos de la población tienen la misma probabilidad de ser seleccionados, la muestra 
no es aleatoria simple porque el número de hombres y mujeres está fijado de antemano. En una muestra aleatoria 
simple, todas las muestras posibles de un tamaño dado tienen la misma probabilidad de ser seleccionadas, lo cual 
no ocurre en este diseño estratificado."

# **************************************************
# PREGUNTA 3.29 - Muestreo estratificado con atención a minorías
# **************************************************

# El profesorado de una universidad está constituido por 2.000 hombres y
# 500 mujeres. Una agencia interesada en la igualdad de oportunidades en el traba-
# jo quiere conocer la opinión de los profesores sobre la situación en la universidad.

# Con el fin de prestar suficiente atención a la opinión de las mujeres, la agencia
# decide obtener una muestra aleatoria estratificada compuesta de 200 hombres y
# 200 mujeres. Se dispone de una lista de profesores ordenados alfabéticamente
# y otra de profesoras.

# (a) Explica cómo asignarías etiquetas numéricas y cómo utilizarías una tabla de 
# dígitos aleatorios para escoger la muestra deseada.

"Se asignan etiquetas numéricas independientes a hombres y mujeres. A partir de la tabla de dígitos 
aleatorios se seleccionan números válidos dentro de cada estrato, descartando los que queden fuera del 
rango o se repitan, hasta completar 200 hombres y 200 mujeres.

===========================

Estrato 1: hombres (2.000)

Estrato 2: mujeres (500)

Muestra: 200 hombres + 200 mujeres

"

# (b) Situándote en la fila 122 de la tabla B asigna etiquetas numéricas a las cinco 
# primeras profesoras y a los 5 profesores de la muestra.

linea_122 <- "1387381598950529090873592751868713695761"

linea_123 <- "5458081507271025602755892330634184281868"

#PROFESORAS
tripletes_122 <- substring(
  linea_122,
  seq(1, nchar(linea_122), 3),
  seq(3, nchar(linea_122), 3)
)

numeros_profesoras <- as.numeric(tripletes_122)

profesoras_escogidas <- unique(
  numeros_profesoras[numeros_profesoras >= 1 & numeros_profesoras <= 500]
)[1:5]


#PROFESORES
cuartetos_122 <- substring(
  linea_122,
  seq(1, nchar(linea_122), 4),
  seq(4, nchar(linea_122), 4)
)

cuartetos_123 <- substring(
  linea_123,
  seq(1, nchar(linea_123), 4),
  seq(4, nchar(linea_123), 4)
)

numeros_profesores <- as.numeric(cuartetos_122)

numeros_123 <- as.numeric(cuartetos_123)

profesores_escogidos <- unique(
  numeros_profesores[numeros_profesores >= 1 & numeros_profesores <= 2000]
)[1:5]

nuevos_profesores <- numeros_123[
  numeros_123 >= 1 & numeros_123 <= 2000 &
    !numeros_123 %in% profesores_escogidos
]

profesores_escogidos <- c(
  profesores_escogidos[!is.na(profesores_escogidos)],
  nuevos_profesores
)[1:5]

resultados_profesores <- data.frame(
  Genero = c(rep("Profesora", 5), 
              rep("Profesor", 5)),
  Numero = c(profesoras_escogidas, profesores_escogidos)
)

print(resultados_profesores)

"
      Genero Numero
1  Profesora    138
2  Profesora    159
3  Profesora     52
4  Profesora     87
5  Profesora    359
6   Profesor   1387
7   Profesor    529
8   Profesor    908
9   Profesor   1369
10  Profesor    815
"

# **************************************************
# PREGUNTA 3.30 - Redactado de preguntas
# **************************************************

# Haz un comentario sobre cada una de las siguientes cuestiones como posibles 
# preguntas de una encuesta. ¿Está clara la pregunta? ¿Predispone a una respuesta 
# determinada?

# (a) ¿Cuál de las siguientes afirmaciones representa mejor tu opinión sobre el
# control de los inmigrantes ilegales?
# 1. El Gobierno debería impedir la inmigración ilegal.
# 2. No se puede impedir el derecho de una persona a emigrar de su país.

"La pregunta no está bien formulada. Aunque parece ofrecer dos opciones opuestas, no cubre todo 
el abanico de opiniones posibles. Además:

Las dos afirmaciones no son mutuamente excluyentes: una persona podría creer que el gobierno debe 
regular la inmigración ilegal y, al mismo tiempo, defender el derecho a emigrar.

No permite posiciones intermedias (por ejemplo, regular pero no impedir completamente).

El uso del término “inmigrantes ilegales” puede introducir un sesgo valorativo.

Conclusión: la pregunta no es clara y puede forzar respuestas que no reflejan la opinión real del 
encuestado."

# (b) Se debería favorecer una moratoria de las armas nucleares, ya que de esta
# forma se iniciaría un proceso, muy necesario, para detener su fabricación en todo
# el mundo, lo que reduciría la posibilidad futura de una guerra nuclear. ¿Estás de
# acuerdo o en desacuerdo?

"La pregunta predispone claramente a una respuesta favorable. Antes de pedir la opinión, presenta una 
cadena de argumentos positivos (“muy necesario”, “reduciría la posibilidad futura de una guerra nuclear”), 
lo que constituye una pregunta tendenciosa.

No se presenta ningún argumento alternativo o crítico.

El encuestado puede sentirse presionado a estar de acuerdo para no parecer irracional o irresponsable.

Conclusión: aunque la pregunta es comprensible, no es neutral y puede influir en la respuesta."

# (c) En vista de la incesante degradación medioambiental y del agotamiento
# de los recursos naturales, ¿favorecerías con incentivos económicos el reciclaje de
# los bienes de consumo?

"La pregunta es comprensible, pero no es neutral. El enunciado introduce un contexto alarmista 
(“incesante degradación medioambiental”, “agotamiento de los recursos naturales”) que puede predisponer 
al encuestado a responder afirmativamente.

Mezcla una afirmación fuerte con la pregunta propiamente tal.

No separa el diagnóstico del problema de la política concreta propuesta.

Conclusión: la pregunta es clara, pero está redactada de forma sesgada, ya que introduce juicios previos 
que pueden influir en la respuesta."
