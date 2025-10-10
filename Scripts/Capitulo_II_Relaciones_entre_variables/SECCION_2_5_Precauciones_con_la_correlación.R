# EJERCICIOS "Estadistica básica aplicada" de David S. Moore (2005)

# Capítulo II "Relaciones entre variables"

# SECCIÓN 2.5 "Precauciones con la correlación"

# **************************************************
# PREGUNTA 2.53 - Disminución de la población rural
# **************************************************

# En Estados Unidos la población rural ha ido disminuyendo de forma constante 
# a lo largo de este siglo. He aquí datos sobre esta población (expresado en 
# millones de personas) desde 1935 hasta 1980.

# TABLA DE DATOS:
años <- c(1935, 1940, 1945, 1950, 1955, 1960, 1965, 1970, 1975, 1980)
poblacion_rural <- c(32.1, 30.5, 24.4, 23.0, 19.1, 15.6, 12.4, 9.7, 8.9, 7.2)

# (a) Dibuja un diagrama de dispersión con estos datos. Halla la recta de regre
# sión que exprese la relación entre la población rural en EE UU y el año.

regresion <- lm(poblacion_rural ~ años)

plot(años,
     poblacion_rural,
     main = "Densidad demografica rural en EE.UU (1935 - 1980)",
     xlab = "Años",
     ylab = "Población (en millones de personas",
     pch = 16,
     col = "blue")
abline(a = coef(regresion)[1],b = coef(regresion)[2], lwd = 2, lty = 2,col = "red")
grid()

# (b) De acuerdo con la recta de regresión, ¿cuánto disminuye, como media, la
# población rural cada año durante este periodo? ¿Qué porcentaje de la variación
# observada se explica con la recta de regresión?

summary(regresion)

r <- cor(años,poblacion_rural)

r2 <- r * r

valores <- data.frame(
  Medidas = c("Media de disminución","r2"),
  Valores = c(abs(round(coef(regresion)[2],2)),paste0(round(r2,2)*100,"%"))
)
print(valores)

"
            Medidas  Valores
Media de disminución    0.59
                  r2     98%

(media en millones de personas)
"

# (c) Utiliza la recta de regresión para predecir la población rural en el año 1990.
# ¿Te parece un valor razonable? ¿Por qué?

recta_1990 <- coef(regresion)[1] + coef(regresion)[2] * 1990

print(recta_1990)

cat("Para 1990 la población habrá descendido hasta:",recta_1990)

"Para 1990 la población habrá descendido hasta: -0.7806061

=============================================================

La predicción de -0.78 millones para 1990 no es razonable porque:
  
Imposibilidad física: La población no puede ser negativa

Extrapolación excesiva: 1990 está 10 años fuera del rango de datos (1935-1980)

Cambio de tendencia: Las tasas de disminución probablemente se ralentizaron o estabilizaron después de 1980"

# **************************************************
# PREGUNTA 2.54 - Índices del mercado de valores
# **************************************************

# El índice bursátil Standard & Poor's es la media de los valores de 500 acciones. 
# Existe una correlación moderadamente fuerte (aproximadamente r = 0,6) entre 
# la variación de este índice en enero y su variación en todo el año.

# De todas formas, si nos fijáramos en las variaciones individuales de 500 acciones 
# encontraríamos una correlación bastante distinta.

# (a) ¿Esta correlación sería mayor o menor que la obtenida con las medias?

"La correlación sería MENOR a nivel individual que con las medias."

# (b) ¿Por qué?

"Cuando se promedian 500 acciones en un índice:

Se cancelan las variaciones individuales específicas de cada empresa

Solo queda la tendencia general del mercado

Se reduce el 'ruido' y aumenta la señal común"

# **************************************************
# PREGUNTA 2.55 - Televisión y notas escolares
# **************************************************

# Los niños que pasan muchas horas delante del televisor obtienen, como media, 
# peores notas en la escuela que los niños que pasan menos horas.

# (a) Sugiere variables latentes que puedan afectar a la relación entre
# estas variables debido a que influyen tanto sobre el hecho de pasar muchas horas
# delante de la televisión como sobre las notas escolares.

"La relación entre televisión y malas notas podría ser espuria, explicada por variables 
como el nivel socioeconómico, supervisión parental, peligrosidad en el barrio que limita 
alternativas de ocio, y falta de acceso a actividades educativas extracurriculares."

# **************************************************
# PREGUNTA 2.56 - Educación e ingresos
# **************************************************

# Existe una fuerte correlación positiva entre los años de formación y los ingresos
# de los economistas empleados en empresas. En especial, los economistas doctorados 
# ganan más que los que sólo son licenciados.

# Hay también una fuerte correlación positiva entre los años de formación y los 
# ingresos de los economistas empleados en las universidades. Sin embargo, cuando
# se considera conjuntamente a todos los economistas, existe una correlación nega
# tiva entre la educación y los ingresos.

# La explicación es que las empresas pagan salarios altos y emplean principalmente 
# a economistas que son sólo licenciados, mientras que las universidades pagan 
# salarios bajos y emplean principalmente a economistas con doctorado.

# (a) Haz un diagrama de dispersión con dos tipos de observaciones (de la empresa 
# y de la universidad), para ilustrar cómo se puede tener al mismo tiempo una 
# correlación positiva fuerte dentro de cada grupo y una correlación conjunta negativa.
# (Consejo: empieza estudiando la figura 2.18.)


educacion <- c(4, 5, 6, 4, 5, 6)  # Años de educación
ingresos <- c(40, 45, 50, 80, 85, 90)  # Ingresos en miles
grupo <- c("Universidad", "Universidad", "Universidad", 
           "Empresa", "Empresa", "Empresa")

datos <- data.frame(educacion, ingresos, grupo)

plot(datos$educacion, datos$ingresos, 
     col = ifelse(datos$grupo == "Universidad", "blue", "red"),
     pch = 16, cex = 1.5,
     main = "Paradoja de Simpson: Educación vs Ingresos",
     xlab = "Años de Educación", 
     ylab = "Ingresos (miles $)")
abline(lm(ingresos ~ educacion, data = datos[datos$grupo == "Universidad",]), 
       col = "blue", lwd = 2)
abline(lm(ingresos ~ educacion, data = datos[datos$grupo == "Empresa",]), 
       col = "red", lwd = 2)
abline(lm(ingresos ~ educacion, data = datos), 
       col = "black", lwd = 2, lty = 2)
legend("topleft", 
       legend = c("Universidades", "Empresas", "Global"),
       col = c("blue", "red", "black"),
       lty = c(1, 1, 2), lwd = 2)

"Si dibujáramos el diagrama, veríamos dos nubes de puntos separadas: las universidades 
(alta educación, bajos salarios) en la esquina superior izquierda, y las empresas (menos educación, altos salarios) 
en la esquina inferior derecha. Dentro de cada nube, la relación educación-ingresos es positiva, pero al unir todos 
los puntos, la tendencia general se vuelve negativa."

# **************************************************
# PREGUNTA 2.57 - Los bomberos, ¿causan mayores incendios?
# **************************************************

# Alguien afirma: "Existe una fuerte correlación positiva entre el número de bomberos 
# que actúan en la extinción de un incendio y la importancia del daño que éste ocasiona. 
# Por tanto, el hecho de enviar muchos bomberos sólo ocasiona más daños".

# (a) Explica por qué este razonamiento es incorrecto.

"El razonamiento es incorrecto porque confunde correlación con causalidad. La variable oculta es 
el tamaño del incendio: los incendios más grandes requieren más bomberos y simultáneamente causan más daños. 
Los bomberos no son la causa de los daños, sino que tanto el número de bomberos como la magnitud de los daños 
son consecuencia de la gravedad inicial del incendio. Esta es una correlación espuria donde una tercera variable 
explica la relación observada."

# **************************************************
# PREGUNTA 2.58 - ¿Cómo está tu autoestima?
# **************************************************

# Las personas que tienen éxito tienden a estar satisfechas con ellas mismas. 
# Es posible que ayudar a la gente para que se sienta satisfecha les pueda ayudar 
# a tener más éxito en la escuela y en general en la vida.

# (a) ¿A qué se debe la asociación entre la autoestima y el éxito escolar?

"La asociación entre autoestima y éxito escolar podría deberse a que una autoestima alta motiva un mejor rendimiento académico, 
pero también podría ser que el éxito escolar fortalezca la autoestima, creando un ciclo de refuerzo mutuo."

# (b) ¿Qué podemos decir aparte de que una autoestima alta es la causa de un 
# mejor éxito escolar?

"Además de que la autoestima cause éxito escolar, podría ser que el éxito académico cause mayor autoestima, o que factores 
como el apoyo familiar, recursos económicos y habilidades naturales del estudiante influyan simultáneamente en ambas variables, 
creando una correlación sin relación causal directa."

# **************************************************
# PREGUNTA 2.59 - Los grandes hospitales, ¿son malos?
# **************************************************

# Un estudio muestra que existe una correlación positiva entre el tamaño de un 
# hospital (medido como número de camas x) y el número medio de días y que los 
# enfermos permanecen en él.

# (a) ¿Significa esto que se puede reducir la estancia en un hospital si se escogen 
# hospitales pequeños? ¿Por qué?

"No necesariamente, porque la correlación probablemente refleja que los hospitales grandes atienden 
casos más complejos y pacientes más graves que requieren estancias más prolongadas. La variable de confusión 
es la gravedad del paciente: no es el tamaño del hospital lo que causa estancias largas, sino que los hospitales 
grandes reciben pacientes que de por sí necesitan más días de hospitalización debido a su condición médica."

# **************************************************
# PREGUNTA 2.60 - Para tener éxito en la universidad, ¿hay que estudiar matemáticas?
# **************************************************

# Un estudio con 15.941 estudiantes de secundaria estadounidenses encontró que
# los estudiantes universitarios pertenecientes a minorías raciales que escogieron 
# en secundaria como asignaturas optativas álgebra y geometría se graduaron en la 
# misma proporción que los hijos de anglosajones.

# (a) ¿Qué variables latentes podrían explicar la asociación entre pasar por diver
# sos cursos de matemáticas y el éxito en la universidad?

"Variables latentes como el nivel socioeconómico, calidad de la escuela secundaria, apoyo familiar y motivación personal 
podrían explicar tanto la elección de matemáticas avanzadas como el éxito universitario, creando una correlación espuria."

# (b) Explica por qué exigir haber estudiado álgebra y geometría seguramente 
# tendría poco efecto sobre los estudiantes universitarios que tienen éxito.

"Exigir álgebra y geometría tendría poco efecto porque estas materias son indicadores de estudiantes ya motivados 
y con buen apoyo, no la causa directa del éxito; la correlación refleja características preexistentes más que causalidad."

# **************************************************
# PREGUNTA 2.61 - Comprensión de textos escritos y tamaño del pie
# **************************************************

# Un estudio con niños de 6 a 11 años que asisten a una escuela de primaria halla 
# una fuerte correlación positiva entre el número de calzado x y la nota obtenida 
# en una prueba de comprensión de textos escritos.

# (a) ¿Qué explica esta correlación?

"La correlación se explica perfectamente por la edad: niños mayores tienen pies más grandes y mayor desarrollo cognitivo, 
haciendo que ambas variables aumenten juntas sin relación causal directa."

# **************************************************
# PREGUNTA 2.62 - Los edulcorantes artificiales, ¿provocan un aumento de peso?
# **************************************************

# La gente que utiliza edulcorantes artificiales en vez de azúcar tiende a tener 
# más peso que la gente que toma azúcar.

# (a) ¿Significa esto que los edulcorantes artificiales provocan un aumento de peso?

"No, los edulcorantes no causan aumento de peso; la relación es inversa: personas con sobrepeso usan más 
edulcorantes para reducir calorías."

# (b) Da una explicación más plausible para esta asociación.

"Una explicación más plausible es que personas con problemas de peso o tendencia a engordar eligen edulcorantes 
como estrategia para controlar su peso, no al revés."

# **************************************************
# PREGUNTA 2.63 - Calificaciones de Lengua y Matemáticas
# **************************************************

# La tabla 2.1 proporciona datos sobre la educación en los diversos Estados de EE UU. 
# La correlación en cada Estado entre la media de las calificaciones de Matemáticas 
# y la media de las calificaciones de Lengua en la prueba SAT es r = 0,970.

# (a) Halla r² y explica con palabras sencillas qué nos indica este número.

"r² = 0.941, lo que significa que el 94.1% de la variación en las calificaciones de Lengua entre estados se 
explica por su relación con las calificaciones de Matemáticas."

# (b) Si calcularas la correlación entre las calificaciones de Matemáticas y las de
# Lengua en la prueba SAT de un gran número de estudiantes individuales, ¿crees
# que la correlación sería 0,97 o bastante distinta? Justifica tu respuesta.

"Sería bastante menor porque a nivel individual hay mucha variabilidad (estudientes buenos en mates pero 
malos en lengua, y viceversa), mientras que los promedios estatales eliminan este 'ruido'."

# **************************************************
# PREGUNTA 2.64 - El té, ¿beneficia a los ancianos?
# **************************************************

# Un grupo de estudiantes universitarios cree que el té tiene efectos muy beneficiosos 
# para la salud. Para verificarlo, los estudiantes decidieron hacer una serie de visitas 
# semanales a una residencia de ancianos. En cada una de estas visitas los estudiantes 
# servían té a los residentes. El personal que los atendía se percató de que al cabo 
# de unos meses muchos de los residentes se mostraban más alegres y tenían un aspecto 
# más saludable.

# (a) Identifica las variables explicativa y respuesta de este estudio.

"Variable explicativa: consumo de té; Variable respuesta: estado de ánimo y salud aparente."

# (b) Explica qué variables latentes pueden explicar la asociación observada.

"Variables latentes como la interacción social durante las visitas, mayor atención recibida, y efecto 
placebo explican mejor la mejora observada que el té mismo."

# **************************************************
# PREGUNTA 2.65 - ¿Es interesante estudiar idiomas?
# **************************************************

# Los miembros del seminario de idiomas de una escuela de secundaria creen que 
# el estudio de una lengua extranjera mejora el dominio de la lengua propia de 
# los estudiantes. La media de las calificaciones de los alumnos que estudiaron 
# una lengua extranjera durante al menos dos años es mucho más alta que la de 
# los alumnos que no la estudiaron.

# (a) Identifica las variables explicativa y respuesta de este estudio.

"Variable explicativa: estudio de lengua extranjera; Variable respuesta: calificaciones en lengua propia."

# (b) Explica qué variable latente anula la conclusión de que el estudio de lenguas 
#     mejora el dominio de la lengua propia.

"La variable latente es la capacidad académica general: estudiantes más capaces eligen estudiar idiomas y 
también obtienen mejores notas en todas las materias."

# **************************************************
# PREGUNTA 2.66 - Formación e ingresos
# **************************************************

# Existe una fuerte correlación positiva entre los años de escolarización x y los 
# ingresos a lo largo de la vida y de los hombres en Europa.

# (a) Sugiere algunas variables latentes que explicarían por qué los hombres con 
# más formación ganan más.

"Variables latentes como capacidad intelectual, ambición personal, conexiones familiares, y acceso a oportunidades 
explican por qué personas con más educación ganan más, no solo los años de estudio."

# **************************************************
# PREGUNTA 2.67 - Las líneas de alta tensión, ¿provocan cáncer?
# **************************************************

# Se ha sugerido que los campos electromagnéticos como los que se hallan junto 
# a las líneas de alta tensión pueden causar leucemia en los niños. Estudios 
# minuciosos sobre el tema no han hallado ninguna asociación entre la exposición 
# a campos electromagnéticos y la leucemia infantil.

# (a) Sugiere algunas variables latentes sobre las que quisieras información con el
# objetivo de investigar la afirmación de que vivir junto a una línea de alta tensión
# está asociado con el cáncer.

"Variables latentes importantes incluyen nivel socioeconómico del vecindario, contaminación ambiental general, 
factores genéticos familiares, y acceso a atención médica, que podrían confundir la relación percibida."

# :::::::::::::::::::::::::::::::::::::::::::::::::::: FIN SECCIÓN ::::::::::::::::::::::::::::::::::::::::::::::::::::