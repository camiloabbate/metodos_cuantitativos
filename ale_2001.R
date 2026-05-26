


rm(list = ls())

library(pacman)

p_load(tidyverse,tidylog)


load("full_intendentes_margen_2001_2021.RData")

inten_2001 <- subset(
  full_intendentes_margen_2001_2021,
  ano == 2001 & electo == 1)

cantidad_por_partido <- as.data.frame(
  sort(table(inten_2001$siglas_lista), decreasing = TRUE)
)
names(cantidad_por_partido) <- c("siglas_lista", "cantidad")

cantidad_por_partido

colnames(inten_2001)

# que porcentaje de los intendentes son de sexo femenino
porcentaje_mujeres <- data.frame(
  porcentaje_mujeres = mean(inten_2001$sexo == "F", na.rm = TRUE) * 100
)

cat("\n1. Porcentaje de intendentes mujeres:\n")
porcentaje_mujeres

inten_2001 %>% group_by(sexo) %>% count()


# cual es la edad promedio de los intendentes
edad_promedio <- data.frame(
  edad_promedio = mean(inten_2001$edad, na.rm = TRUE)
)

cat("\n2. Edad promedio de los intendentes:\n")
edad_promedio

# de los intendentes colorados, cual es el promedio de edad
inten_2001_colorados <- subset(inten_2001, siglas_lista == "ANR")

edad_promedio_colorados <- data.frame(
  edad_promedio_colorados = mean(inten_2001_colorados$edad, na.rm = TRUE)
)

cat("\n3. Edad promedio de los intendentes colorados ANR:\n")
edad_promedio_colorados

# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS
inten_2001$no_colorado <- inten_2001$siglas_lista != "ANR"

total_por_departamento <- aggregate(
  unique_id ~ depdes,
  data = inten_2001,
  FUN = length
)
names(total_por_departamento) <- c("depdes", "total_intendentes")

no_colorados_por_departamento <- aggregate(
  no_colorado ~ depdes,
  data = inten_2001,
  FUN = sum
)
names(no_colorados_por_departamento) <- c("depdes", "intendentes_no_colorados")

porcentaje_no_colorados_por_departamento <- merge(
  total_por_departamento,
  no_colorados_por_departamento,
  by = "depdes"
)

porcentaje_no_colorados_por_departamento$porcentaje_no_colorados <-
  porcentaje_no_colorados_por_departamento$intendentes_no_colorados /
  porcentaje_no_colorados_por_departamento$total_intendentes * 100

porcentaje_no_colorados_por_departamento <- porcentaje_no_colorados_por_departamento[
  order(
    -porcentaje_no_colorados_por_departamento$porcentaje_no_colorados,
    -porcentaje_no_colorados_por_departamento$total_intendentes
  ),
]

departamento_mayor_porcentaje_no_colorados <-
  porcentaje_no_colorados_por_departamento[1,]

cat("\n4. Departamento con mayor porcentaje de intendentes no colorados:\n")
departamento_mayor_porcentaje_no_colorados


# AVERIGUAR, PORQUE EN BOQUERON SOLO APARECE 1 DISTRITO!



# Sugerencias de preguntas adicionales para 2001:
# - Cual fue el partido/lista con mayor cantidad de intendentes electos?
# - Cual fue el departamento con mayor cantidad de intendentes electos por la ANR?
# - Cual fue el margen de victoria promedio de los intendentes electos?
# - En que distrito se dio la victoria mas ajustada?
# - En que distrito se dio la victoria mas amplia?
# - Cual fue el porcentaje promedio de votos obtenido por los intendentes electos?
# - Existen diferencias de edad promedio entre intendentes hombres y mujeres?



