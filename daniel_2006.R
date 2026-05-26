rm(list=ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

inte_2006 <- full_intendentes_margen_2001_2021 %>% filter(ano == 2006)


# que porcentaje de los intendentes son de sexo femenino
# cual es la edad promedio de los intendentes
# de los intendentes colorados, cual es el promedio de edad
# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS
# cuantas intendentes mujeres hay en cada departamento

inte_2006 %>% summarise(porcentaje_femenino = sum (sexo == "F", na.rm = TRUE)/ sum(!is.na(sexo)) * 100)

inte_2006 %>% summarise(edad_promedio = mean(edad, na.rm = TRUE))

inte_2006 %>% filter(siglas_lista == "ANR") %>% summarise(edad_promedio_colorados = mean(edad, na.rm = TRUE))

inte_2006 %>% group_by(depdes) %>% summarise(porcentaje_no_colorados = sum(siglas_lista != "ANR", na.rm = TRUE)/ sum(!is.na(siglas_lista)) * 100) %>% arrange(desc(porcentaje_no_colorados))

# COMO puede ser que asuncion tiene un porcentaje que NO es 0% o 100%?


inte_2006 %>% group_by(depdes) %>% summarise(cant_mujeres = sum(sexo == "F", na.rm = TRUE)) %>% arrange(desc(cant_mujeres))
