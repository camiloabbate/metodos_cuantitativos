rm(list = ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

inten_2015 <- full_intendentes_margen_2001_2021 %>% filter(ano = 2015)

# que porcentaje de los intendentes son de sexo femenino
# cual es la edad promedio de los intendentes
# de los intendentes colorados, cual es el promedio de edad
# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS