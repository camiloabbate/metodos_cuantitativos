rm(list = ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

inten_2015 <- full_intendentes_margen_2001_2021 %>% 
  filter(ano == 2015, electo == 1)


# que porcentaje de los intendentes son de sexo femenino
# cual es la edad promedio de los intendentes
# de los intendentes colorados, cual es el promedio de edad
# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS

names(inten_2015)


# que porcentaje de los intendentes son de sexo femenino
inte_2015
# El porcentaje de intendentes de sexo femenino en 2015 fue de 10.3%.


# cual es la edad promedio de los intendentes

inten_2015 %>% 
  summarise(promedio_edad = mean(edad, na.rm = TRUE))
# La edad promedio de los intendentes en 2015 fue de 44.0 años.


# de los intendentes colorados, cual es el promedio de edad
inten_2015 %>% 
  filter(siglas_lista == "ANR") %>% 
  summarise(promedio_colorados = mean(edad, na.rm = TRUE))
# La edad promedio de los intendentes colorados en 2015 fue de 44.1 años.

inten_2015 %>% 
  mutate(no_colorado = siglas_lista != "ANR") %>% 
  group_by(dep) %>% 
  summarise(porcentaje_no_colorados = mean(no_colorado, na.rm = TRUE) * 100) %>% 
  arrange(desc(porcentaje_no_colorados))

# El departamento con mayor porcentaje de intendentes no colorados es el
#departamento 10 que es Alto Parana


inten_2015 %>% 
  filter(dep == 10) %>% 
  select(dep, depdes) %>% 
  distinct()


# Top 5 de los distritos en los que el ganador, gano por el mayor margen de victoria
# como se define margen de victoria: basicamente el % de votos del 1ro menos el % de votos




