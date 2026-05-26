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

inte_2006 %>%
  filter(depdes == "asuncion") %>%
  count(siglas_lista, sort = TRUE)

inte_2006 %>%
  filter(depdes == "asuncion") %>%
  summarise(
    total_validos = sum(!is.na(siglas_lista)),
    anr = sum(siglas_lista == "ANR", na.rm = TRUE),
    no_anr = sum(siglas_lista != "ANR", na.rm = TRUE),
    pct_no_anr = no_anr / total_validos * 100)
    

inte_2006 %>%
  filter(depdes == "asuncion") %>%
  summarise(
    total_validas = sum(!is.na(siglas_lista)),
    no_anr = sum(siglas_lista != "ANR", na.rm = TRUE),
    anr = sum(siglas_lista == "ANR", na.rm = TRUE),
    pct_no_anr = 100 * no_anr / total_validas,
    pct_anr = 100 * anr / total_validas)

inte_2006 %>% group_by(depdes) %>% summarise(cant_mujeres = sum(sexo == "F", na.rm = TRUE)) %>% arrange(desc(cant_mujeres))

#Top 5 de intendentes que ganaron por los mayores margenes (% de votos del primero menos % de votos del segundo)

inte_2006 %>%
  filter(candidatura == 1, rank_porcentaje == 1) %>%
  mutate(margen = 100 * (first_porcentaje - second_porcentaje)) %>%
  arrange(desc(margen)) %>%
  slice_head(n = 5)

#Top 5 de las elecciones más competitivas

inte_2006 %>%
  filter(candidatura == 1, rank_porcentaje == 1) %>%
  mutate(margen = 100 * (first_porcentaje - second_porcentaje)) %>%
  arrange(margen) %>%
  slice_head(n = 5)


