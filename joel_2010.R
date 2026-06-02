rm(list = ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

intent_2010 <- full_intendentes_margen_2001_2021 %>% 
  filter(ano == 2010, electo == 1)


# Fijarte que ahora mismo tu codigo esta analizando TODOS LOS AÑOS
# Vos en realidad queres ver solo el año 2010:




# que porcentaje de los intendentes son de sexo femenino
# cual es la edad promedio de los intendentes
# de los intendentes colorados, cual es el promedio de edad
# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS

intent_2010 %>% 
  summarise(
    porcentaje_mujeres = mean(sexo == "F", na.rm = TRUE) * 100
  )

# Si quieres verlo ya redondeado:
  
  intent_2010 %>% 
  summarise(
    porcentaje_mujeres = round(mean(sexo == "F", na.rm = TRUE) * 100, 2)
  )

  
  intent_2010 %>% 
    summarise(edad_promedio = round(mean(edad, na.rm = TRUE), 2))
  
  colnames(intent_2010)
  
  intent_2010 %>% filter(siglas_lista == "ANR") %>% 
    summarise(
      edad_promedio_colorados = round(mean(edad, na.rm = TRUE), 2)
    )
  
  colorados_2010 <- intent_2010 %>% 
    filter(siglas_lista == "ANR")  

  colorados_2010 %>% 
    summarise(
      edad_promedio_colorados = round(mean(edad, na.rm = TRUE), 2)
    )  
  
  intent_2010 %>%
    mutate(
      no_colorado = ifelse(siglas_lista != "ANR",1,0)
    ) %>% 
    group_by(depdes) %>% 
    summarise(
      porcentaje_no_colorados = round(mean(no_colorado, na.rm = TRUE) * 100, 2)
    ) %>% 
    arrange(desc(porcentaje_no_colorados))
  
  full_intendentes_margen_2001_2021$nombre_lista %>% unique()
  
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010, nombre_lista == "blancos") %>% 
    mutate(
      porcentaje_blanco = votos / total_de_votos_eleccion * 100
    ) %>% 
    select(ano, depdes, disdes, votos, total_de_votos_eleccion, porcentaje) %>% 
    arrange(desc(porcentaje))
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010, nombre_lista == "nulos") %>% 
    mutate(
      porcentaje_nulo = votos / total_de_votos_eleccion * 100
    ) %>% 
    select(ano, depdes, disdes, votos, total_de_votos_eleccion, porcentaje) %>% 
    arrange(desc(porcentaje))
  
  
  #RESPUESTAS
  # El porcentaje de intendentes de sexo femenino en 2010 es de aproximadamente 9.72%.
  # La edad promedio de los intendentes en 2010 es de aproximadamente 44.2 años.
  # La edad promedio de los intendentes colorados en 2010 es de aproximadamente 44.2 años.
  # El departamento con mayor porcentaje de intendentes NO-COLORADOS en 2010 es "CENTRAL" 
  # con un porcentaje de 90.3%.
  
  # El porcentaje de votos en blanco en las elecciones municipales de 2010 fue del 0.95%.
    # representan 16.431 votos
  
  # El porcentaje de votos nulos en las elecciones municipales de 2010 fue del 3%.
   #representan 51.829 votos.
  