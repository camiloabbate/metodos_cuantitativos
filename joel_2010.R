rm(list = ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

intent_2010 <- full_intendentes_margen_2001_2021 %>% 
  filter(ano == 2010)


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
  inte_2010 <- intent_2010 %>% 
    filter(ano == 2010)
  
  inte_2010 %>% 
    summarise(
      porcentaje_mujeres = round(mean(sexo == "F", na.rm = TRUE) * 100, 2)
    )
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010) %>% 
    summarise(edad_promedio = round(mean(edad, na.rm = TRUE), 2))
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010, partido == "ANR") %>% 
    summarise(
      edad_promedio_colorados = round(mean(edad, na.rm = TRUE), 2)
    )
  
  names(full_intendentes_margen_2001_2021)

  unique(full_intendentes_margen_2001_2021)
  
  full_intendentes_margen_2001_2021 %>% 
    filter(
      ano == 2010,
      PARTIDO %in% c("ANR", "Asociacion Nacional Republicana", "Colorado", "Partido Colorado")
    ) %>% 
    summarise(
      edad_promedio_colorados = round(mean(edad, na.rm = TRUE), 2)
    )
  load("full_intendentes_margen_2001_2021.RData")  
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010) %>% 
    View()

  intent_2010 <- full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010)

  colorados_2010 <- intent_2010 %>% 
    filter(nombre_lista == "PARTIDO COLORADO")  

  colorados_2010 %>% 
    summarise(
      edad_promedio_colorados = round(mean(edad, na.rm = TRUE), 2)
    )  
  
  full_intendentes_margen_2001_2021 %>% 
    mutate(
      no_colorado = nombre_lista != "PARTIDO COLORADO"
    ) %>% 
    group_by(depdes) %>% 
    summarise(
      porcentaje_no_colorados = round(mean(no_colorado, na.rm = TRUE) * 100, 2)
    ) %>% 
    arrange(desc(porcentaje_no_colorados)) %>% 
    slice(1)
  
  full_intendentes_margen_2001_2021 %>% 
    mutate(
      no_colorado = nombre_lista != "PARTIDO COLORADO"
    ) %>% 
    group_by(depdes) %>% 
    summarise(
      porcentaje_no_colorados = round(mean(no_colorado, na.rm = TRUE) * 100, 2)
    ) %>% 
    arrange(desc(porcentaje_no_colorados))
  

  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010, str_to_upper(nombre_lista) == "BLANCO") %>% 
    mutate(
      porcentaje_blanco = votos / total_de_votos_eleccion * 100
    ) %>% 
    select(ano, depdes, disdes, votos, total_de_votos_eleccion, porcentaje)
  
  full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010, str_detect(str_to_upper(nombre_lista), "BLANCO"))
  
  blancos_2010 <- full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010) %>% 
    group_by(depdes, disdes) %>% 
    summarise(
      votos_blancos = sum(votos[str_detect(str_to_upper(nombre_lista), "BLANCO")], na.rm = TRUE),
      total_votos_eleccion = first(total_de_votos_eleccion),
      .groups = "drop"
    )
  
  blancos_2010 %>% 
    summarise(
      total_votos_blancos = sum(votos_blancos, na.rm = TRUE),
      total_votos_2010 = sum(total_votos_eleccion, na.rm = TRUE),
      porcentaje_total_blancos = round(total_votos_blancos / total_votos_2010 * 100, 2)
    )
  
  nulos_2010 <- full_intendentes_margen_2001_2021 %>% 
    filter(ano == 2010) %>% 
    group_by(depdes, disdes) %>% 
    summarise(
      votos_nulos = sum(votos[str_detect(str_to_upper(nombre_lista), "NULO")], na.rm = TRUE),
      total_votos_eleccion = first(total_de_votos_eleccion),
      .groups = "drop"
    )
  
  nulos_2010 %>% 
    summarise(
      total_votos_nulos = sum(votos_nulos, na.rm = TRUE),
      total_votos_2010 = sum(total_votos_eleccion, na.rm = TRUE),
      porcentaje_total_nulos = round(total_votos_nulos / total_votos_2010 * 100, 2)
    )
  
  
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
  