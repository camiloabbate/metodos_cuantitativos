rm(list = ls())

library(tidyverse)

load("full_intendentes_margen_2001_2021.RData")

inten_2021 <- full_intendentes_margen_2001_2021 %>% filter(ano == 2021)

# que porcentaje de los intendentes son de sexo femenino
# cual es la edad promedio de los intendentes
# de los intendentes colorados, cual es el promedio de edad
# cual es el departamento con mayor porcentaje de intendentes NO-COLORADOS


---
  title: "Análisis de intendentes electos en Paraguay, 2021"
author: "Samuel Recalde"
date: "2026-05-19"
output:
  html_document:
  toc: true
toc_float: true
number_sections: true
theme: cosmo
---
  
```{r setup, include=FALSE}
  knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 8,
  fig.height = 5
)
  

library(tidyverse)
library(knitr)
library(scales)
```

# Introducción

#Este trabajo analiza la base de intendentes municipales electos en Paraguay para el año 2021. El objetivo es responder preguntas descriptivas sobre sexo, edad, partido político y distribución departamental.

```{r carga-datos}
rm(list = ls())

load("full_intendentes_margen_2001_2021.RData")

inten_2021 <- full_intendentes_margen_2001_2021 %>% 
  filter(ano == 2021)

head(inten_2021)
```

# Pregunta 1

#¿Qué porcentaje de los intendentes son de sexo femenino?
  
  ```{r pregunta-1}
inten_2021 %>% 
  count(sexo) %>% 
  mutate(
    porcentaje = n / sum(n) * 100
  )
```

# Pregunta 2

#¿Cuál es la edad promedio de los intendentes?
  
  ```{r pregunta-2}
inten_2021 %>% 
  summarise(
    edad_promedio = mean(edad, na.rm = TRUE)
  )
```

# Pregunta 3

#De los intendentes colorados, ¿cuál es el promedio de edad?
  
  ```{r pregunta-3}
inten_2021 %>% 
  filter(partido == "ANR") %>% 
  summarise(
    edad_promedio_colorados = mean(edad, na.rm = TRUE)
  )
```

# Pregunta 4

#¿Cuál es el departamento con mayor porcentaje de intendentes no colorados?
  
  ```{r pregunta-4}
inten_2021 %>% 
  mutate(
    no_colorado = partido != "ANR"
  ) %>% 
  group_by(departamento) %>% 
  summarise(
    total_intendentes = n(),
    intendentes_no_colorados = sum(no_colorado, na.rm = TRUE),
    porcentaje_no_colorados = mean(no_colorado, na.rm = TRUE) * 100
  ) %>% 
  arrange(desc(porcentaje_no_colorados))
```

# Conclusión

El análisis permite observar la composición de los intendentes electos en 2021 según sexo, edad, partido político y departamento. En particular, permite identificar la participación femenina, la edad promedio de las autoridades municipales y la distribución territorial de intendentes colorados y no colorados.
