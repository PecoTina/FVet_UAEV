# !diagnostics off
library(tidyverse)
library(janitor)

# cargo la planilla de datos con todos los registros
datos_bed <- data_frame(read_csv("01_datos/rendimientos_2015_2019.csv", 
                            col_types = cols(Aprobado = col_factor(levels = c()), 
                                             `Código materia` = col_factor(levels = c()), 
                                             Materia = col_factor(levels = c()), 
                                             Servicio = col_factor(levels = c()), 
                                             `Servicio instancia` = col_factor(levels = c()), 
                                             Sexo = col_factor(levels = c()), 
                                             `Tipo actividad` = col_factor(levels = c())))) %>% 
  clean_names()

# View(problems(datos))
# 
# View(datos[338, ])


# filtrado por materias curriculares --------------------------------------

datos_bed <- datos_bed %>%
  dplyr::select(servicio, 
                codigo_materia, 
                materia, 
                aprobado, 
                nota, 
                fecha,
                servicio_instancia) %>% 
  filter(str_detect(codigo_materia, "98")) %>% 
  filter(codigo_materia != "1098")


