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

# filtrado por materias curriculares --------------------------------------

# selecciono los cursos obligatorios del tronco comun
datos_bed <- datos_bed %>%
  dplyr::select(servicio, 
                codigo_materia, 
                materia, 
                aprobado, 
                nota, 
                fecha,
                servicio_instancia) %>% 
  filter(str_detect(codigo_materia, "98")) %>% 
  filter(codigo_materia != "1098") %>% 
  filter(materia != "INGLES TECNICO (OPCIONAL)") %>% 
  filter(materia != "INGLES TECNICO II (OPCIONAL)")

# seleccionando datos montevideo ------------------------------------------

# selecciono los datos de montevideo
# separo fecha en tres variables para quedarmo solo con el ano
datos <- datos_bed %>% 
  filter(servicio_instancia == "FVET") %>% 
  separate(fecha, c("dia", "periodo", "anho")) %>% 
  mutate(dia = as.factor(dia),
         periodo = as.factor(periodo),
         ano = as.factor(anho))


datos <- datos %>% 
  mutate(aprobado = as.character(aprobado))

# creo el nivel exonerado
datos$aprobado[datos$nota > 0] <- "Exonerado"

# renoimbro la variable aprobado como resultado
datos <- datos %>% 
  mutate(aprobado = as.factor(aprobado)) %>% 
  rename(resultado = aprobado)

