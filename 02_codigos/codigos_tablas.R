# !diagnostics off
library(here) 
library(tidyverse) 
library(knitr) 
library(kableExtra)
library(cowplot) 
library(jtools)
library(ggdist) 
library(gghalves) 
library(patchwork) 
library(plotly)

# datos -------------------------------------------------------------------

# cargo los datos con los cursos obligatorios
source(here("./02_codigos/cargando_datos.R"))

# datos$materia[datos$codigo_materia == "98205"]

# datos por materia de 2015 a 2019 ----------------------------------------

# BMC
t_datos_98101 <- datos %>% 
  filter(str_detect(codigo_materia, "98101")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98101 <- t_datos_98101 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98101 <-kable(t_datos_98101, "pandoc", booktabs = T,
                     caption = "Comparación de los rendimientos por año para BMC")

# CIEV
t_datos_98102 <- datos %>% 
  filter(str_detect(codigo_materia, "98102")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98102 <- t_datos_98102 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98102[3, 5:6] <- c(0)

tb_datos_98102 <-kable(t_datos_98102, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para CIEV")

# Bioestadistica
t_datos_98103 <- datos %>% 
  filter(str_detect(codigo_materia, "98103")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98103 <- t_datos_98103 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98103[3, 2] <- c(0)

tb_datos_98103 <-kable(t_datos_98103, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Bioestadística")

# Anatomia
t_datos_98105 <- datos %>% 
  filter(str_detect(codigo_materia, "98105")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98105 <- t_datos_98105 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98105 <-kable(t_datos_98105, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Anatomía")

# Histologia
t_datos_98106 <- datos %>% 
  filter(str_detect(codigo_materia, "98106")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98106 <- t_datos_98106 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98106 <-kable(t_datos_98106, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Histología")

# Genetica
t_datos_98107 <- datos %>% 
  filter(str_detect(codigo_materia, "98107")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98107 <- t_datos_98107 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98107 <-kable(t_datos_98107, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Genética")

# Fisiologia
t_datos_98201 <- datos %>% 
  filter(str_detect(codigo_materia, "98201")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98201 <- t_datos_98201 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98201 <-kable(t_datos_98201, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Fisiología")

# Microbiología
t_datos_98202 <- datos %>% 
  filter(str_detect(codigo_materia, "98202")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98202 <- t_datos_98202 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98202 <-kable(t_datos_98202, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Microbiología")

# Inmunologia
t_datos_98203 <- datos %>% 
  filter(str_detect(codigo_materia, "98203")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98203 <- t_datos_98203 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98203 <-kable(t_datos_98203, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Inmunología")

# Zootecnia
t_datos_98204 <- datos %>% 
  filter(str_detect(codigo_materia, "98204")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98204 <- t_datos_98204 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98204[3, 2] <- c(0)

tb_datos_98204 <-kable(t_datos_98204, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Zootecnia")

# Patología
t_datos_98205 <- datos %>% 
  filter(str_detect(codigo_materia, "98205")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98205 <- t_datos_98205 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98205 <-kable(t_datos_98205, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Patología")

# Semiologia
t_datos_98206 <- datos %>% 
  filter(str_detect(codigo_materia, "98206")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98206 <- t_datos_98206 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98206 <-kable(t_datos_98206, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Semiología")

# Farmacologia
t_datos_98207 <- datos %>% 
  filter(str_detect(codigo_materia, "98207")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98207 <- t_datos_98207 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98207 <-kable(t_datos_98207, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Farmacología")

# Nutricion
t_datos_98208 <- datos %>% 
  filter(str_detect(codigo_materia, "98208")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98208 <- t_datos_98208 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98208[3, 2] <- c(0)

tb_datos_98208 <-kable(t_datos_98208, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Nutrición")

# Infecto
t_datos_98301 <- datos %>% 
  filter(str_detect(codigo_materia, "98301")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98301 <- t_datos_98301 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98301[3, 2] <- c(0)

tb_datos_98301 <-kable(t_datos_98301, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Enfermedades infecciosas")

# Parasitología
t_datos_98302 <- datos %>% 
  filter(str_detect(codigo_materia, "98302")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98302 <- t_datos_98302 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98302 <-kable(t_datos_98302, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Parasitología")

# Toxicología
t_datos_98303 <- datos %>% 
  filter(str_detect(codigo_materia, "98303")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98303 <- t_datos_98303 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98303 <-kable(t_datos_98303, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Toxicología")

# Epidemiología
t_datos_98304 <- datos %>% 
  filter(str_detect(codigo_materia, "98304")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98304 <- t_datos_98304 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98304[3, 2] <- c(0)

tb_datos_98304 <-kable(t_datos_98304, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Epidemiología")

# Pequeños
t_datos_98305 <- datos %>% 
  filter(str_detect(codigo_materia, "98305")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98305 <- t_datos_98305 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98305[2, c(2, 4, 6)] <- c(0)

tb_datos_98305 <-kable(t_datos_98305, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Clínica de pequeños")

# Equinos
t_datos_98306 <- datos %>% 
  filter(str_detect(codigo_materia, "98306")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98306 <- t_datos_98306 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98306[2, 2] <- c(0)

tb_datos_98306 <-kable(t_datos_98306, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Clínica de equinos")

# Rumiantes
t_datos_98307 <- datos %>% 
  filter(str_detect(codigo_materia, "98307")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98307 <- t_datos_98307 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98307[2, 3:4] <- c(0)

tb_datos_98307 <-kable(t_datos_98307, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Clínica de rumiantes")

# Terio
t_datos_98308 <- datos %>% 
  filter(str_detect(codigo_materia, "98308")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98308 <- t_datos_98308 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98308[3, 2] <- c(0)

tb_datos_98308 <-kable(t_datos_98308, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Teriogenología")

# Tecnica quirurgica
t_datos_98309 <- datos %>% 
  filter(str_detect(codigo_materia, "98309")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98309 <- t_datos_98309 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98309[3, c(2, 4:6)] <- c(0)

tb_datos_98309 <-kable(t_datos_98309, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Técnica quirúrgica")

# Leg sanitaria
t_datos_98310 <- datos %>% 
  filter(str_detect(codigo_materia, "98310")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98310 <- t_datos_98310 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98310[3, 2] <- c(0)

tb_datos_98310 <-kable(t_datos_98310, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Legislación sanitaria")

# Prod rumiantes
t_datos_98401 <- datos %>% 
  filter(str_detect(codigo_materia, "98401")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98401 <- t_datos_98401 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98401[3, c(2, 4:5)] <- c(0)

tb_datos_98401 <-kable(t_datos_98401, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Producción de rumiantes")

# Prod suinos
t_datos_98402 <- datos %>% 
  filter(str_detect(codigo_materia, "98402")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98402 <- t_datos_98402 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98402[1, 3:4] <- c(0)
t_datos_98402[3, c(2, 4:5)] <- c(0)

tb_datos_98402 <-kable(t_datos_98402, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Producción de suinos")

# Alimentacion
t_datos_98403 <- datos %>% 
  filter(str_detect(codigo_materia, "98403")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98403 <- t_datos_98403 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98403[3, c(2, 4)] <- c(0)

tb_datos_98403 <-kable(t_datos_98403, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Alimentación")

# Economia
t_datos_98404 <- datos %>% 
  filter(str_detect(codigo_materia, "98404")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98404 <- t_datos_98404 %>%
  pivot_wider(names_from = ano, values_from = n)

tb_datos_98404 <-kable(t_datos_98404, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Economía")

# Mejora
t_datos_98405 <- datos %>% 
  filter(str_detect(codigo_materia, "98405")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98405 <- t_datos_98405 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98405[3, 2] <- c(0)

tb_datos_98405 <-kable(t_datos_98405, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Mejora genética")

# Leg agraria
t_datos_98406 <- datos %>% 
  filter(str_detect(codigo_materia, "98406")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98406 <- t_datos_98406 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98406[3, 2] <- c(0)

tb_datos_98406 <-kable(t_datos_98406, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Legislación agraria")

# Patologia
t_datos_98407 <- datos %>% 
  filter(str_detect(codigo_materia, "98407")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98407 <- t_datos_98407 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98407[3, 2:3] <- c(0)

tb_datos_98407 <-kable(t_datos_98407, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Patología")

# Higiene
t_datos_98501 <- datos %>% 
  filter(str_detect(codigo_materia, "98501")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98501 <- t_datos_98501 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98501[3, 3:6] <- c(0)

tb_datos_98501 <-kable(t_datos_98501, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Higiene")

# CyT
t_datos_98502 <- datos %>% 
  filter(str_detect(codigo_materia, "98502")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98502 <- t_datos_98502 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98502[3, 3:6] <- c(0)

tb_datos_98502 <-kable(t_datos_98502, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Ciencia y tecnología")

# Salud publica
t_datos_98503 <- datos %>% 
  filter(str_detect(codigo_materia, "98503")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98503 <- t_datos_98503 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98503[3, 2:3] <- c(0)

tb_datos_98503 <-kable(t_datos_98503, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Salud pública")

# Leg alimentaria
t_datos_98504 <- datos %>% 
  filter(str_detect(codigo_materia, "98504")) %>%
  group_by(ano, resultado) %>% 
  summarise(n = n())

t_datos_98504 <- t_datos_98504 %>%
  pivot_wider(names_from = ano, values_from = n)

t_datos_98504[3, c(2, 6)] <- c(0)

tb_datos_98504 <-kable(t_datos_98504, "pandoc", booktabs = T,
                       caption = "Comparación de los rendimientos por año para Legislación alimentaria")
