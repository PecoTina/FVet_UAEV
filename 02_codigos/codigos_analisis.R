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
library(chisq.posthoc.test)
# datos -------------------------------------------------------------------

# cargo los datos con los cursos obligatorios
source(here("./02_codigos/cargando_datos.R"))

# cargo las materias para el analisis 
source(here("./02_codigos/codigos_tablas.R"))


# datos por materia de 2015 a 2019 ----------------------------------------

# BMC
c_datos_98101 <- data.frame(t_datos_98101[-1])
row.names(t_datos_98101$resultado)
chisq.test(c_datos_98101)
chisq.posthoc.test(c_datos_98101)

# CIEV
c_datos_98102 <- data.frame(t_datos_98102[-1])
chisq.test(c_datos_98102)

# Bioestadistica
c_datos_98103 <- data.frame(t_datos_98103[-1])
chisq.test(c_datos_98103)

# Anatomia
c_datos_98105 <- data.frame(t_datos_98105[-1])
chisq.test(c_datos_98105)

# Histologia
c_datos_98106 <- data.frame(t_datos_98106[-1])
chisq.test(c_datos_98106)

# Genetica
c_datos_98107 <- data.frame(t_datos_98107[-1])
chisq.test(c_datos_98107)

# Fisiologia
c_datos_98201 <- data.frame(t_datos_98201[-1])
chisq.test(c_datos_98201)

# Microbiología
c_datos_98202 <- data.frame(t_datos_98202[-1])
chisq.test(c_datos_98202)

# Inmunologia
c_datos_98203 <- data.frame(t_datos_98203[-1])
chisq.test(c_datos_98203)

# Zootecnia
c_datos_98204 <- data.frame(t_datos_98204[-1])
chisq.test(c_datos_98204)

# Patología
c_datos_98205 <- data.frame(t_datos_98205[-1])
chisq.test(c_datos_98205)

# Semiologia
c_datos_98206<- data.frame(t_datos_98206[-1])
chisq.test(c_datos_98206)

# Farmacologia
c_datos_98207 <- data.frame(t_datos_98207[-1])
chisq.test(c_datos_98207)

# Nutricion
c_datos_98208 <- data.frame(t_datos_98208[-1])
chisq.test(c_datos_98208)

# Infecto
c_datos_98301 <- data.frame(t_datos_98301[-1])
chisq.test(c_datos_98301)

# Parasitología
c_datos_98302 <- data.frame(t_datos_98302[-1])
chisq.test(c_datos_98302)

# Toxicología
c_datos_98303 <- data.frame(t_datos_98303[-1])
chisq.test(c_datos_98303)

# Epidemiología
c_datos_98304 <- data.frame(t_datos_98304[-1])
chisq.test(c_datos_98304)

# Pequeños
c_datos_98305 <- data.frame(t_datos_98305[-1])
chisq.test(c_datos_98305)

# Equinos
c_datos_98306 <- data.frame(t_datos_98306[-1])
chisq.test(c_datos_98306)

# Rumiantes
c_datos_98307 <- data.frame(t_datos_98307[-1])
chisq.test(c_datos_98307)

# Terio
c_datos_98308 <- data.frame(t_datos_98308[-1])
chisq.test(c_datos_98308)

# Tecnica quirurgica
c_datos_98309 <- data.frame(t_datos_98309[-1])
chisq.test(c_datos_98309)

# Leg sanitaria
c_datos_98310 <- data.frame(t_datos_98310[-1])
chisq.test(c_datos_98310)

# Prod rumiantes
c_datos_98401 <- data.frame(t_datos_98401[-1])
chisq.test(c_datos_98401)

# Prod suinos
c_datos_98402 <- data.frame(t_datos_98402[-1])
chisq.test(c_datos_98402)

# Alimentacion
c_datos_98403 <- data.frame(t_datos_98403[-1])
chisq.test(c_datos_98403)

# Economia
c_datos_98404 <- data.frame(t_datos_98404[-1])
chisq.test(c_datos_98404)

# Mejora
c_datos_98405 <- data.frame(t_datos_98405[-1])
chisq.test(c_datos_98405)

# Leg agraria
c_datos_98406 <- data.frame(t_datos_98406[-1])
chisq.test(c_datos_98406)

# Patologia
c_datos_98407 <- data.frame(t_datos_98407[-1])
chisq.test(c_datos_98407)

# Higiene
c_datos_98501 <- data.frame(t_datos_98501[-1])
chisq.test(c_datos_98501)

# CyT
c_datos_98502 <- data.frame(t_datos_98502[-1])
chisq.test(c_datos_98502)

# Salud publica
c_datos_98503 <- data.frame(t_datos_98503[-1])
chisq.test(c_datos_98503)

# Leg alimentaria
c_datos_98504 <- data.frame(t_datos_98504[-1])
chisq.test(c_datos_98504)
