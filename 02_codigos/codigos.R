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

source(here("./02_codigos/cargando_datos.R"))

# tema del grafico --------------------------------------------------------

mi_tema <- theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 10, colour = "black"),
        axis.title = element_text(size = 11),
        plot.title = element_text(size = 11),
        panel.background =  element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA),
        text = element_text(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# seleccionando datos montevideo ------------------------------------------

datos <- datos_bed %>% 
  filter(servicio_instancia == "FVET") %>% 
  separate(fecha, c("dia", "periodo", "anho")) %>% 
  mutate(dia = as.factor(dia),
         periodo = as.factor(periodo),
         ano = as.factor(anho))
         
datos <- datos %>% 
  mutate(aprobado = as.character(aprobado))

datos$aprobado[datos$nota > 0] <- "Exonerado"

datos <- datos %>% 
  mutate(aprobado = as.factor(aprobado)) %>% 
  rename(resultado = aprobado)

# datos por materia de 2015 a 2019 ----------------------------------------

datos_981_m <- datos %>% 
  filter(str_detect(codigo_materia, "981")) %>%
  group_by(materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("RENDIMIENTOS PRIMER AÑO POR MATERIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

#50c878  #85bb65 #00a86b  #29ab87
#0d98ba  #26619c  #0077be
#e52b50  #ec3b83  #e75480

# datos$materia[datos$codigo_materia == "98205"]

### 98101 BIOLOGIA CELULAR Y MOLECULAR
### 98102 C.I.E.V.
### 98103 BIOESTADISTICA I
### 98105 ANATOMIA NORMAL
### 98106 HISTOLOGIA NORMAL Y EMBRIOLOGIA DEL DESARROLLO
### 98107 GENETICA GENERAL
### 98201 FISIOLOGIA
### 98202 MICROBIOLOGIA
### 98203 INMUNOLOGIA BASICA
### 98204 ZOOTECNIA (TECNOLOGIA)
### 98205 PATOLOGIA FUNCIONAL Y MORFOLOGIA
### 98206 SEMIOLOGIA
### 98207 FARMACOLOGIA BASICA
### 98208 NUTRICION
### 98301 ENFERMEDADES INFECCIOSAS
### 98302 Parasitologia y enfermedades parasitarias
### 98303 Toxicologia y enfermedades toxicologicas
### 98304 Medicina preventiva y epidemiología
### 98305 Patologia y clinica de peq animales
### 98306 Patologia y clinica de equinos I
### 98307 Parología y clinica rumiantes y suinos I
### 98308 Teriogenologia I
### 98309 Tecnica quirurgica
### 98310 Legislacion sanitaria
### 98401 Produccion de rumiantes I
### 98402 Prod de suinos y animales de granja I
### 98403 Alimentacion
### 98404 Economía y administración agropecuaria
### 98405 Mejora genetica
### 98406 Legislacion agraria
### 98407 Patologia, clinica y produccion avicola
### 98501 Higiene, inspeccion-control de alimentos de origen animal
### 98502 Ciencia y tecnologia de alimentos de origen animal
### 98503 Salud publica veterinaria
### 98504 Legislacion alimentaria y ambiental

datos_982_m <- datos %>% 
  filter(str_detect(codigo_materia, "982")) %>%
  group_by(materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("RENDIMIENTOS SEGUNDO AÑO POR MATERIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_983_m <- datos %>% 
  filter(str_detect(codigo_materia, "983")) %>%
  group_by(materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("RENDIMIENTOS TERCER AÑO POR MATERIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_984_m <- datos %>% 
  filter(str_detect(codigo_materia, "984")) %>%
  group_by(materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("RENDIMIENTOS CUARTO AÑO POR MATERIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_985_m <- datos %>% 
  filter(str_detect(codigo_materia, "985")) %>%
  group_by(materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("RENDIMIENTOS QUINTO AÑO POR MATERIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# datos por ano lectivo y ano curricular ------------------------------------------------

datos_981_a <- datos %>% 
  filter(str_detect(codigo_materia, "981")) %>%
  group_by(codigo_materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = codigo_materia, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("MATERIAS DE PRIMER AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_982_a <- datos %>% 
  filter(str_detect(codigo_materia, "982")) %>%
  group_by(codigo_materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = codigo_materia, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("MATERIAS DE SEGUNDO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_983_a <- datos %>% 
  filter(str_detect(codigo_materia, "983")) %>%
  group_by(codigo_materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = codigo_materia, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("MATERIAS DE TERCER AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_984_a <- datos %>% 
  filter(str_detect(codigo_materia, "984")) %>%
  group_by(codigo_materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = codigo_materia, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("MATERIAS DE CUARTO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

datos_985_a <- datos %>% 
  filter(str_detect(codigo_materia, "985")) %>%
  group_by(codigo_materia, ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = codigo_materia, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text(),
        axis.text.x = element_text(angle = -90, vjust = 1, hjust = 0)) +
  labs (x = "materia",
        y = "resultados del curso por año") +
  ggtitle("MATERIAS DE QUINTO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))



# Render Dashboard --------------------------------------------------------

rmarkdown::render(
  input = here("./03_resultados/informe_rendimientos_2015_2019.Rmd"),
  output_format = "pdf_document",
  output_dir = here("./03_resultados/"),
  run_pandoc = TRUE,
  output_file = paste0("informe_rendimientos_2015_2019.pdf")
)

