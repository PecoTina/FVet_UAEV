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

datos <- datos %>% 
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

# datos_2019 <- datos %>% 
#   filter(str_detect(anho, "2019"))
# 
# datos_1 <- datos %>% 
#   filter(str_detect(codigo_materia, "981"))

# datos por anho lectivo y materia ----------------------------------------

### biologia molecular y celular

datos98101 <- datos %>% 
  filter(codigo_materia == "98101") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Bio. Molecular y Celular") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

#50c878  #85bb65 #00a86b  #29ab87
#0d98ba  #26619c  #0077be
#e52b50  #ec3b83  #e75480


### CIEV

datos98102 <- datos %>% 
  filter(codigo_materia == "98102") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("CIEV") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Bioestadistica I

datos98103 <- datos %>% 
  filter(codigo_materia == "98103") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Bioestadística I") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Ingles tecnico I

datos98104 <- datos %>% 
  filter(codigo_materia == "98104") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Inglés técnico I") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Anatomia normal

datos98105 <- datos %>% 
  filter(codigo_materia == "98105") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Anatomía normal") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Hist y embriologia del desarrollo

datos98106 <- datos %>% 
  filter(codigo_materia == "98106") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Hist. y embriología del desarrollo") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Genetica general

datos98107 <- datos %>% 
  filter(codigo_materia == "98107") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Genética general") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Ingles tecnico II

datos98108 <- datos %>% 
  filter(codigo_materia == "98108") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Inglés técnico II") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 


### Fisiologia

datos98201 <- datos %>% 
  filter(codigo_materia == "98201") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  mi_tema +
  labs (x = "año",
        y = "cantidad de examenes rendidos por año") +
  ggtitle("Fisiología") + 
  scale_color_manual(values = c("#50c878", "#0d98ba", "#e52b50")) +
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 










# datos$materia[datos$codigo_materia == "98201"]

# Render Dashboard --------------------------------------------------------

rmarkdown::render(
  input = here("./03_resultados/informe_rendimientos_2015_2019.Rmd"),
  output_format = "pdf_document",
  output_dir = here("./03_resultados/"),
  run_pandoc = TRUE,
  output_file = paste0("informe_rendimientos_2015_2019.pdf")
)

