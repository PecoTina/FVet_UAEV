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

# datos_2019 <- datos %>% 
#   filter(str_detect(anho, "2019"))
# 
# datos_1 <- datos %>% 
#   filter(str_detect(codigo_materia, "981"))

# datos por materia de 2015 a 2019 ----------------------------------------

### biologia molecular y celular

datos98101 <- datos %>% 
  filter(codigo_materia == "98101") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(aes(label = n), position = position_dodge(width=0.9), vjust=-0.25) +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Bio. Molecular y Celular") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83"))

#50c878  #85bb65 #00a86b  #29ab87
#0d98ba  #26619c  #0077be
#e52b50  #ec3b83  #e75480

### CIEV

datos98102 <- datos %>% 
  filter(codigo_materia == "98102") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("CIEV") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Bioestadistica I

datos98103 <- datos %>% 
  filter(codigo_materia == "98103") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Bioestadística I") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Ingles tecnico I

datos98104 <- datos %>% 
  filter(codigo_materia == "98104") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Inglés técnico I") + 
  scale_fill_manual(values = c("#26619c")) 

### Anatomia normal

datos98105 <- datos %>% 
  filter(codigo_materia == "98105") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Anatomía normal") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Hist y embriologia del desarrollo

datos98106 <- datos %>% 
  filter(codigo_materia == "98106") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Hist. y embriología del desarrollo") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Genetica general

datos98107 <- datos %>% 
  filter(codigo_materia == "98107") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Genética general") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Ingles tecnico II

datos98108 <- datos %>% 
  filter(codigo_materia == "98108") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Inglés técnico II") + 
  scale_color_manual(values = c("#0d98ba")) +
  scale_fill_manual(values = c("#26619c")) 

### Fisiologia

datos98201 <- datos %>% 
  filter(codigo_materia == "98201") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Fisiología") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Microbiología

datos98202 <- datos %>% 
  filter(codigo_materia == "98202") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Microbiología") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Inmunologia basica

datos98203 <- datos %>% 
  filter(codigo_materia == "98203") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Inmunología básica") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Zootecnia (tecnologia)

datos98204 <- datos %>% 
  filter(codigo_materia == "98204") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Zootecnia (Tecnología)") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia funcional y morfologia

datos98205 <- datos %>% 
  filter(codigo_materia == "98205") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Patología funcional y morfología)") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Sefmiologia

datos98206 <- datos %>% 
  filter(codigo_materia == "98206") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Semiología") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Farmacologia basica

datos98207 <- datos %>% 
  filter(codigo_materia == "98207") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Farmacología básica") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Nutricion

datos98208 <- datos %>% 
  filter(codigo_materia == "98208") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Nutrición") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Enfermedades infecciosas

datos98301 <- datos %>% 
  filter(codigo_materia == "98301") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Enfermedades infecciosas") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Parasitologia y enfermedades parasitarias

datos98302 <- datos %>% 
  filter(codigo_materia == "98302") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Parasitología y enfermedades parasitarias") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Toxicologia y enfermedades toxicologicas

datos98303 <- datos %>% 
  filter(codigo_materia == "98303") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Toxicología y enfermedades toxicológicas") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Medicina preventiva y epidemiología

datos98304 <- datos %>% 
  filter(codigo_materia == "98304") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Medicina preventiva y epidemiología") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia y clinica de peq animales

datos98305 <- datos %>% 
  filter(codigo_materia == "98305") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Patología y clínica de pequeños animales") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Patologia y clinica de equinos I

datos98306 <- datos %>% 
  filter(codigo_materia == "98306") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Patología y clínica de equinos I") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Parología y clinica rumiantes y suinos I

datos98307 <- datos %>% 
  filter(codigo_materia == "98307") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Patología y clínica rumiantes y suinos I") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Teriogenologia I

datos98308 <- datos %>% 
  filter(codigo_materia == "98308") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Teriogenología") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Tecnica quirurgica

datos98309 <- datos %>% 
  filter(codigo_materia == "98309") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Técnica quirúrgica") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion sanitaria

datos98310 <- datos %>% 
  filter(codigo_materia == "98310") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Legislación sanitaria") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Produccion de rumiantes I

datos98401 <- datos %>% 
  filter(codigo_materia == "98401") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Producción de rumiantes I") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Prod de suinos y animales de granja I

datos98402 <- datos %>% 
  filter(codigo_materia == "98402") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Producción de suinos y animales de granja I") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Alimentacion

datos98403 <- datos %>% 
  filter(codigo_materia == "98403") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Alimentación") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Economía y administración agropecuaria

datos98404 <- datos %>% 
  filter(codigo_materia == "98404") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Economía y administración agropecuaria") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Mejora genetica

datos98405 <- datos %>% 
  filter(codigo_materia == "98405") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Mejora genética") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion agraria

datos98406 <- datos %>% 
  filter(codigo_materia == "98406") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Lesgilación agraria") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia, clinica y produccion avicola

datos98407 <- datos %>% 
  filter(codigo_materia == "98407") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Patología, clínica y producción avícola") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Higiene, inspeccion-control de alimentos de origen animal

datos98501 <- datos %>% 
  filter(codigo_materia == "98501") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Higiene, inspección-control de alimentos de origen animal") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Ciencia y tecnologia de alimentos de origen animal

datos98502 <- datos %>% 
  filter(codigo_materia == "98502") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Ciencia y tecnología de alimentos de origen animal") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Salud publica veterinaria

datos98503 <- datos %>% 
  filter(codigo_materia == "98503") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100.1)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Salud pública veterinaria") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion alimentaria y ambiental

datos98504 <- datos %>% 
  filter(codigo_materia == "98504") %>% 
  group_by(ano, resultado) %>% 
  summarise(n = n()) %>%
  mutate(porc = (n / sum(n) *100)) %>% 
  ggplot(aes(x = ano, y = n, fill = resultado)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label= paste(round(porc), "%")), 
            position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
  mi_tema +
  theme(legend.title = element_text()) +
  labs (x = "año",
        y = "resultados del curso por año") +
  ggtitle("Legislación alimentaria y ambiental") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 



# datos por ano lectivo y ano curricular ------------------------------------------------

# datos_981_2015 <- datos %>% 
#   filter(ano == "2015" & str_detect(codigo_materia, "981")) %>%
#   group_by(materia, resultado) %>% 
#   summarise(n = n()) %>%
#   mutate(porc = (n / sum(n) *100)) %>% 
#   ggplot(aes(x = materia, y = n, fill = resultado)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label= paste(round(porc), "%")), 
#             position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
#   mi_tema +
#   theme(legend.position = "none",
#         axis.title.x = element_blank()) +
#   labs (y = "resultados del curso por año") +
#   ggtitle("2015, MATERIAS DE PRIMER AÑO") + 
#   scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
#   scale_x_discrete(breaks = NULL, labels = NULL)
# 
# datos_981_2016 <- datos %>% 
#   filter(ano == "2016" & str_detect(codigo_materia, "981")) %>%
#   group_by(materia, resultado) %>% 
#   summarise(n = n()) %>%
#   mutate(porc = (n / sum(n) *100)) %>% 
#   ggplot(aes(x = materia, y = n, fill = resultado)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label= paste(round(porc), "%")), 
#             position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
#   mi_tema +
#   theme(legend.position = "none",
#         axis.title.x = element_blank()) +
#   labs (y = "resultados del curso por año") +
#   ggtitle("2016") + 
#   scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
#   scale_x_discrete(breaks = NULL, labels = NULL)
# 
# datos_981_2017 <- datos %>% 
#   filter(ano == "2017" & str_detect(codigo_materia, "981")) %>%
#   group_by(materia, resultado) %>% 
#   summarise(n = n()) %>%
#   mutate(porc = (n / sum(n) *100)) %>% 
#   ggplot(aes(x = materia, y = n, fill = resultado)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label= paste(round(porc), "%")), 
#             position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
#   mi_tema +
#   theme(legend.position = "none",
#         axis.title.x = element_blank()) +
#   labs (y = "resultados del curso por año") +
#   ggtitle("2017") + 
#   scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
#   scale_x_discrete(breaks = NULL, labels = NULL)
# 
# datos_981_2018 <- datos %>% 
#   filter(ano == "2018" & str_detect(codigo_materia, "981")) %>%
#   group_by(materia, resultado) %>% 
#   summarise(n = n()) %>%
#   mutate(porc = (n / sum(n) *100)) %>% 
#   ggplot(aes(x = materia, y = n, fill = resultado)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label= paste(round(porc), "%")), 
#             position = position_dodge(width = 0.9), vjust=-0.25, size = 2.5) +
#   mi_tema +
#   theme(legend.position = "none",
#         axis.title.x = element_blank()) +
#   labs (y = "resultados del curso por año") +
#   ggtitle("2018") + 
#   scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
#   scale_x_discrete(breaks = NULL, labels = NULL)

datos_981 <- datos %>% 
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




# datos$materia[datos$codigo_materia == "98505"]

# Render Dashboard --------------------------------------------------------

rmarkdown::render(
  input = here("./03_resultados/informe_rendimientos_2015_2019.Rmd"),
  output_format = "pdf_document",
  output_dir = here("./03_resultados/"),
  run_pandoc = TRUE,
  output_file = paste0("informe_rendimientos_2015_2019.pdf")
)

