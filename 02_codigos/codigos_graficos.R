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

# tema del grafico --------------------------------------------------------

#genero un tema de graficos
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

# datos por materia de 2015 a 2019 ----------------------------------------

### biologia molecular y celular
g_datos_98101 <- datos %>% 
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
  ggtitle("BMC") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83"))

### CIEV
g_datos_98102 <- datos %>% 
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
g_datos_98103 <- datos %>% 
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
  ggtitle("BIOESTADÍSTICA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Anatomia normal
g_datos_98105 <- datos %>% 
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
  ggtitle("ANATOMÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Hist y embriologia del desarrollo
g_datos_98106 <- datos %>% 
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
  ggtitle("HISTOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Genetica general
g_datos_98107 <- datos %>% 
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
  ggtitle("GENÉTICA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Fisiologia
g_datos_98201 <- datos %>% 
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
  ggtitle("FISIOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Microbiología
g_datos_98202 <- datos %>% 
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
  ggtitle("MICROBIOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Inmunologia basica
g_datos_98203 <- datos %>% 
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
  ggtitle("INMUNOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Zootecnia (tecnologia)
g_datos_98204 <- datos %>% 
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
  ggtitle("ZOOTECNIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia funcional y morfologia
g_datos_98205 <- datos %>% 
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
  ggtitle("PATOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Semiologia
g_datos_98206 <- datos %>% 
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
  ggtitle("SEMIOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Farmacologia basica
g_datos_98207 <- datos %>% 
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
  ggtitle("FARMACOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Nutricion
g_datos_98208 <- datos %>% 
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
  ggtitle("NUTRICIÓN") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Enfermedades infecciosas
g_datos_98301 <- datos %>% 
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
  ggtitle("ENFERMEDADES INFECCIOSAS") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Parasitologia y enfermedades parasitarias
g_datos_98302 <- datos %>% 
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
  ggtitle("PARASITOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Toxicologia y enfermedades toxicologicas
g_datos_98303 <- datos %>% 
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
  ggtitle("TOXICOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Medicina preventiva y epidemiología
g_datos_98304 <- datos %>% 
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
  ggtitle("EPIDEMIOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia y clinica de peq animales
g_datos_98305 <- datos %>% 
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
  ggtitle("PATOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Patologia y clinica de equinos I
g_datos_98306 <- datos %>% 
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
  ggtitle("PATOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Patología y clinica rumiantes y suinos I
g_datos_98307 <- datos %>% 
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
  ggtitle("CLÍNICA DE RUMIANTES") + 
  scale_fill_manual(values = c("#85bb65", "#ec3b83")) 

### Teriogenologia I
g_datos_98308 <- datos %>% 
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
  ggtitle("TERIOGENOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Tecnica quirurgica
g_datos_98309 <- datos %>% 
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
  ggtitle("TÉCNICA QUIRÚRGICA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion sanitaria
g_datos_98310 <- datos %>% 
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
  ggtitle("LEGISLACIÓN SANITARIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Produccion de rumiantes I
g_datos_98401 <- datos %>% 
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
  ggtitle("PRODUCCIÓN DE RUMIANTES") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Prod de suinos y animales de granja I
g_datos_98402 <- datos %>% 
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
  ggtitle("PRODUCCCIÓN DE SUINOS") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Alimentacion
g_datos_98403 <- datos %>% 
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
  ggtitle("ALIMENTACIÓN") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Economía y administración agropecuaria
g_datos_98404 <- datos %>% 
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
  ggtitle("ECONOMÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Mejora genetica
g_datos_98405 <- datos %>% 
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
  ggtitle("MEJORA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion agraria
g_datos_98406 <- datos %>% 
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
  ggtitle("LEGISLACIÓN AGRARIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Patologia, clinica y produccion avicola
g_datos_98407 <- datos %>% 
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
  ggtitle("PATOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Higiene, inspeccion-control de alimentos de origen animal
g_datos_98501 <- datos %>% 
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
  ggtitle("HIGIENE") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Ciencia y tecnologia de alimentos de origen animal
g_datos_98502 <- datos %>% 
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
  ggtitle("CIENCIA Y TECNOLOGÍA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Salud publica veterinaria
g_datos_98503 <- datos %>% 
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
  ggtitle("SALUD PÚBLICA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 

### Legislacion alimentaria y ambiental
g_datos_98504 <- datos %>% 
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
  ggtitle("LEGISLACIÓN ALIMENTARIA") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) 









# primero, rendimientos en funcion de los años agrupado por materia
g_datos_981_m <- datos %>% 
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
  ggtitle("RENDIMIENTOS EN FUNCION DE LOS AÑOS PARA LAS MATERIAS DE PRIMERO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 2) +
  theme(strip.text.y = element_text(angle = -90))

#50c878  #85bb65 #00a86b  #29ab87
#0d98ba  #26619c  #0077be
#e52b50  #ec3b83  #e75480

# segundo, rendimientos en funcion de los años agrupado por materia
g_datos_982_m <- datos %>% 
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
  ggtitle("RENDIMIENTOS EN FUNCION DE LOS AÑOS PARA LAS MATERIAS DE SEGUNDO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 2) +
  theme(strip.text.y = element_text(angle = -90))

# tercero, rendimientos en funcion de los años agrupado por materia
g_datos_983_m <- datos %>% 
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
  ggtitle("RENDIMIENTOS EN FUNCION DE LOS AÑOS PARA LAS MATERIAS DE TERCERO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 2) +
  theme(strip.text.y = element_text(angle = -90))

# cuarto, rendimientos en funcion de los años agrupado por materia
g_datos_984_m <- datos %>% 
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
  ggtitle("RENDIMIENTOS EN FUNCION DE LOS AÑOS PARA LAS MATERIAS DE CUARTO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 2) +
  theme(strip.text.y = element_text(angle = -90))

# quito, rendimientos en funcion de los años agrupado por materia
g_datos_985_m <- datos %>% 
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
  ggtitle("RENDIMIENTOS EN FUNCION DE LOS AÑOS PARA LAS MATERIAS DE QUINTO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(materia ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# datos por ano lectivo y ano curricular ------------------------------------------------

# primero, rendimientos en funcion de las materias agrupado por anos
g_datos_981_a <- datos %>% 
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
  scale_x_discrete(labels = c("BMC",
                              "CIEV",
                              "BIOESTADISTICA",
                              "ANATOMIA",
                              "HISTOLOGIA",
                              "GENETICA")) +
  ggtitle("MATERIAS DE PRIMER AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# segundo, rendimientos en funcion de las materias agrupado por anos
g_datos_982_a <- datos %>% 
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
  scale_x_discrete(labels = c("FISIOLOGIA",
                              "MICROBIOLOGIA",
                              "INMUNOLOGIA",
                              "ZOOTECNIA",
                              "PATOLOGIA",
                              "SEMIOLOGIA",
                              "FARMACOLOGIA",
                              "NUTRICION")) +
  ggtitle("MATERIAS DE SEGUNDO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# tercero, rendimientos en funcion de las materias agrupado por anos
g_datos_983_a <- datos %>% 
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
  scale_x_discrete(labels = c("INFECTO",
                              "PARASITOLOGIA",
                              "TOXICOLOGIA",
                              "EPIDEMIOLOGIA",
                              "CL. PEQUEÑOS",
                              "CL. EQUINOS",
                              "CL. RUMIANTES",
                              "TERIOLOGIA",
                              "TEC. QUIRURGICA",
                              "LEG. SANITARIA")) +
  ggtitle("MATERIAS DE TERCER AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# cuarto, rendimientos en funcion de las materias agrupado por anos
g_datos_984_a <- datos %>% 
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
  scale_x_discrete(labels = c("PROD. RUMIANTES",
                              "PROD SUINOS",
                              "ALIMENTACION",
                              "ECONOMIA",
                              "MEJORA",
                              "LEG. AGRARIA",
                              "PATOLOGIA")) +
  ggtitle("MATERIAS DE CUARTO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

# quinto, rendimientos en funcion de las materias agrupado por anos
g_datos_985_a <- datos %>% 
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
  scale_x_discrete(labels = c("HIGIENE",
                              "CYT",
                              "SALUD PUBLICA",
                              "LEG. ALIMENTARIA")) +
  ggtitle("MATERIAS DE QUINTO AÑO") + 
  scale_fill_manual(values = c("#85bb65", "#26619c", "#ec3b83")) +
  facet_wrap(ano ~ ., ncol = 1) +
  theme(strip.text.y = element_text(angle = -90))

