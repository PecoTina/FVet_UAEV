# !diagnostics off
library(here) 

# datos -------------------------------------------------------------------

# cargo los datos con los cursos obligatorios
source(here("./02_codigos/cargando_datos.R"))

# cargo los graficos
source(here("./02_codigos/codigos_graficos.R"))

# cargo las tablas
source(here("./02_codigos/codigos_tablas.R"))







# Render Dashboard --------------------------------------------------------

rmarkdown::render(
  input = here("./03_resultados/informe_rendimientos_2015_2019.Rmd"),
  output_format = "pdf_document",
  output_dir = here("./03_resultados/"),
  run_pandoc = TRUE,
  output_file = paste0("informe_rendimientos_2015_2019.pdf")
)

