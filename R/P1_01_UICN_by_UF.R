library(readr)
library(dplyr)
library(fs)

# reads IUCN data
UICN_BR <- read_csv("./data/dados_crus/UICN_BR.csv", guess_max = 10000, col_types = "cccccc")
UICN_BR <- distinct(UICN_BR)

#Obtivemos a lista de espécies avaliadadas pela UICN usando a API. Em `lubridate::today()`, esta lista continha `r nrow(UICN_BR)` nomes únicos.

# rename para padrao
output <- "data/dados_formatados/uicn/"
UICN_BR <- UICN_BR %>% rename(especie_original = scientific_name,
                   subespecie = subspecies,
                   cat_ameaca_iucn = category) %>%
  mutate(fonte = "IUCN")
write_csv(UICN_BR, file = fs::path(output, "UICN_BR", ext = "csv"))
