library(readr)
library(dplyr)
library(fs)

# reads IUCN data
UICN_BR <- read_csv("./data/dados_crus/UICN_BR.csv", guess_max = 10000, col_types = "cccccc")
#length(unique(UICN_BR$scientific_name))#tinha um duplicado
#dup <- which(duplicated(UICN_BR$scientific_name))
#dup_sp <- slice(UICN_BR, dup) %>% pull(scientific_name)
#filter(UICN_BR, scientific_name == dup_sp)
UICN_BR <- distinct(UICN_BR)

#Obtivemos a lista de espécies avaliadadas pela UICN usando a API. Em `lubridate::today()`, esta lista continha `r nrow(UICN_BR)` nomes únicos.

# rename para padrao
output <- "data/dados_formatados"
UICN_BR <- UICN_BR %>% rename(especie_original = scientific_name,
                   subespecie = subspecies,
                   cat_ameaca_iucn = category) %>%
  mutate(fonte = "IUCN")
write_csv(UICN_BR, file = fs::path(output, "UICN_BR", ext = "csv"), na = "s.i.")

campos_p1

