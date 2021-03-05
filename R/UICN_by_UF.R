library(readr)
library(dplyr)
library(flora)
library(stringr)

# reads IUCN data
UICN_BR <- read_csv("./data/UICN_BR.csv", guess_max = 10000, col_types = "cccccc")
#length(unique(UICN_BR$scientific_name))#tinha um duplicado
#dup <- which(duplicated(UICN_BR$scientific_name))
#dup_sp <- slice(UICN_BR, dup) %>% pull(scientific_name)
#filter(UICN_BR, scientific_name == dup_sp)
UICN_BR <- distinct(UICN_BR)

#Obtivemos a lista de espécies avaliadadas pela UICN usando a API. Em `lubridate::today()`, esta lista continha `r nrow(UICN_BR)` nomes únicos.

# checks with flora
flora_IUCN <- get.taxa(UICN_BR$scientific_name,
                       states = TRUE,
                       suggest.names = T,
                       replace.synonyms = T,
                       life.form = T,
                       habitat = T,
                       vegetation.type = T,
                       vernacular = T,
                       establishment = T,
                       domain = T,
                       endemism = T)
readr::write_csv(flora_IUCN, "./data/UICN_BR_flora.csv")

UICN_Flora_SP <- flora_IUCN %>% filter(!is.na(occurrence)) %>%
  filter(str_detect(occurrence, pattern = "SP"))
readr::write_csv(UICN_Flora_SP, "./data/UICN_SP_flora.csv")
    geocode %>% select(sigla, codigo) %>% distinct() %>% right_join(sarampo)
#A partir da lista de especies da flora do Brasil, buscamos as espécies que se encontram no estado de São Paulo usando o pacote flora de R. Segundo a Flora do Brasil, das `r nrow(UICN_BR)` espécies avaliadas pela UICN para o Brasil, `r nrow(UICN_FLora_SP)` ocorrem no Estado de São Paulo.
#kableExtra::kable(UICN_Flora_SP)

