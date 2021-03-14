library(tidyverse)
library(readr)
# checks with flora
output <- "data/dados_formatados/uicn/"
UICN_BR <- read_csv(fs::path(output, "UICN_BR.csv"), guess_max = 10000)
# flora_IUCN <- get.taxa(UICN_BR$especie_original,
#                        states = TRUE,
#                        suggest.names = T,
#                        replace.synonyms = T,
#                        life.form = T,
#                        habitat = T,
#                        vegetation.type = T,
#                        vernacular = T,
#                        establishment = T,
#                        domain = T,
#                        endemism = T)
#readr::write_csv(flora_IUCN, file = fs::path(output, "UICN_BR_flora", ext = "csv"), na = "s.i.")
flora_IUCN <-
  readr::read_csv(file = fs::path(output, "UICN_BR_flora", ext = "csv"),
                  na = "s.i.", guess_max = 10000)

#rename binomio
flora_IUCN$especie_original <- flora_IUCN$original.search

UICN_BR_flora <- full_join(UICN_BR, flora_IUCN)

# filtra para ocorrencias em SP de acordo com Flora 2020
UICN_Flora_SP <- UICN_BR_flora %>%
  filter(!is.na(occurrence)) %>%
  filter(str_detect(occurrence,
                    pattern = "SP")) %>%
  mutate(fonte = "UICN_API")

UICN_Flora_SP <- UICN_Flora_SP %>%
  select(especie_original, starts_with("cat_ameaca"), fonte)

readr::write_csv(UICN_Flora_SP,
                 file = fs::path(output, "UICN_SP_flora_format", ext = "csv"))

# A partir da lista de especies da flora do Brasil, buscamos as espécies que se encontram no estado de São Paulo usando o pacote flora de R. Segundo a Flora do Brasil, das `r nrow(UICN_BR)` espécies avaliadas pela UICN para o Brasil, `r nrow(UICN_FLora_SP)` ocorrem no Estado de São Paulo.
#kableExtra::kable(UICN_Flora_SP)
