library(tidyverse)
library(readr)
# checks with flora
output <- "data/dados_formatados"
UICN_BR <- read_csv("data/dados_formatados/UICN_BR.csv", na = "s.i.", guess_max = 10000)
flora_IUCN <- get.taxa(UICN_BR$especie,
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
readr::write_csv(flora_IUCN, file = fs::path(output, "UICN_BR_flora", ext = "csv"), na = "s.i.")
flora_IUCN <- readr::read_csv(file = fs::path(output, "UICN_BR_flora", ext = "csv"), na = "s.i.", guess_max = 10000)

flora_IUCN$especie <- flora_IUCN$original.search
flora_IUCN %>% count(taxon.status, notes)
flora_IUCN %>% count(notes)

#checar se algum dos nomes que ficou para atras ainda e planta
flora_IUCN %>% filter(notes == "not found") %>% View() #e mas nao tem jeito
flora_IUCN %>% count(notes == "not found") %>% View() #e mas nao tem jeito

UICN_BR_flora <- full_join(UICN_BR, flora_IUCN)

UICN_Flora_SP <- UICN_BR_flora %>%
  filter(!is.na(occurrence)) %>%
  filter(str_detect(occurrence, pattern = "SP"))
UICN_Flora_SP %>% count(cat_ameaca_iucn, threat.status) %>% View()
UICN_Flora_SP %>% count(cat_ameaca_iucn == threat.status) %>% View() #isto e uma vergonha
UICN_Flora_SP %>% count(cat_ameaca_iucn, notes, taxon.status) %>% View()
UICN_Flora_SP %>% count(notes) %>% View()

synonyms_from_UICN <- UICN_Flora_SP %>% select(especie, original.search, search.str, scientific.name, notes, taxon.status, cat_ameaca_iucn, threat.status) %>% filter(notes != "")
synonyms_but_in_list <- synonyms_from_UICN %>% filter(search.str %in% UICN_BR$especie) %>% select(search.str) %>% rename(especie = search.str)
synonyms_but_in_list_save <- left_join(synonyms_but_in_list, UICN_BR_flora) %>% filter(str_detect(occurrence, pattern = "SP"))
View(synonyms_but_in_list_save)
all(synonyms_but_in_list_save$especie %in% UICN_Flora_SP$search.str)
#todos esos sinonimos si entraron

mandar <- synonyms_from_UICN %>%
  filter(!search.str %in% UICN_BR$especie) %>%
  filter(especie != "Amazona brasiliensis") %>%
  #filter(especie != "Amazona brasiliensis") %>%
  rename(especie_iucn = especie,
                              nome_correto_flora = search.str) %>%
  select(especie_iucn, nome_correto_flora, scientific.name, notes, taxon.status, cat_ameaca_iucn, threat.status)
write_csv(mandar, "output/01_UICN_sinonimizados_por_FB2020.csv")

comecar_aqui <- bind_rows(mandar[mandar$especie_iucn %in% lista_unificada$especie,],
          mandar[mandar$nome_correto_flora %in% lista_unificada$especie,]) %>% distinct()
write_csv(comecar_aqui, "output/01_UICN_sinonimizados_por_FB2020_na_lista.csv")


dim(UICN_BR)
dim(UICN_Flora_SP)

names(UICN_Flora_SP)

head(UICN_Flora_SP$original.search)
UICN_Flora_SP %>% select(scientific.name, original.search, search.str) %>% View()
count(UICN_Flora_SP, notes, taxon.status)
UICN_Flora_SP %>% filter(notes != "") %>% select(original.search, search.str, scientific.name) %>% View()
all_names_begin <- str_detect(string = UICN_Flora_SP$scientific.name, pattern = UICN_Flora_SP$search.str)
which(all_names_begin ==F)
UICN_Flora_SP %>% filter(all_names_begin ==F) %>% View()
UICN_Flora_SP %>% rename(original_uicn_name = original.search,
                         especie = search.str) %>%
  select(original_uicn_name, especie) %>% write_csv(fs::path(output, "UICN_SP_flora_names_only.csv"))
readr::write_csv(UICN_Flora_SP, file = fs::path(output, "UICN_SP_flora", ext = "csv"), na = "s.i.")

#A partir da lista de especies da flora do Brasil, buscamos as espécies que se encontram no estado de São Paulo usando o pacote flora de R. Segundo a Flora do Brasil, das `r nrow(UICN_BR)` espécies avaliadas pela UICN para o Brasil, `r nrow(UICN_FLora_SP)` ocorrem no Estado de São Paulo.
#kableExtra::kable(UICN_Flora_SP)


SP_UICN_Flora <- read_csv("./data/dados_formatados/UICN_SP_flora.csv", na = "s.i.")
glimpse(SP_UICN_Flora)
count(SP_UICN_Flora, taxon.status)
count(SP_UICN_Flora, notes)
count(SP_UICN_Flora, threat.status)
names(SP_UICN_Flora)
Resumo <- data.frame(
  Especie_original = SP_UICN_Flora$search.str,
  Nome = SP_UICN_Flora$scientific.name,
  Notas = SP_UICN_Flora$notes)
head(Resumo)

