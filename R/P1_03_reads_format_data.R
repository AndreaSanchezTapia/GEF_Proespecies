# carrega pacotes ----
library(readxl)
library(dplyr)
library(tidyr)
library(flora)
library(readr)
#remotes::install_github("liibre/Rocc")
library(Rocc)
library(stringr)
library(textclean)
library(janitor)

# campos do produto 1 ----
campos_p1 <- c("grupo", "familia", "genero", "epiteto_especifico", "especie", "sinonimia", "fonte_sinonimia", "cat_ameaca_iucn", "cat_ameaca_br", "cat_ameaca_cncflora", "cat_ameaca_sp")

# 1. ler IUCN_BR_SP ----
IUCN_CNCFlora_BR_SP <- read_xlsx("data/dados_crus/Flora_EstadoSP_Listas_IUCN_CNCFlora_BR_SP.xlsx")

# rename columns ----
IUCN_CNCFlora_BR_SP <- clean_names(IUCN_CNCFlora_BR_SP)
names(IUCN_CNCFlora_BR_SP)# tem campos demais
5
# manter categorias anteriores ----
#vou revisar so no final
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  rename(cat_ameaca_br_old = br_oficial_publicado_em_portaria_pelo_mma,
         cat_ameaca_cncflora_old = cnc_flora_atual_quando_diferente_da_coluna_anterior,
         cat_ameaca_sp_old = sp)
IUCN_CNCFlora_BR_SP %>%
  select(starts_with("cat_ameaca")) %>%
  count(cat_ameaca_br_old, cat_ameaca_sp_old, cat_ameaca_cncflora_old) %>% View()
# checa se tem outros clados
IUCN_CNCFlora_BR_SP %>% count(reino)#only plants

# cria coluna especie sera a unica que usaremos ----
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  unite(col = "especie", genero, epiteto_especifico, remove = F, sep = " ") %>%
  #seleciona campos e as categorias antigas de ameaca
  select(any_of(campos_p1), starts_with("cat_ameaca"))
names(IUCN_CNCFlora_BR_SP)
# salva ----
write_csv(IUCN_CNCFlora_BR_SP, fs::path(output, "IUCN_CNCFlora_BR_SP_format", ext = "csv"), na = "s.i.")

## 2. le Sao Paulo ----
SP <- read_xlsx("data/dados_crus/Flora_Ameacada_SPOficial.xlsx")
SP <- janitor::clean_names(SP)
SP$familia <- stringr::str_to_sentence(SP$familia)
SP <- SP %>% rename(especie_autor = especie)
SP <- SP %>% rename(cat_ameaca_sp = status_de_conservacao)
SP

#alem de author removal tem que ver o resto do string
rocc_check1 <- check_string(SP$especie_autor) #com autor
rocc_check2 <- check_string(rocc_check1$species)
rocc_check <- cbind(rocc_check1, rocc_check2)
rocc_check <- janitor::clean_names(rocc_check)

names(rocc_check)
count(rocc_check, species_status, species_status_2)

rocc_check %>% filter(species_status != "name_w_authors") %>% View()
#usa verbatim
rocc_check <- rocc_check %>%
  mutate(especie = case_when(
  species_status %in% c("subspecies", "variety", "forma") ~ verbatim_species,
  species_status_2 %in% c("possibly_ok") & species_status %in% c("name_w_authors") ~ species_2))

# modificar a mao os que faltam
l <- length(rocc_check$especie[rocc_check$species_status != "name_w_authors"])
for (i in seq_along(1:l)) {
rocc_check$especie[rocc_check$species_status != "name_w_authors"][i] <- remove.authors(rocc_check$especie[rocc_check$species_status != "name_w_authors"][i])
}
SP$especie <- rocc_check$especie
# seleciona os nomes
SP <- SP %>% select(familia, especie_autor, especie, cat_ameaca_sp)
write_csv(SP,fs::path(output, "SP_Oficial_format", ext = "csv"), na = "s.i.")
#blz


## CR_Lacuna
CR_Lac <- readxl::read_xlsx("data/DADOS/CR_Lacuna_ProEspecies_Originais_Territorio20.xlsx")
CR_Lac <- clean_names(CR_Lac)
CR_Lac <- CR_Lac %>% filter(grupao == "Flora") %>%
  rename(especie = especie_simplificado,
         cat_ameaca_CR_lac = categoria)
names(CR_Lac)
write_csv(CR_Lac,fs::path(output, "CR_Lac_format", ext = "csv"), na = "s.i.")
