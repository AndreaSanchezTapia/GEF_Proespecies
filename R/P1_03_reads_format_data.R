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
IUCN_CNCFlora_BR_SP <- read_xlsx("data/dados_crus/Flora_EstadoSP_Listas_IUCN_CNCFlora_BR_SP.xlsx") %>%
  distinct()

# rename columns ----
IUCN_CNCFlora_BR_SP <- clean_names(IUCN_CNCFlora_BR_SP)
names(IUCN_CNCFlora_BR_SP)
check_ws <- function(x) {
  x <- trimws(x, whitespace = "[ \\t\\r\\n\U00A0]")
  x <- gsub("\\t|\\r|\\n|\U00A0", " ", x)
}
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  mutate_all(.funs = check_ws)
# manter categorias anteriores ----
#vou revisar so no final
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  rename(cat_ameaca_br_sima = br_oficial_publicado_em_portaria_pelo_mma,
         cat_ameaca_cncflora_sima = cnc_flora_atual_quando_diferente_da_coluna_anterior,
         cat_ameaca_sp_sima = sp)
unique(IUCN_CNCFlora_BR_SP$cat_ameaca_br_sima)
unique(IUCN_CNCFlora_BR_SP$cat_ameaca_cncflora_sima)
unique(IUCN_CNCFlora_BR_SP$cat_ameaca_sp_sima)

#substitui NA por NE

IUCN_CNCFlora_BR_SP$cat_ameaca_cncflora_sima[IUCN_CNCFlora_BR_SP$cat_ameaca_cncflora_sima == "NA"] <- "NE"
count(IUCN_CNCFlora_BR_SP,cat_ameaca_cncflora_sima)
# checa se tem outros clados
IUCN_CNCFlora_BR_SP %>% count(reino)#only plants

# cria coluna especie sera a unica que usaremos ----
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  mutate(var = if_else(!is.na(var), paste("var.", var), var),
         subsp = if_else(!is.na(subsp), paste("subsp.", subsp), subsp))

IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  unite(col = "especie_original", genero, epiteto_especifico, subsp, var, remove = F, sep = " ",  na.rm = T) %>%
  #seleciona campos e as categorias antigas de ameaca
  select(especie_original, any_of(campos_p1), starts_with("cat_ameaca"))

names(IUCN_CNCFlora_BR_SP)
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>% mutate(fonte = "sima")


IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>% distinct()
# salva ----
output <- "data/dados_formatados"
write_csv(IUCN_CNCFlora_BR_SP, fs::path(output, "IUCN_CNCFlora_BR_SP_format", ext = "csv"))

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
SP$especie_original <- unlist(rocc_check$species_2)

# seleciona os nomes
SP <- SP %>% select(familia, especie_autor, especie_original, cat_ameaca_sp)
SP <- SP %>% mutate(fonte = "SP_oficial")
distinct(SP)
output <- "data/dados_formatados"
write_csv(SP, fs::path(output, "SP_Oficial_format", ext = "csv"))
#checa espacios a posteriori
SP <- read_csv(fs::path(output, "SP_Oficial_format", ext = "csv"))

## CR_Lacuna
CR_Lac <- readxl::read_xlsx("data/dados_crus/CR_Lacuna_ProEspecies_Originais_Territorio20.xlsx")
sp_espacios <- trimws(CR_Lac$especie_original, whitespace = "[ \\t\\r\\n\U00A0]")
sp_espacios <- gsub("\\t|\\r|\\n|\U00A0", " ", sp_espacios)
CR_Lac$especie_original <- sp_espacios
CR_Lac <- clean_names(CR_Lac)
CR_Lac <- CR_Lac %>% filter(grupao == "Flora") %>%
  rename(especie_original = especie_simplificado,
         cat_ameaca_CR_lac = categoria) %>%
  mutate(fonte = "CR_Lac")
names(CR_Lac)
write_csv(CR_Lac,fs::path(output, "CR_Lac_format", ext = "csv"))
