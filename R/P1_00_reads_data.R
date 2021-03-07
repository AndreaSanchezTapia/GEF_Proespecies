# carrega pacotes ----
library(readxl)
library(dplyr)
library(tidyr)
library(flora)
library(readr)
# remotes::install_github("liibre/Rocc")
library(Rocc)
library(stringr)

# campos do produto 1 ----
campos_p1 <- c("grupo", "familia", "genero", "epiteto_especifico", "especie", "sinonimia", "fonte_sinonimia", "cat_ameaca_iucn", "cat_ameaca_br", "cat_ameaca_cncflora", "cat_ameaca_sp")

#1. ler IUCN_BR_SP ----
IUCN_CNCFlora_BR_SP <- read_xlsx("data/DADOS/Flora_EstadoSP_Listas_IUCN_CNCFlora_BR_SP.xlsx")

# rename columns ----
format_names <- function(x) {
  x <- tolower(replace_non_ascii(x))
  x <- gsub(x = x, pattern = " ",replacement = "_")
  }
IUCN_CNCFlora_BR_SP <- rename_with(
  .data = IUCN_CNCFlora_BR_SP,
  .fn = format_names,
  .cols = everything()
)
names(IUCN_CNCFlora_BR_SP)# tem campos demais

# manter categorias anteriores ----
#vou revisar so no final
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  rename(cat_ameaca_br_old = `br_oficial_(publicado_em_portaria_pelo_mma)`,
         cat_ameaca_cncflora_old = `cncflora_atual_(quando_diferente_da_coluna_anterior)`,
         cat_ameaca_sp_old = sp)
# checa se tem outros clados
IUCN_CNCFlora_BR_SP %>% count(reino)#only plants

# cria coluna especie sera a unica que usaremos ----
IUCN_CNCFlora_BR_SP <- IUCN_CNCFlora_BR_SP %>%
  unite(col = "especie", genero, epiteto_especifico, remove = F, sep = " ") %>%
  #seleciona campos e as categorias antigas de ameaca
  select(any_of(campos_p1), starts_with("cat_ameaca"))
names(IUCN_CNCFlora_BR_SP)
# salva ----
write_csv(IUCN_CNCFlora_BR_SP, "data/dados_formatados/IUCN_CNCFlora_BR_SP_format.csv")


## 2. le Sao Paulo ----
SP <- read_xlsx("data/DADOS/Flora_Ameacada_SPOficial.xlsx")
head(SP)
SP <- rename_with(
  .data = SP,
  .fn = format_names,
  .cols = everything()
)

SP$familia <- stringr::str_to_sentence(SP$familia)
SP <- SP %>% rename(especie_autor = especie)
SP <- SP %>% rename(cat_ameaca_sp = status_de_conservacao)
SP
#removes authors
#SP$especie <- data.frame(especie = sapply(X = SP$especie_autor,
 #                               FUN = remove.authors))
remove.authors2 <- Vectorize(remove.authors)
SP <- SP %>% mutate(especie = remove.authors2(especie_autor))

library(Rocc)
remotes::install_github("liibre/Rocc", force = T)

#checar essa remocao
noauth <- SP %>% filter(especie_autor == especie) %>% pull(especie)
sum(SP$especie_autor %in% noauth)
SP[SP$especie_autor %in% noauth, "especie"] <- "Lysimachia buxifolia"
SP %>% count(especie_autor != especie)
rocc_check <- Rocc::check_string(SP$especie)
#blz


## CR_Lacuna
CR_Lac <- readxl::read_xlsx("data/DADOS/CR_Lacuna_ProEspecies_Originais_Territorio20.xlsx")
CR_Lac <- CR_Lac %>% filter(Grupão == "Flora")

#Guarda formatados
output <- "data/dados_formatados"
dir.create(output)
write_csv(IUCN_CNCFlora_BR_SP_format, fs::path(output, "IUCN_CNCFlora_BR_SP_format", ext = "csv"))
write_csv(SP,fs::path(output, "SP_Oficial_format", ext = "csv"))
write_csv(CR_Lac,fs::path(output, "CR_Lac_format", ext = "csv"))

