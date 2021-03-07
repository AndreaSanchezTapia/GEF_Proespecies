library(readxl)
library(dplyr)
library(tidyr)
library(flora)
#remotes::install_github("liibre/Rocc")
library(Rocc)

#1. ler todas as fontes de dados----
## IUCN_BR_SP
IUCN_CNCFlora_BR_SP <- read_xlsx("data/DADOS/Flora_EstadoSP_Listas_IUCN_CNCFlora_BR_SP.xlsx")
names(IUCN_CNCFlora_BR_SP)
IUCN_CNCFlora_BR_SP %>% count(Reino)#only plants
IUCN_CNCFlora_BR_SP_format <- IUCN_CNCFlora_BR_SP %>%
  unite(col = "Nome cientifico", Gênero, `Epíteto Específico`, remove = F, sep = " ")
IUCN_CNCFlora_BR_SP_format
## Sao Paulo
SP <- read_xlsx("data/DADOS/Flora_Ameacada_SPOficial.xlsx")
SP_no_auth <- sapply(X = SP$Espécie, FUN = remove.authors)
names_SP_no_auth <- data.frame(SP_no_auth)
SP$names_no_auth <- names_SP_no_auth$SP_no_auth
noauth <- SP %>% filter(Espécie == names_no_auth) %>% pull(Espécie)
SP[SP$names_no_auth %in% noauth,"names_no_auth"] <- "Lysimachia buxifolia"
SP %>% count(Espécie != names_no_auth)
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

