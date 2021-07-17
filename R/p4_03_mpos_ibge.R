library(dplyr)
library(readxl)
#googlesheets4::gs4_browse(ss = )
#dados_p4 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1qkaAzzlEM0XlryckvcKFIg37QY1Ng0U-/edit#gid=1564349686", sheet = 3)
source("R/functions.R")
registros <- read_excel("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx"
                    , sheet =1)
write_csv(registros, "output/p4/registros.csv")

cruza_mpos_ibge <- function(csv) {
  final_sf <- read_sf(csv,
                      options = c("X_POSSIBLE_NAMES=long_corrigida", "Y_POSSIBLE_NAMES=lat_corrigida"))
  mposIBGE <- mpos %>% dplyr::select(NM_MUN)
  st_crs(mposIBGE)
  final_sf <- st_set_crs(final_sf, st_crs(mposIBGE))
  final_sf <- st_join(final_sf, mposIBGE)
  final_sf <- final_sf %>% mutate(conflito_municipios_new = if_else(municipio == NM_MUN, "OK", "conflito"))
}
p4_mpos <- cruza_mpos_ibge("output/p4/registros.csv")
count(p4_mpos, conflito_municipios_new)
p4_mpos %>%
  sf::st_drop_geometry() %>%
write_csv("output/p4/p4_conflitos.csv")
