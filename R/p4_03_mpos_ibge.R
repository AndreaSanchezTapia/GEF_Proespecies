library(dplyr)
library(readxl)
#googlesheets4::gs4_browse(ss = )
#dados_p4 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1qkaAzzlEM0XlryckvcKFIg37QY1Ng0U-/edit#gid=1564349686", sheet = 3)
source("R/functions.R")
registros <- read_excel("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx"
                    , sheet = 1)

write_csv(registros, "output/p4/registros.csv")

csv <- "output/p4/registros.csv"
#cruza_mpos_ibge <- function(csv) {
  final_sf <<- read_sf(csv,
                      options = c("X_POSSIBLE_NAMES=long_corrigida", "Y_POSSIBLE_NAMES=lat_corrigida"))
  mposIBGE <<- mpos %>% dplyr::select(NM_MUN)
  st_crs(mposIBGE)
  final_sf <- st_set_crs(final_sf, st_crs(mposIBGE))
  final_sf <- st_join(final_sf, mposIBGE)
  final_sf <- final_sf %>% mutate(conflito_municipios_new_new_new = if_else(municipio == NM_MUN.y...83, "OK", "conflito"))
#}
  names(final_sf)
names(final_sf)
#p4_mpos <- cruza_mpos_ibge(csv)
p4_mpos <- final_sf
count(p4_mpos, conflito_municipios_new_new_new)
p4_mpos <- p4_mpos %>%
  select(
  -NM_MUN.x,
  -conflito_municipios_new,
  -conflito_municipios_new_new,
  -NM_MUN.y...78) %>%
  rename(NM_MUN = NM_MUN.y...83,
         conflito_municipios = -conflito_municipios_new_new_new,) %>%

  sf::st_drop_geometry()

names(p4_mpos)

write_csv(p4_mpos, "output/p4/p4_base.csv")
