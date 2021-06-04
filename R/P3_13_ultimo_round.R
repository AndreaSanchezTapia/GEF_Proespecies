library(readxl)
library(readr)
library(dplyr)

#
#le pr
PRHL <- read_xlsx("data/dados_formatados/p3/patricia/ATUALIZADA - Base Produto 3 - 03-06 - sem SIG-Para Haroldo e PR.xlsx")
names(PRHL)
final <- PRHL %>%
  rename(id = ID) %>%
  dplyr::select(-any_of(cols_leo)) %>%
  left_join(leo)
ncol(final)
final <- final %>% dplyr::select(all_of(nomes_final))
write_csv(final, "output/p3/p3_final_04Jun.csv")
#cruza com mpos IBGE
final_sf <- read_sf("output/p3/p3_final_04Jun.csv",
             options = c("X_POSSIBLE_NAMES=long_consolidada", "Y_POSSIBLE_NAMES=lat_consolidada"))
mposIBGE <- mpos %>% dplyr::select(NM_MUN)
st_crs(mposIBGE)
final_sf <- st_set_crs(final_sf, st_crs(mposIBGE))
final_sf <- st_join(final_sf, mposIBGE)
final_sf <- final_sf %>% mutate(conflito_municipios = if_else(municipio_consolidado == NM_MUN, "OK", "conflito"))
 count(conflito_municipios)
max(final_sf$lat_consolidada, na.rm = T)
max(coord_original$lat_original, na.rm = T)
max(coord_original$long_original, na.rm = T)
min(coord_original$long_original, na.rm = T)
max(leo$long_corrigida, na.rm = T)
max(leo$lat_corrigida, na.rm = T)
min(leo$lat_corrigida, na.rm = T)
min(leo$long_corrigida, na.rm = T)
min(coord_original$long_original, na.rm = T)
min(coord_original$lat_original, na.rm = T)
min(final_sf$lat_consolidada, na.rm = T)
count(final_sf, NM_MUN) %>% arrange(desc(n))
count(final_sf,municipio_consolidado ==  NM_MUN)
count(final_sf, municipio_consolidado)%>% arrange(desc(n))
count(final, municipio_consolidado)%>% arrange(desc(n))
count(final_sf, NM_MUN) %>% arrange(desc(n))
setdiff(names(final), names(final_sf))
setdiff(nomes_final, names(final))
setdiff(names(final), nomes_final)
count(final_sf, municipio_consolidado ==  NM_MUN, coordenada_original_boa)
count(final_sf, municipio_consolidado ==  NM_MUN, situacao_coordenada, coordenada_original_boa)

final_sf %>% sf::st_drop_geometry() %>% write_csv("output/p3/p3_4JUN.csv")
