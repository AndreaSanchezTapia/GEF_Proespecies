library(dplyr)
anexo2 <- readxl::read_xlsx("output/p3/Anexo2_Produto 3 - v.2-14-06-21hs-1.xlsx", skip = 1)
anexo2$lat_original <- as.numeric(anexo2$lat_original)
anexo2$long_original <- as.numeric(anexo2$long_original)
anexo2$long_corrigida <- as.numeric(anexo2$long_corrigida)
anexo2$lat_corrigida <- as.numeric(anexo2$lat_corrigida)

anexo2 %>% count(
  is.na(lat_original) & is.na(long_original),
  is.na(lat_corrigida) & is.na(long_corrigida),
  lat_original == lat_corrigida,
  long_original == long_corrigida,
  situacao_coordenada)#9229  cosr ok, CO 112 ok


anexo2 <- anexo2 %>% mutate(situacao_coordenada = if_else(
  is.na(lat_original) & !is.na(lat_corrigida), "CR", situacao_coordenada)
)
anexo2 <- anexo2 %>% mutate(check_situacao = if_else(
  long_original == long_corrigida & lat_original == lat_corrigida & situacao_coordenada == "COSR", "isto estava marcado COSR mas tinha coordenadas originais e corrigidas iguais", NA_character_)) %>%
  mutate(situacao_coordenada = if_else(
    long_original == long_corrigida &
    lat_original == lat_corrigida &
    situacao_coordenada == "COSR", "CO", situacao_coordenada))


anexo2 <- anexo2 %>%
  mutate(
    long_corrigida = if_else(
    situacao_coordenada == "CO",long_original,long_corrigida),
    lat_corrigida = if_else(
    situacao_coordenada == "CO",lat_original,lat_corrigida),
    )
anexo2 %>% count(is.na(lat_original), is.na(lat_corrigida), lat_original == lat_corrigida, situacao_coordenada)

sum(is.na(anexo2$lat_corrigida))
sum(is.na(anexo2$long_corrigida))
names(anexo2)
library(readxl)
readr::write_csv(anexo2, "output/p3/anexo2_14jun.csv")

library(sf)
anexo3 <- anexo2[complete.cases(
  anexo2[,c("long_corrigida", "lat_corrigida")]),]
anexo3 <- janitor::clean_names(anexo3, case = "all_caps" )
names(anexo3)
shape <-  st_as_sf(anexo3, coords = c("long_corrigida", "lat_corrigida"))
stringr::str_trunc(names(shape), 10, side = c("right", "left", "center"), ellipsis = "")
names(shape)
shape %>% select(id) %>%
write_sf( "output/p3/anexo2_shapefile.shp")
