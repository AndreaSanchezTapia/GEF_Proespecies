#centroide shape
library(sf)
distritos_shp <- read_sf("data/dados_crus/distritos/LAYER_DISTRITO/DEINFO_DISTRITO.shp")
distritos_c <- sf::st_point_on_surface(distritos_shp)
write_sf(distritos_c, "output/p4/distritos_centroide.kml")
write_sf(distritos_c, "output/p4/distritos_centroide.shp"
write_sf(distritos_c, "output/p4/distritos_centroide.dbf")
distritos_c %>% View()
st_crs(distritos_shp)
st_transform(distritos_c, 4326) %>%  write_sf("output/p4/distritos_centroide.shp")
st_transform(distritos_c, 4326) %>% View()
