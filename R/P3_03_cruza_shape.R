source("R/functions.R")
#join municipios
p3_corr_sf <-  read_sf2("output/p3/p3_municipio_localidade_match_coletores_numero.csv", column = "ellos")
dim(p3_corr_sf)
#p3_corr_sf <- st_as_sf(p3_corr,
 #        coords = c("long_original",
  #                  "lat_original"))
#tibble(municipio_localidade = p3_corr_sf$municipio_localidade) %>% readr::write_csv("output/p3/checks_municipio_localidade.csv")
names(p3_corr_sf)

mpos <- read_sf("data/dados_crus/BR_Municipios_2020/BR_Municipios_2020.shp")
st_crs(mpos)
st_crs(p3_corr_sf)
p3_corr_sf <- st_set_crs(p3_corr_sf, st_crs(mpos))
st_crs(p3_corr_sf)
p3_corr_sf <- st_join(p3_corr_sf, mpos)
sum(p3_corr_sf$mpo_check == "NA")
sum(is.na(p3_corr_sf$NM_MUN))
p3_corr_sf %>% count(municipio_padronizado_final, NM_MUN)
p3_corr_sf %>% count(municipio_padronizado_final == "NA", is.na(NM_MUN))#14148 com ambos, 6666 só fura, 1933 só etiqueta, 8931 nenhum
p3_corr_sf %>% count(municipio_padronizado_final == NM_MUN)#11468 iguais, 4613 diferentes, 15597 NA


write_sf(p3_corr_sf, "output/p3/p3_municipio_localidade_match_coletores_numero_SHAPE.csv")

write_sf(p3_corr_sf, "output/p3/p3_municipio_localidade_match_coletores_numero_SHAPE_NEW.csv", delete_dsn = T)
