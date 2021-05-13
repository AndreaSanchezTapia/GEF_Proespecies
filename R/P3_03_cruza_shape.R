#jioin municipios
p3_corr_sf <-  read_sf2("output/p3/p3_municipio_localidade_match_coletores.csv", column = "ellos")
mpos <- read_sf("data/dados_crus/BR_Municipios_2020/BR_Municipios_2020.shp")
mpos <- clean_string(mpos, NM_MUN)
st_crs(mpos)
st_crs(p3_corr_sf)
p3_corr_sf <- st_set_crs(p3_corr_sf, st_crs(mpos))
st_crs(p3_corr_sf)
join_mpos <- st_join(p3_corr_sf, mpos)

cbind(join_mpos$mpo_check.x, join_mpos$mpo_check.y) %>% View()
count(join_mpos, mpo_check.x == mpo_check.y)
count(join_mpos, is.na(mpo_check.x), is.na(mpo_check.y))


count(join_mpos, mpo_county, municipio_localidade, municipio_padronizado, municipio_padronizado2, NM_MUN, SIGLA_UF) %>% View()
count(join_mpos, SIGLA_UF == "SP")
count(join_mpos, SIGLA_UF)
count(join_mpos, SIGLA_UF)

plot(join_mpos$geometry)

