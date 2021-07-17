
#lo que yo corregi de coordenadas trocadas se sobrepuso a la correcicon de leo. sori.
template <- readr::read_csv("output/p3/p3_template.csv", guess_max = 13000)
template$especie
identical(mpo$municipio_corrigido_final, municipios_novos_geral_merge$municipio)
names()
ast$situacao_coordenada
filter(municipio_consolidado, id == 43703)
new <- template %>% left_join(municipio_consolidado) %>% #municipio corrigido
  left_join(ast_coord_original) %>% #coordenadas originais
  left_join(coord_corrigida) %>%
  left_join(sc_final) %>%
  left_join(merge_cob)
new$especie
uc_p_leo$especie

####toca comparar con el que le mandé
mande <- readr::read_csv("output/p3/p3_UC_PARA_LEO.csv", guess_max = 1000)
mande$especie
names(mande)
count(mande, is.na(municipio_consolidado))
uc_p_leo_corregido <- readr::read_csv("data/dados_formatados/p3/leo/p3_UC_PARA_LEO.csv") %>%
  select(-lat_original, -long_original,-municipio_final, -municipio_consolidado, -localidade_x)
#all(names(mande) %in% names(uc_p_leo_corregido))
#all(names(uc_p_leo_corregido) %in% names(mande))
identical(mande$especie, uc_p_leo_corregido$especie)
#leo_uc_comp <- compareDF::compare_df(mande, uc_p_leo_corregido, "id")
#diff <- diff_data(mande, uc_p_leo_corregido)
#render_diff(diff)
ast
uc_leo <- left_join(new, uc_p_leo_corregido, by = c("id", "especie"))
identical(uc_leo$especie, new$especie)
count(ast, !is.na(lat_corrigida))
count(new, !is.na(lat_corrigida))
names(uc_p_leo_corregido)
names(uc_leo)

count(uc_leo, is.na(municipio), is.na(municipio_corrigidos_leo))
count(uc_leo, is.na(lat_corrigida), is.na(lat_corrigida_final), lat_corrigida_final == lat_corrigida,
      lat_corrigida_final != lat_corrigida)
count(uc_leo, coordenada_original_boa.x, coordenada_original_boa.y)
count(uc_leo, situacao_coordenada.x, situacao_coordenada.y)
count(uc_leo2, situacao_coordenada.x, situacao_coordenada.y, situacao_coordenada)
uc_leo2 <- uc_leo %>% mutate(
  municipio = if_else(!is.na(municipio_corrigidos_leo),
                      municipio_corrigidos_leo, municipio),
  lat_corrigida = if_else(!is.na(lat_corrigida_final),
                          lat_corrigida_final, lat_corrigida),
  long_corrigida = if_else(!is.na(long_corrigida_final),
                           long_corrigida_final, long_corrigida),
  coordenada_original_boa = if_else(!is.na(coordenada_original_boa.y),
                                    coordenada_original_boa.y, coordenada_original_boa.x),
  situacao_coordenada = if_else(!is.na(situacao_coordenada.y),
                                situacao_coordenada.y, situacao_coordenada.x))
count(uc_leo2, situacao_coordenada.x, situacao_coordenada.y, situacao_coordenada)
names(uc_leo2)
setdiff(ast$nome_uc, uc_leo2$nome_uc)
identical(ast$id,uc_leo2$id)
identical(ast$id,jun4$id)
head(template$id)
head(ast$id)
write_csv(uc_leo2, "output/p3/p3_NEW_5JUN.csv")



pat <- readr::read_csv("data/dados_formatados/p3/patricia/p3_5JUN - Patricia_V2 - p3_5JUN - Patricia_V2.csv", guess_max = 13000) %>% arrange(especie, id)
summary(pat$lat_corrigida)
summary(pat$lat_corrigida)
#quais que valem?

#situacao
pat_sit <- pat %>% select(id, situacao_coordenada)
sc <- sc_final %>% left_join(pat_sit, by = "id")
table(sc$situacao_coordenada.x, sc$situacao_coordenada.y)
filter(situacao_coordenada.x != situacao_coordenada.y) %>%
  View()#no need
#cob
pat_cob <- pat %>% select(id, coordenada_original_boa, lat_corrigida, lat_corrigida_centroide, long_corrigida_centroide)
COB_PAT <- merge_cob %>% left_join(pat_cob, by = "id") %>%
mutate(coordenada_original_boa =
         if_else(
           (id %in% c(55878, 55879)) | (coordenada_original_boa.x == "NAO" & !is.na(long_corrigida_centroide)), coordenada_original_boa.x, coordenada_original_boa.y)) %>%
  select(id, coordenada_original_boa)

### observacao atualizada adicionada (nao tinha antes)
obs_jun <- jun4 %>% select(id, obs_leo)
obs_ast <- ast %>% select(id, obs_leo)
obs_leo <- leo %>% select(id, obs_leo)
obs_leo_merged <- merge_data(obs_jun, obs_ast, obs_leo)
obs_pat <- pat %>% select(id, obs_leo)
obs <- left_join(obs_leo_merged, obs_pat, by = "id") %>%
mutate(obs_leo = if_else(!is.na(obs_leo.y), obs_leo.y, obs_leo.x)) %>%
  select(id, obs_leo)
#  count(obs_leo.x ,obs_leo.y, obs_leo) %>%
 # View()

setdiff(uc_leo2$coordenada_original_boa,
uc_leo3$coordenada_original_boa)
uc_leo3 <- uc_leo2 %>% left_join(obs)
"coordenada_original_boa" %in% names(uc_leo3)
uc_leo3 <- uc_leo3 %>% select(-coordenada_original_boa) %>% left_join(COB_PAT)
sort(names(pat))
pat_update <- pat %>%
  select(id, municipio_corrigidos_leo, lat_corrigida, long_corrigida, conflito_municipios, municipio_duplo) %>% #obs_leo ya fue en las lineas siguientes
  rename(
    municipio_corrigidos_leo_new = municipio_corrigidos_leo,
    #coordenada_original_boa_new = coordenada_original_boa,
    lat_corrigida_new = lat_corrigida,
    long_corrigida_new = long_corrigida,
    #situacao_coordenada_new = situacao_coordenada
    )

uc_leo3 <- uc_leo3 %>% select(-municipio_corrigidos_leo,
                              -lat_corrigida_final,
                              -long_corrigida_final,
                              -coordenada_original_boa.y,
                              -coordenada_original_boa.x,
                              -situacao_coordenada.y,
                              -situacao_coordenada.x)
names(uc_leo3)[!names(uc_leo3) %in% names(pat)]
all(names(uc_leo3) %in% names(pat))
uc_leo3 <- uc_leo3 %>% select(any_of(names(pat)))
names(uc_leo3)
uc_leo3 %>% select(starts_with("long"))
#falta consolidar
uc_leo3 %>% select(starts_with("municipio"))

uc_leo3 %>% select(starts_with("situacao"))
uc_leo3 %>% select(starts_with("coordenada"))

ast %>% filter(id %in% c(55878, 55879)) %>% pull(coordenada_original_boa)

summary(pat_update$lat_corrigida_new)
summary(pat_update$long_corrigida_new)
# cruza con pat update
uc_leo4 <- left_join(uc_leo3, pat_update)
names(uc_leo4)

#municipios_duplo

write_csv(uc_leo4, "output/p3/p3_NEW_6JUN.csv")

#consolida correcao coordenadas
count(uc_leo4, is.na(long_corrigida), is.na(long_corrigida_new))

uc_leo5 <- uc_leo4 %>% mutate(
  lat_corrigida =
    case_when(is.na(lat_corrigida) & !is.na(lat_corrigida_new) ~ lat_corrigida_new,
              !is.na(lat_corrigida) & is.na(lat_corrigida_new) ~ lat_corrigida),
  long_corrigida =
    case_when(is.na(long_corrigida) & !is.na(long_corrigida_new) ~ long_corrigida_new,
              !is.na(long_corrigida) & is.na(long_corrigida_new) ~ long_corrigida),
  )


# centorides municipios
source("R/functions.R")
mpos_t20c <- sf::st_point_on_surface(mpos_t20) %>% mutate(col = "red")
mm <- mpos_t20c %>% st_as_sf()
library(tmap)
tmap_mode("view")
tm_shape(mpos_t20) +
  tm_borders() +
  tm_shape(mm) +
  tm_dots(size = 1, col ="red", shape = 1)
plot(mpos_t20[1])
plot(mm[1], add = T)

library(dplyr)

mpos_t20c_df <- mpos_t20c %>%
  dplyr::mutate(lat_corrigida_cent = sf::st_coordinates(.)[,2],
                long_corrigida_cent = sf::st_coordinates(.)[,1]) %>%
  rename(municipio = municipio_padronizado) %>%
  dplyr::select(municipio, lat_corrigida_cent, long_corrigida_cent) %>%
  st_drop_geometry()
mpos_t20c_df
points(mpos_t20c_df$long_corrigida_cent, mpos_t20c_df$lat_corrigida_cent, col = "red")
uc_leo6 <- uc_leo5 %>% left_join(mpos_t20c_df) %>%
  mutate(lat_corrigida_centroide = if_else(coordenada_original_boa == "Cent_Mun",lat_corrigida_cent, NA_real_ )) %>%
  mutate(long_corrigida_centroide = if_else(coordenada_original_boa == "Cent_Mun",long_corrigida_cent, NA_real_ )) %>%
  dplyr::select(-lat_corrigida_cent, -long_corrigida_cent)
uc_leo6 <- uc_leo6 %>% mutate(T20 = if_else(municipio %in% mpos_t20c_df$municipio, "DENTRO de T20", "FORA de T20"))

uc_leo6 %>% count(coordenada_original_boa,is.na(lat_original), is.na(lat_corrigida), is.na(lat_corrigida_centroide)
                  )


#consolida
uc_leo7 <- uc_leo6 %>%
mutate(
  lat_consolidada = case_when(
    !is.na(lat_corrigida_centroide) ~ lat_corrigida_centroide,
    is.na(lat_corrigida_centroide) & !is.na(lat_corrigida_new) ~ lat_corrigida_new,
    is.na(lat_corrigida_centroide) & is.na(lat_corrigida_new) ~ lat_corrigida,
    is.na(lat_corrigida_centroide) & is.na(lat_corrigida_new) & is.na(lat_corrigida) ~ lat_original),
  long_consolidada = case_when(
    !is.na(long_corrigida_centroide) ~ long_corrigida_centroide,
    is.na(long_corrigida_centroide) & !is.na(long_corrigida_new) ~ long_corrigida_new,
    is.na(long_corrigida_centroide) & is.na(long_corrigida_new) ~ long_corrigida,
  is.na(long_corrigida_centroide) & is.na(long_corrigida_new) & is.na(long_corrigida) ~ long_original)) %>%
  mutate(lat_consolidada = if_else(coordenada_original_boa == "SIM", lat_original, lat_consolidada)) %>%
  mutate(long_consolidada = if_else(coordenada_original_boa == "SIM", long_original, long_consolidada))


write_csv(uc_leo7, "output/p3/p3_NEW_7JUN.csv")
names(uc_leo7)
#cruza com ibge
cruza_mpos_ibge <- function(csv) {
  final_sf <- read_sf(csv,
                      options = c("X_POSSIBLE_NAMES=long_consolidada", "Y_POSSIBLE_NAMES=lat_consolidada"))
  mposIBGE <- mpos %>% dplyr::select(NM_MUN)
  st_crs(mposIBGE)
  final_sf <- st_set_crs(final_sf, st_crs(mposIBGE))
  final_sf <- st_join(final_sf, mposIBGE)
  final_sf <- final_sf %>% mutate(conflito_municipios_new = if_else(municipio == NM_MUN, "OK", "conflito"))
}

uc_leo8 <- cruza_mpos_ibge("output/p3/p3_NEW_7JUN.csv")
count(uc_leo8, conflito_municipios, conflito_municipios_new)
uc_leo9 <- uc_leo8 %>% sf::st_drop_geometry() %>%
  arrange(especie, Nome_coletor_padronizado, numero_coletor, ano_coleta)
names(uc_leo9)
write_csv(uc_leo9, "output/p3/p3_NEW_8JUN_conflitos2.csv")

uc_leo8 %>% count(coordenada_original_boa, is.na(lat_corrigida), is.na(lat_corrigida_new), lat_corrigida == lat_corrigida_new,
                  is.na(lat_consolidada))

uc_leo9 %>% count(conflito_municipios, conflito_municipios_new)
uc_leo8 %>% count(coordenada_original_boa, is.na(lat_corrigida_centroide))
uc_leo8 %>% count(coordenada_original_boa, lat_corrigida_new)

names(uc_leo8)
summary(uc_leo9$lat_consolidada)


