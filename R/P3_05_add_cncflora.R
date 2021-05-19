library(readr)
source("R/functions.R")
spp <- readr::read_csv("data/dados_formatados/Clip/Base de dados Territorio 20 - Produto 3.xlsx - Lista de spp. e cat.csv")
clip <- readr::read_csv("data/dados_formatados/Clip/Planilha_Clip_Livro_Vermelhor_T20 - Planilha_Clip_Livro_Vermelhor_T.csv", guess_max = 100000)


mpos_t20 <-  sf::read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos_t20 <- clean_string(mpos_t20, NM_MUNICIP)
clip <- clean_string(clip, city)
sum(spp$nome_aceito_correto %in% clip$specie)
sum(mpos_t20$mpo_check %in% clip$mpo_check)
sum(clip$mpo_check %in% mpos_t20$mpo_check & clip$specie %in% spp$nome_aceito_correto)
clip_filtrado <- clip %>%
  filter(mpo_check %in% mpos_t20$mpo_check & specie %in% spp$nome_aceito_correto)
names(clip_filtrado)
locs_cncflora <- clip_filtrado %>% select(state, city, locality, longitude, latitude, mpo_check) %>% filter(!is.na(locality)) %>% distinct()
write_csv(locs_cncflora, "output/p3/cncflora.csv")
