p4_pronto <- read_csv("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx - Registros_flora_ja_levantados.csv", guess_max = 12000)

#s.i


p4_anexo4_sai <- p4_pronto %>%
  filter(`IPF - EXCLUIR` == "excluir registro")
nrow(p4_pronto)
nrow(p4_anexo4_sai)
p4_fica <- p4_pronto %>% anti_join(p4_anexo4_sai)
nrow(p4_fica)

p4_sai_not_t20 <- p4_fica %>% filter(!p4_fica$municipio %in% mpos_t20$municipio_padronizado) %>%
  filter(!is.na(municipio)) %>%
  filter(municipio != "s.i.")
p4_sai_not_t20$municipio

p4_fica <- anti_join(p4_fica, p4_sai_not_t20)


# novas <- c("Macroscepis magnifica", "Lessingianthus exiguus", "Vriesea brusquensis", "Didymoglossum ovale", "Persea punctata", "Urbanodendron bahiense", "Lepanthopsis floripecten", "Zygostates pellucida", "Phlegmariurus taxifolius", "Stenogrammitis limula", "Psychotria microcarpa", "Selaginella mendoncae", "Smilax lutescens", "Xyris longifolia", "Renealmia brasiliensis")
# test <- p4_pronto %>% filter(especie %in% novas)
# all(novas %in% test$especie)
# test %>% count(especie) %>% View()
#
# sai_andrea <- p4_anexo4_sai$especie[!p4_anexo4_sai$especie %in% p4_fica$especie] %>% unique() %>% tibble()
# write_csv(sai_andrea, "output/p4/p4_saem.csv")
#
# t20 <- read_csv("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx - Lista_Especies_T20.csv")
# sae_patricia <- t20 %>% filter(`IPF - EXCLUIR` == "excluir especie") %>% select(especie)
#
# setdiff(sai_andrea$., sae_patricia$especie) %>% tibble()
# setdiff()
#
# t20 %>% filter(especie %in% sai_andrea)
#
#
###############
p4_fica$municipio[!p4_fica$municipio %in% mpos_t20$municipio_padronizado]


 p4_fica %>% count(
  situacao_coordenada)#9229  cosr ok, CO 112 ok

library(sf)
p4_fica$lat_original <- as.numeric(p4_fica$lat_original)
p4_fica$long_original <- as.numeric(p4_fica$long_original)
p4_fica$long_corrigida <- as.numeric(p4_fica$long_corrigida)
p4_fica$lat_corrigida <- as.numeric(p4_fica$lat_corrigida)
p4_fica_shp <- p4_fica[complete.cases(
p4_fica[,c("long_corrigida", "lat_corrigida")]),]
p4_fica_shp[p4_fica_shp == ""] <- "s.i."
p4_fica_shp[p4_fica_shp == "NA"] <- "s.i."
p4_fica_shp <- p4_fica_shp %>% mutate_all(.funs = ~replace_na(., "s.i."))
shape <-  st_as_sf(p4_fica_shp, coords = c("long_corrigida", "lat_corrigida"))
write_sf(shape, "output/p4/p4_anexo1_shapefile.gpkg")


p4_sai <- bind_rows(p4_anexo4_sai,
p4_sai_not_t20) %>% mutate_all(.funs = ~replace_na(., "s.i."))
p4_sai[p4_sai == ""] <- "s.i."
p4_sai[p4_sai == "NA"] <- "s.i."

p4_fica <- p4_fica %>% mutate_all(.funs = ~replace_na(., "s.i."))
p4_fica[p4_fica == ""] <- "s.i."
p4_fica[p4_fica == "NA"] <- "s.i."

p4_pronto <- p4_pronto %>% mutate_all(.funs = ~replace_na(., "s.i."))
p4_pronto[p4_pronto == ""] <- "s.i."
p4_pronto[p4_pronto == "NA"] <- "s.i."

nrow(p4_pronto) == nrow(p4_fica) + nrow(p4_sai)
write_csv(p4_fica, "output/p4/p4_fica_FINAL.csv")
write_csv(p4_sai, "output/p4/p4_sai_FINAL.csv")
write_csv(p4_pronto, "output/p4/p4_tudo_FINAL.csv")
