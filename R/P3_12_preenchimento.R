library(readr)
library(dplyr)
library(readxl)
library(tidyr)
source("R/functions.R")
rtf_o <- readr::read_csv("output/p3/p3_Planilha_filtrada28MAY.csv", guess_max = 100000) %>%
  mutate(municipio_final = trimws(municipio_final))
coord_original <- rtf_o %>% dplyr::select(id,
                                   especie,
                                   lat_original,
                                   long_original)

# municipios_table <- rtf_o %>%
#   group_by(etiqueta_duplicaDO) %>%
#   mutate(municipios = paste(municipio_final, collapse = "/")) %>%
#   select(id, etiqueta_duplicaDO, municipios, categoria_final)
# municipios_table
# municipios_table <- municipios_table %>% separate(municipios, into = paste0("mpo_",1:42), sep = "/")
# municipios_table[municipios_table == "NA"] <- NA
# table2 <- municipios_table %>% pivot_longer(c(-id, -etiqueta_duplicaDO, -categoria_final), names_to = "MPOS") %>%
#   filter(!is.na(value)) %>% select(-id, -MPOS) %>% distinct() %>%
#   mutate(municipios = paste(value, collapse = "/")) %>%
#   select(etiqueta_duplicaDO, municipios, categoria_final)
# table2
# municipios_table <- municipios_table %>%
#   select(id, etiqueta_duplicaDO) %>% left_join(table2) %>%
#   ungroup() %>%
#   select(id, municipios)
write_csv(municipios_table, "output/p3/municipios_table.csv")

leo <- read_xlsx("data/dados_formatados/p3/leo/p3_sem_duplicados_03_06_End_Day.xlsx")
write_csv(leo, "output/p3/p3_preencher.csv")
leo <- read_csv("output/p3/p3_preencher.csv", guess_max = 13000)
names_ord <- names(leo)
leo <- leo %>%
  dplyr::select(-ends_with("original")) %>%
  left_join(coord_original) %>%
  dplyr::select(one_of(names_ord))

count(leo, municipio_final == "NA")
head(leo$lat_original)
head(leo$long_original)

#count casos para substituir
leo %>% count(coordenada_original_boa)

#centroide municipio----
## primeiro: consolidar municipio----
names(leo)
leo <- leo %>%
  mutate(municipio_consolidado = case_when(
    !is.na(municipio_corrigidos_leo) ~ municipio_corrigidos_leo,
    is.na(municipio_corrigidos_leo) & !is.na(municipio_pati) ~ municipio_pati,
    is.na(municipio_corrigidos_leo) & is.na(municipio_pati) ~ municipio_final))
count(leo, municipio_consolidado == "NA")
municipios_table <- distinct(municipios_table)
leo <- leo %>% left_join(municipios_table)
stringr::str_detect(leo$municipios, "/", negate = T) %>% table()
count(leo, is.na(municipio_consolidado))
leo <- leo %>%
  mutate(municipio_consolidado = if_else(
    is.na(municipio_consolidado) & stringr::str_detect(leo$municipios, "/", negate = T),
    municipios,municipio_consolidado))
count(leo, is.na(municipio_consolidado))

leo <- leo %>% mutate(municipio_duplo = if_else(is.na(municipio_consolidado), municipios, NA_character_))


leo %>%
  filter(!municipio_consolidado %in% mpos_t20$municipio_padronizado & !is.na(municipio_consolidado)) %>%
  dplyr::select(municipio_consolidado, municipios) %>%
  View()
leo <- leo %>% mutate(ainda_fora_t20 = if_else(!is.na(municipio_consolidado) & !municipio_consolidado %in% mpos_t20$municipio_padronizado, "fora_de_t20", NA_character_))



# prenchimento automatico de centroides de municipio----
leo %>%
  filter(coordenada_original_boa == "Cent_Mun", !is.na(municipio_consolidado))
leo %>% count(coordenada_original_boa == "Cent_UC", !is.na(municipio_consolidado))
leo %>% count(coordenada_original_boa, !is.na(municipio_consolidado))
leo %>% filter(coordenada_original_boa == "Cent_Mun", municipio_consolidado == "Mairiporã")
#base <- read_sf2("output/p3/p3_sem_duplicados_leo_p_pat.csv", column = "original")

# prenchimento automatico de centroides de UC
mpos_t20c <- sf::st_centroid(mpos_t20)
#mpos_t20c <- mpos_t20c %>% st_as_sf() %>% mutate(mun_cent_mun = "mun")
library(dplyr)
mpos_t20c_df <- mpos_t20c %>%
  dplyr::mutate(lat_corrigida_cent = sf::st_coordinates(.)[,2],
                long_corrigida_cent = sf::st_coordinates(.)[,1]) %>%
  rename(municipio_consolidado = municipio_padronizado) %>%
  dplyr::select(municipio_consolidado, lat_corrigida_cent, long_corrigida_cent) %>%
  st_drop_geometry()
mpos_t20c_df
leo <- leo %>% left_join(mpos_t20c_df) %>%
  mutate(lat_corrigida_centroide = if_else(coordenada_original_boa == "Cent_Mun",lat_corrigida_cent, NA_real_ )) %>%
  mutate(long_corrigida_centroide = if_else(coordenada_original_boa == "Cent_Mun",long_corrigida_cent, NA_real_ )) %>%
  dplyr::select(-lat_corrigida_cent, -long_corrigida_cent)

leo %>% count(coordenada_original_boa, is.na(lat_corrigida_centroide))
leo %>% count(coordenada_original_boa, is.na(long_corrigida_centroide))

leo %>% filter(is.na(coordenada_original_boa)) %>% View()
names_new <-  names(leo)[c(1:6,16,18, 7:13,20:21,14:15, 19)]
names_ord

leo <- leo %>% select(names_new)
leo %>% count(!is.na(lat_corrigida_centroide), situacao_coordenada)
leo <- leo %>% mutate(situacao_coordenada = if_else(!is.na(lat_corrigida_centroide), "CR", situacao_coordenada))
write_csv(leo, "output/p3/p3_centroides_municipios.csv")
names(leo)
leo %>% count(coordenada_original_boa, is.na(lat_corrigida))

leo <- leo %>%
  mutate(lat_consolidada = case_when(
  coordenada_original_boa == "SIM" ~ lat_original,
  coordenada_original_boa == "Cent_Mun" ~ lat_corrigida_centroide,
  coordenada_original_boa == "NAO" ~ lat_corrigida)) %>%
  mutate(long_consolidada = case_when(
  coordenada_original_boa == "SIM" ~ long_original,
  coordenada_original_boa == "Cent_Mun" ~ long_corrigida_centroide,
  coordenada_original_boa == "NAO" ~ long_corrigida))
write_csv(leo, "output/p3/p3_centroides_municipios.csv")

leo %>%
  dplyr::select(localidade_x, lat_consolidada, long_consolidada) %>%
  group_by(localidade_x) %>%
  mutate(lats = paste(lat_consolidada, collapse = "/")) %>%
  mutate(longs = paste(long_consolidada, collapse = "/")) %>%
  ungroup() %>%
  dplyr::select(-ends_with("consolidada")) %>% distinct() %>%
  write_csv("output/p3/latlong_consolidadas.csv")
leo %>%
  count(is.na(localidade_x), is.na(lat_consolidada), is.na(municipio_consolidado))
cols_leo <- setdiff(names(leo), c("id","especie"))

large <- read_csv("output/p3/p3_sem_duplicados_leo_p_pat.csv", guess_max = 13000)
large2 <- left_join(large, leo)
write_csv(large2, "output/p3/p3_lat_lon_consolidada.csv")
names(leo)
