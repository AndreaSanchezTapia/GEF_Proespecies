
library(readxl)
library(dplyr)
library(janitor)
library(readr)
source("R/functions.R")


# atualizaçao tabela leo----

# le tabela velha e salva ela de vez como csv
rtf_old <-  readr::read_csv("output/p3/p3_ready_to_filter.csv",
                            guess_max = 100000) %>%
  janitor::clean_names()
write_csv(rtf_old, "output/p3/p3_ready_to_filter.csv")

rtfPR <-  readxl::read_xlsx("data/dados_formatados/p3/patricia/p3_ready_to_filter-20-05-21- PR e HCL_AST.xlsx", guess_max = 100000, trim_ws = T) %>%
  janitor::clean_names()

rtfPR <- rtfPR %>%
  mutate(across(starts_with("ano"), as.numeric)) %>%
  mutate(across(starts_with("mes"), as.numeric)) %>%
  mutate(across(starts_with("dia"), as.numeric)) %>%
  mutate(across(starts_with("presenca"), as.numeric)) %>%
  mutate(tombo_herbario_origem = as.numeric(tombo_herbario_origem)) %>% mutate(cd_mun = as.numeric(cd_mun)) %>%
  select(-area_km2)
write_csv(rtfPR, "output/p3/p3_rtf.csv")

leo <-  readxl::read_xlsx("data/dados_formatados/p3/leo/Base de dados Territorio 20 - Produto 3_Cópia 24_05_Download.xlsx", guess_max = 100000, skip = 1) %>%
  rename(especie = especie...6) %>%
  janitor::clean_names()
write_csv(leo, "output/p3/p3_leo.csv")

#filtra leo pelos id
leo <- leo %>%
  filter(id %in% rtf$id)


#COMPARA OS DATA FRAMES PARA DECIDIR QUAIS COLUNAS DE ONDE
#https://sharla.party/post/comparing-two-dfs/

#formata coord original y leo----
#dados anteriores para conferencia e coordenadas originais

coord_original <- rtf_old %>% select(id,
                                     especie,
                                     lat_original, long_original,
                                     #localidade,
                                     verbatim_locality,
                                     #observacao_localidade,
                                     municipio_padronizado_final
)
coord_leo <- leo %>%
  select(id,
         especie,
         lat_corrigida, long_corrigida,
         coordenada_original_boa,
         #centroide_municipio,
         situacao_coordenada, situacao_coordenada_2,
         localidade, observacao_localidade,
         municipio, nome_uc) %>%
  rename(municipio_leo = municipio)



# padronizar municipio patricia affe----

mpo_PR <- rtfPR %>%
  select(id, especie, municipio) %>%
  rename(mPR = municipio) %>%
  clean_string(mPR)
# mpo_original <- rtf_old %>%
#   select(id, especie, municipio) %>%
#   clean_string(municipio) %>%
#   rename(mpo_old = mpo_check)
mpo_PR_final <- #full_join(mpo_original, mpo_PR) %>%
  left_join(mpo_PR, mpo_shape) %>%
  #filter(mpo_check != mpo_old) %>%
  filter(mPR != "NA") %>%
# só manter quando a substituição é feita por um nome de municipio válido
  filter(!is.na(municipio_padronizado)) %>%
  rename(mPRf = municipio_padronizado) %>%
  select(id, mPRf)



#desambiuiagao
disam <- rtf %>% select(id, especie, localidade, municipio, municipio_padronizado_final) %>%
  filter(stringr::str_detect(
    string = rtf$municipio_padronizado_final,
    pattern = ","))
disamb <- disam %>% clean_string(municipio) %>% left_join(mpo_shape) %>%
  select(id, municipio_padronizado_final, municipio_padronizado) %>%
  rename(municipio_corrigido = municipio_padronizado)

mpo_corrigido <- full_join(mpo_PR_final, disamb) %>% select(-municipio_padronizado_final)
View(mpo_corrigido)
rtf <- rtf %>%
  left_join(mpo_corrigido) %>% #count(id, municipio_padronizado_final, municipio_corrigido) %>% filter(!is.na(municipio_corrigido)) %>% View()
  mutate(municipio_final = if_else(
  !is.na(municipio_corrigido),
  municipio_corrigido, municipio_padronizado_final))


# junta tudo

rtf_atual <- rtf %>%
  select(
    -lat_original,
    -long_original,
    -lat_corrigida,
    -long_corrigida,
    -coordenada_original_boa,
    -situacao_coordenada,
    -situacao_coordenada_2,
    -municipio,
    #-mpo_county,
    -centroide_municipio,
    -nome_uc,
    -mpo_check,
    -municipio_localidade,
    -mun_detected_t20,
    -municipio_verbatim_localidade,
    -mun_detected_t20_v,
    -municipio_padronizado2
    #-municipio_padronizado_final,
    #-municipio_padronizado,
    #-municipio_corrigido
  ) %>%
  rename(obs_loc_patricia = observacao_localidade) %>%
  left_join(coord_original, by = c("id", "especie")) %>%
  left_join(coord_leo, by = c("id", "especie"))
names(rtf_atual)



# #lo que le pasé a leo
#
# p_leo <- rtf_atual %>% #rename(qual_dupl = which_dupl) %>%
#  filter(presenca_terr_20_revisada == 1)
#
# p_leo <- p_leo %>% select(id, especie, ano_coleta, municipio_padronizado_final, municipio_patricia, municipio_leo, localidade, localidade_patricia, lat_original, long_original, coordenada_original_boa, lat_corrigida, long_corrigida, situacao_coordenada, nome_uc)
# write_csv(p_leo, "output/p3/p3_recente_leo.csv")
#
# write_csv(rtf_atual, "output/p3/p3_ready_to_filter25MAIO.csv")
#identical(names(rtf_atual), names(p_leo))
#rtf_atual %>% count(presenca_terr_20_revisada)

#rtf_atual %>% filter(presenca_terr_20_revisada.x == 0) %>% write_csv("output/p3/p3_presenca0.csv ")
#rtf_atual %>% filter(presenca_terr_20_revisada.x == 2) %>% write_csv("output/p3/p3_presenca2.csv ")
#rtf_atual %>% filter(presenca_terr_20_revisada.x %in% c(1, 3, 4))
mpos_t20 <-  read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos_t20 <- clean_string(mpos_t20, NM_MUNICIP)
sort(mpos_t20$mpo_check)

#inclui
ambiguidade <- readxl::read_xlsx("data/dados_formatados/p3/patricia/presenca_ambigua- rev.PR.xlsx") %>% select(etiqueta, categoria)
rtf_atual <- left_join(rtf_atual, ambiguidade)
count(rtf_atual, presenca_terr_20_revisada, categoria)

rtf_atual <- rtf_atual %>%
  mutate(presenca_terr_20_revisada = if_else(!is.na(categoria), categoria, presenca_terr_20_revisada))

# etiqueta

## junta herbarios mandados por patricia

herbarios <- readxl::read_xlsx("data/dados_formatados/p3/patricia/Padronização herbarios.xlsx", sheet = 3)
herbarios
rtf_atual <- left_join(rtf_atual, herbarios)
etiquetas2 <- rtf_atual %>% select(id, especie, herbario_origem_boa, nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta, etiqueta_duplicaDO, etiqueta_duplicaTA)
write_csv(etiquetas2, "output/p3/p3_etiquetas.csv")


rtf_atual <- rtf_atual %>%
  mutate(etiqueta_duplicaDO = tolower(paste0(especie, nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta))) %>%
  mutate(etiqueta_duplicaTA = tolower(paste0(especie, nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta, herbario_origem_boa)))
count(rtf_atual, etiqueta_duplicaDO)
count(rtf_atual, etiqueta_duplicaTA)

# include leo
leo26 <- read_xlsx("data/dados_formatados/p3/leo/p3_26_05_end_day_leo_V3-12.xlsx") %>%
  rename(id = id...1)
latlong_corr <- leo26 %>% select(id, etiqueta_duplicaDO, lat_corrigida, long_corrigida) %>%
  filter(!is.na(lat_corrigida)|!is.na(long_corrigida)) %>%
  filter(lat_corrigida != "NA" | long_corrigida != "NA") %>%
  distinct()
length(unique(latlong_corr$etiqueta_duplicaDO))
latlong_corr %>% count(etiqueta_duplicaDO) %>% arrange(desc(n))
latlong_corr %>% count(etiqueta_duplicaDO, lat_corrigida) %>% arrange(desc(n))

count(rtf_atual,is.na(lat_corrigida)|lat_corrigida == "NA")
count(rtf_atual,is.na(long_corrigida)|long_corrigida == "NA")
setdiff(rtf_atual$long_corrigida, latlong_corr$long_corrigida)
setdiff(latlong_corr$lat_corrigida, rtf_atual$lat_corrigida)
setdiff(latlong_corr$long_corrigida, rtf_atual$long_corrigida)
rtf_atual %>% _join(latlong_corr) %>% count(is.na(lat_corrigida)|lat_corrigida == "NA")


#checar
#donde ha duplicados?
rtf_atual_check <- rtf_atual %>%
  select(etiqueta_duplicaDO,etiqueta_duplicaTA, presenca_terr_20_revisada) %>%
  group_by(etiqueta_duplicaDO) %>%
  tidyr::pivot_wider(id_cols = c(etiqueta_duplicaDO),
                     names_from = presenca_terr_20_revisada,
                     values_from = presenca_terr_20_revisada,
                     values_fn = length)

bin <- rtf_atual_check %>% mutate(across(.cols = c(`1`, `0`, `2`, `4`, `3`), .fns = function(x) x <- if_else(x>0, 1, 0))) %>% ungroup()

rtf_sum <- bin %>% mutate(n = rowSums(.[,-c(1)], na.rm = T))
rtf_sum <- rtf_sum %>% select(n, etiqueta_duplicaDO) %>% left_join(rtf_atual_check)
count(rtf_sum, n)#9499 ok,


#classificacao das que nao tem ambiguidades-----
no_dupl_class <-  rtf_sum %>% filter(n == 1) %>% select(-n) %>% tidyr::pivot_longer(cols = c(`1`, `0`, `2`, `4`, `3`), names_to = "classificacao") %>% filter(!is.na(value)) %>% select(-value)
count(no_dupl_class,classificacao)

#rtf_atual
mpos_t20 <- left_join(mpos_t20, mpo_shape)
FICA_ETIQUETA_OK <- rtf_atual %>%
  filter(municipio_final %in% mpos_t20$municipio_padronizado) %>%
  filter(etiqueta_duplicaDO %in% no_dupl_class$etiqueta_duplicaDO)
count(FICA_ETIQUETA_OK,etiqueta_duplicaDO)#5415

NAO_FICA_ETIQUETA_OK <- rtf_atual %>%
  filter(etiqueta_duplicaDO %in% no_dupl_class$etiqueta_duplicaDO) %>%
  filter(!municipio_final %in% mpos_t20$municipio_padronizado)
count(NAO_FICA_ETIQUETA_OK, etiqueta_duplicaDO)#4384

rtf_atual_1 <- rtf_atual %>% filter(!etiqueta_duplicaDO %in% NAO_FICA_ETIQUETA_OK$etiqueta_duplicaDO)
setdiff(leo26$id, rtf_atual_1$id)
setdiff(rtf_atual_entra$etiqueta_duplicaDO, leo26$etiqueta_duplicaDO)
setdiff(leo26$etiqueta_duplicaDO, rtf_atual_entra$etiqueta_duplicaDO)
count(rtf_atual_1, presenca_terr_20_revisada)

count(NAO_FICA_ETIQUETA_OK, presenca_terr_20_revisada)
names(rtf_atual_1)
any(sort(unique(NAO_FICA_ETIQUETA_OK$municipio_final)) %in% sort(mpos_t20$municipio_padronizado))

TUDO_LEO_POR_ETIQUETA <- rtf_atual %>% filter(etiqueta_duplicaDO %in% leo26$etiqueta_duplicaDO)
write_csv(TUDO_LEO_POR_ETIQUETA, "output/p3/p3_leo_expanded.csv")
###los que tienen ambiguedades
etiquetas_ambiguidaes <- rtf_sum %>% filter(n != 1)
write_csv(etiquetas_ambiguidaes, "output/p3/etiquetas_ambiguidades.csv")
#lo que importa es que cualquier etiqueta entre
etiquetas_entram <- rtf_atual_1 %>% filter(etiqueta_duplicaDO %in% etiquetas_ambiguidaes$etiqueta_duplicaDO) %>%
  filter(municipio_final %in% mpos_t20$municipio_padronizado) %>%
  select(etiqueta_duplicaDO) %>% distinct()#3927

sum(rtf_atual_1$etiqueta_duplicaDO %in% etiquetas_entram$etiqueta_duplicaDO)
sum(rtf_atual_1$etiqueta_duplicaDO %in% etiquetas_entram$etiqueta_duplicaDO)
#etiquetas ambiguas que entran
rtf_atual_entra <- filter(rtf_atual_1,
                          etiqueta_duplicaDO %in% etiquetas_entram$etiqueta_duplicaDO)#11648
count(rtf_atual_entra, etiqueta_duplicaDO %in% leo26$etiqueta_duplicaDO)

names(rtf_atual)
length(unique(rtf_atual_entra$etiqueta_duplicaDO))#3927
sum(unique(rtf_atual_entra$etiqueta_duplicaDO) %in% unique(leo26$etiqueta_duplicaDO))
unique(leo26$etiqueta_duplicaDO)#6157
#checkm unicipio final
filter(rtf_atual, etiqueta_duplicaDO %in% leo26$etiqueta_duplicaDO) %>%
  filter(municipio_final %in% mpos_t20$municipio_padronizado)#11362

rtf_atual_sai <- filter(rtf_atual, !etiqueta_duplicaDO %in% rtf_atual_entra$etiqueta_duplicaDO)#19976
which(rtf_atual_sai$municipio_final %in% mpos_t20$municipio_padronizado)
any(rtf_atual_2$etiqueta_duplicaDO %in% NAO_FICA_ETIQUETA_OK$etiqueta_duplicaDO)


left_join(mpos_t20) %>%
  group_by(etiqueta_duplicaDO) %>%
  count(etiqueta_duplicaDO, municipio_padronizado_final)
mutate(municipio_corrigido_etiqueta = )
rtf_atual %>% filter( municipio_final == "NA") %>%
  distinct(etiqueta_duplicaDO)#7603

ambiguidade_final <- left_join(rtf_sum, rtf_atual_check)
ambiguidade_final %>% mutate(final = if_else(`1` > 0, "entra", "sai")) %>%
  count(final)#
count(ambiguidade_final, n)



rtf_atual %>%
  select(etiqueta_duplicaDO, municipio_padronizado_final) %>%
  group_by(etiqueta_duplicaDO) %>%
  count(etiqueta_duplicaDO, municipio_padronizado_final) %>% arrange(desc(n))





leo3 <- read_csv("output/p3/p3_recente_leo3.csv")
etiqueta_nueva <- rtf_atual %>% select(id, etiqueta_duplicaDO, etiqueta_duplicaTA)
names(leo3)
leo4 <- left_join(leo3, etiqueta_nueva)
count(leo4, is.na(etiqueta_duplicaDO))
count(leo4, is.na(etiqueta_duplicaTA))
write_csv(leo4, "output/p3/p3_recente_leo4.csv")
eti <- leo4 %>% select(id, starts_with("etiqueta"))
write_csv(eti, "output/p3/p3_etiquetas_leo.csv")

rtf_atual %>% filter(etiqueta_duplicaDO %in% leo4$etiqueta_duplicaDO)
sum(rtf_atual$id %in% leo4$id)
sum(!rtf_atual$id %in% leo4$id)
length(unique(leo4$etiqueta_duplicaDO))
write_csv(leo5, "output/p3/p3_recente_leo5.csv")


ha_dupl <- rtf_atual %>% count(etiqueta_duplicaDO) %>% filter(n != 1) #9702
count(rtf_atual, status_dupl)
rtf_atual <- rtf_atual %>%
  mutate(status_dupl = case_when(
    !etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "NO_DUPL",
    etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "DUPL"
  ))
write_csv(rtf_atual, "output/p3/p3_rtf_26MAY.csv")


index_duplicados <- rtf_atual %>%
  select(id, etiqueta_duplicaDO) %>%
  #mutate(id = as.numeric(id)) %>%

  group_by(etiqueta_duplicaDO) %>% mutate(duplicados = paste(id, collapse = "/"))
index_duplicados
rtf_atual <- left_join(rtf_atual, index_duplicados)
write_csv(index_duplicados, "output/p3/index_duplicados.csv")
index_duplicados %>% select(-id) %>% distinct() %>% write_csv( "output/p3/index_duplicados_unico.csv")




#eliminar as que nao tem dado E nao tem duplicado
count(rtf_atual, municipio_final == "NA") %>% arrange(desc(n))
count(rtf_atual, nm_mun == "NA") %>% arrange(desc(n))

count(rtf_atual, verbatim_locality.y == "NA") %>% arrange(desc(n))
rtf_atual_no_data <- rtf_atual %>% mutate(no_data = if_else(
  municipio_final == "NA" &
    (is.na(lat_original) | is.na(long_original)) &
    (is.na(verbatim_locality.x) | verbatim_locality.x == "NA") &
    (is.na(verbatim_locality.y)) &
#    (nm_mun == "NA") &
    (is.na(localidade.x) | localidade.x == "NA" | localidade.y == "NA" | is.na(localidade.y)), "no_data", "data"
))
count(rtf_atual_no_data, no_data, municipio_final == "NA")
ETiQUETA_NO_DATA <- rtf_atual_no_data %>%
  #mutate(no_data_no_dupl = if_else(no_data = "no_data" & status_dupl == "#count(nodata)
filter(no_data == "no_data") %>% select(etiqueta_duplicaDO) %>%
  distinct()#count(status_dupl) #4297 nao tem dado mas tem duplicado e 877 nao tem dado E nao tem duplicado
rtf_atual_no_data %>% filter(etiqueta_duplicaDO %in% ETiQUETA_NO_DATA$etiqueta_duplicaDO) %>% count(no_data, status_dupl)

colunas_no_data <- c("municipio_final", "lat_original","long_original", "verbatim_locality.x", "verbatim_locality.x", "verbatim_locality.y", "nm_mun", "localidade.x", "localidade.x", "localidade.y", "localidade.y")

filter(rtf_atual_no_data, etiqueta_duplicaDO %in% ETiQUETA_NO_DATA$etiqueta_duplicaDO) %>%
  select("etiqueta_duplicaDO", "no_data", "status_dupl", colunas_no_data, ) %>% View()
