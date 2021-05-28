#
library(readxl)
library(dplyr)
library(janitor)
library(readr)
source("R/functions.R")

# atualizaçao tabela leo----
leo <-
  readxl::read_xlsx("data/dados_formatados/p3/leo/p3_27_05_end_day_leo_V3.xlsx",
                    guess_max = 100000) %>% select(-id...16) %>%
  rename(id = id...1) %>%
  mutate(ano_coleta = as.numeric(ano_coleta))
names(leo)
write_csv(leo, "output/p3/p3_leo.csv")

# rtf_atual
rtf_o <- readr::read_csv("output/p3/p3_rtf_26MAY.csv", guess_max = 100000)
coord_original <- rtf_o %>% select(id,
                                 especie,
                                 lat_original, long_original,
                                 #localidade,
                                 #verbatim_locality,
                                 #observacao_localidade,
                                 #municipio_padronizado_final
)

names(leo)

coord_leo <- leo %>%
  select(id, especie,
         lat_corrigida, long_corrigida,
         coordenada_original_boa,
         #centroide_municipio,
         situacao_coordenada,
        #localidade, observacao_localidade,
         #municipio,
         nome_uc)
count(rtf_o, is.na(lat_corrigida))
count(coord_leo, is.na(lat_corrigida))
# include leo
rtf_corr_leo <- rtf_o %>% select(-lat_corrigida, -long_corrigida,
               -coordenada_original_boa,
               #centroide_municipio,
               -situacao_coordenada,
               #localidade, observacao_localidade,
               #munici,
               -nome_uc) %>% left_join(coord_leo)
rtf_corr_leo %>% count(is.na(lat_corrigida))
length(unique(leo$lat_corrigida))
length(unique(leo$long_corrigida))

mpos_t20 <-  read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos_t20 <- clean_string(mpos_t20, NM_MUNICIP)
sort(mpos_t20$mpo_check)

#solve ambi
ambi_27 <- readxl::read_xlsx("data/dados_formatados/p3/patricia/etiquetas_ambiguidades_PR-27-05-xlxs.xlsx")

ambi_final <- ambi_27 %>%
  mutate(categoria = if_else(`1`!= "NA" & `0` == "NA", 1, NA_real_)) %>%
  mutate(categoria = if_else(DIAGNOSTICO %in% c("fora do terr. 20", "fora do Terr. 20"), 0, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO %in% c("manter", "manter ambos"), 1, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO %in% c("retirar ambos registros"), 0, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO == "retirar registros 2" & n == 2 & `0` != "NA", 0, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO == "retirar registros 2" & n == 2 & `1` != "NA", 1, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO == "retirar registros 2" & n == 2 & `3` != "NA", 1, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO == "retirar registros 2" & n == 2 & `4` != "NA", 1, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO == "retirar registros 2 e 0" & n == 2 , 0, categoria)) %>%
  mutate(categoria = if_else(is.na(DIAGNOSTICO) & is.na(ACAO) & `1` != "NA" & `3` != "NA" & n == 2, 1, categoria)) %>%
  mutate(categoria = if_else(is.na(DIAGNOSTICO) & is.na(ACAO) & `1` != "NA" & `4` != "NA" & n == 2, 1, categoria)) %>%
  mutate(categoria = if_else(MUNICIPIO %in% c("São Paulo", "Mairiporã", "Iperó", "Sorocaba", "São Vicente"), 1, categoria)) %>%
  mutate(categoria = if_else(MUNICIPIO %in% c("Campinas", "Rio Claro"), 0, categoria)) %>%
  mutate(categoria = if_else(DIAGNOSTICO %in% c("retirar duplicado", "retirar registro da coluna 0",
                                                "retirar ids indicados - o que esta em 0 trazer para 4"
                                                ), 1, categoria)) %>%
  mutate(categoria = if_else(is.na(categoria) & `0` != "NA" & `1` == "NA", 0, categoria)) %>%
  mutate(categoria = if_else(is.na(categoria) & DIAGNOSTICO == "retirar registros 2", 0, categoria)) %>%
  mutate(categoria = if_else(n == 2 & `0` == "NA" & `1` == "NA" & `2` == "NA", 1, categoria)) %>%
  mutate(categoria = if_else(n == 2 & `1` != "NA" & `2` != "NA" , 1, categoria)) %>%
  mutate(categoria = if_else(is.na(categoria) & `1` != "NA" & `0` != "NA" , 999, categoria)) %>% rename(categoria_final = categoria) %>% select(etiqueta_duplicaDO, categoria_final)
rtf_now <- left_join(rtf_corr_leo, ambi_final)
rtf_now %>% count(presenca_terr_20_revisada, categoria, categoria_final) %>% View()

rtf <- rtf_now %>% mutate(categoria_final =
                 if_else(!is.na(categoria_final), categoria_final, presenca_terr_20_revisada))

rtf %>%
  count(categoria_final, etiqueta_duplicaDO)
rtf %>%
count(presenca_terr_20_revisada,categoria_final)
length(unique(rtf$etiqueta_duplicaDO))

#beleza, ambiguidade resolvida

#donde ha duplicados? com categoria final
rtf_atual_check2 <- rtf %>%
  select(etiqueta_duplicaDO,etiqueta_duplicaTA, categoria_final) %>%
  group_by(etiqueta_duplicaDO) %>%
  tidyr::pivot_wider(id_cols = c(etiqueta_duplicaDO),
                     names_from = categoria_final,
                     values_from = categoria_final,
                     values_fn = length)

bin <- rtf_atual_check2 %>%
  mutate(across(.cols = c(`1`, `0`, `2`, `4`, `3`, `999`),
                                          .fns = function(x) x <- if_else(x > 0, 1, 0))) %>% ungroup()

rtf_sum <- bin %>% mutate(n = rowSums(.[,-c(1)], na.rm = T))
rtf_sum <- rtf_sum %>% select(n, etiqueta_duplicaDO) %>% left_join(rtf_atual_check2)
count(rtf_sum, n)#9499 ok,
#TUDO BEM PORRA

#classificacao das que nao tem ambiguidades-----antes
#no_dupl_class <-  rtf_sum %>% filter(n == 1) %>% select(-n) %>% tidyr::pivot_longer(cols = c(`1`, `0`, `2`, `4`, `3`), names_to = "classificacao") %>% filter(!is.na(value)) %>% select(-value)
count(no_dupl_class,classificacao)
no_dupl_classNEW <-  rtf_sum %>% filter(n == 1) %>% select(-n) %>% tidyr::pivot_longer(cols = c(`1`, `0`, `2`, `4`, `3`, `999`), names_to = "classificacao") %>% filter(!is.na(value)) %>% select(-value)
count(no_dupl_classNEW, classificacao)
a <- names(rtf)
#rtf_atual
rtf <- rtf %>% left_join(mpo_PR_final)#viene del script anterior
setdiff(names(rtf), a)
mpos_t20 <- left_join(mpos_t20, mpo_shape)


#PAUSA PARA FIX MUNICIPIO---
count(rtf, municipio_final, mPRf) %>% arrange(desc(n)) %>% View()
rtf <- rtf %>% mutate(municipio_final = if_else(
  is.na(municipio_final) & !is.na(mPRf), mPRf, municipio_final
)) %>%
  mutate(municipio_final = if_else(
  !is.na(municipio_final) & !is.na(mPRf) & municipio_final != mPRf, mPRf, municipio_final
))

FICA_ETIQUETA_OK <- rtf %>%
  filter(municipio_final %in% mpos_t20$municipio_padronizado) %>%
  filter(etiqueta_duplicaDO %in% no_dupl_classNEW$etiqueta_duplicaDO)
count(FICA_ETIQUETA_OK,etiqueta_duplicaDO)#5415 #NEW: 9342 #9335

NAO_FICA_ETIQUETA_OK <- rtf %>%
  filter(etiqueta_duplicaDO %in% no_dupl_classNEW$etiqueta_duplicaDO) %>%
  filter(!municipio_final %in% mpos_t20$municipio_padronizado)
count(NAO_FICA_ETIQUETA_OK, etiqueta_duplicaDO)#4384 #8913! #8860 isto tem de sair
nrow(rtf) == (nrow(FICA_ETIQUETA_OK) + nrow(NAO_FICA_ETIQUETA_OK))

rtf <- rtf %>% mutate(T20 = if_else(etiqueta_duplicaDO %in% FICA_ETIQUETA_OK$etiqueta_duplicaDO, "FICA_ETIQUETA_DUPLICADO_IN_T20","SAI_ETIQUETA_DUPLICADO_OUT_T20"))
#todo el mundo sabe para donde ir?
count(rtf, T20)#22689/8935 ·22678/8946
#los NA sabem a donde ir!
count(rtf, T20, municipio_final) %>% arrange(desc(n)) %>% View()
#la categoria 999?

###dupli
ha_dupl <- rtf %>% count(etiqueta_duplicaDO) %>% filter(n != 1) #9702 #10592
count(rtf, status_dupl)#27610 4014 27806, 3818
rtf <- rtf %>%
  mutate(status_dupl = case_when(
    !etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "NO_DUPL",
    etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "DUPL"
  ))
count(rtf, status_dupl)
#write_csv(rtf, "output/p3/p3_rtf_27MAY.csv")


index_duplicados <- rtf %>%
  select(id, etiqueta_duplicaDO) %>%
  #mutate(id = as.numeric(id)) %>%

  group_by(etiqueta_duplicaDO) %>% mutate(duplicados = paste(id, collapse = "/"))
index_duplicados
write_csv(index_duplicados, "output/p3/index_duplicados.csv")
rtf <- left_join(rtf, index_duplicados)
index_duplicados %>% select(-id) %>% distinct() %>% write_csv( "output/p3/index_duplicados_unico.csv")

#eliminar as que nao tem dado E nao tem duplicado
count(rtf, is.na(municipio_final)) %>% arrange(desc(n))#20934/10690
tail(names(rtf))
count(rtf, nm_mun == "NA") %>% arrange(desc(n))

any(rtf$localidade.y == " ", na.rm = T)
count(rtf, is.na(verbatim_locality.y)) %>% arrange(desc(n))
rtf <- rtf %>%
  mutate(no_data = if_else(
  is.na(municipio_final) &
    (is.na(lat_original) | is.na(long_original)) &
    (is.na(verbatim_locality.x) | is.na(verbatim_locality.x)) &
    (is.na(verbatim_locality.y)) &
    #    (nm_mun == "NA") &
    (is.na(localidade.x) | localidade.x == "NA" | localidade.y == "NA" | is.na(localidade.y)), "no_data", "data"
))
rtf %>% count(no_data, categoria_final) %>% View()
rtf %>% count(no_data, T20) %>% View()

#no cambia nada porque la categoría la hicimos con base en las etiquetas


ETiQUETA_NO_DATA <- rtf %>%
  mutate(no_data_no_dupl = if_else(no_data == "no_data" & status_dupl == "NO_DUPL", "no_data_no_dupl", "" ))
 #4297 nao tem dado mas tem duplicado e 877 nao tem dado E nao tem duplicado #4265, 875
ETiQUETA_NO_DATA %>% count(no_data, status_dupl, categoria_final)

rtf %>% filter(no_data == "no_data",
                             status_dupl == "NO_DUPL",
                             categoria_final == 1) %>% View()

colunas_no_data <- c("municipio_final", "lat_original","long_original", "verbatim_locality.x", "verbatim_locality.x", "verbatim_locality.y", "nm_mun", "localidade.x", "localidade.x", "localidade.y", "localidade.y")

filter(rtf_atual_no_data, etiqueta_duplicaDO %in% ETiQUETA_NO_DATA$etiqueta_duplicaDO) %>%
  select("etiqueta_duplicaDO", "no_data", "status_dupl", colunas_no_data, ) %>% View()


rtf %>% count(T20)
rtf %>% filter(T20 == "FICA_ETIQUETA_DUPLICADO_IN_T20") %>% group_by(etiqueta_duplicaDO) %>%  count(etiqueta_duplicaTA, nota_etiqueta) %>% arrange(desc(n))


leo
count(leo, lat_corrigida)
count(rtf, lat_corrigida)
