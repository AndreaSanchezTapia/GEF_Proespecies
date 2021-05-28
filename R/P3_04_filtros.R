p3_corr_sf <- janitor::clean_names(p3_corr_sf)


p3_corr_sf %>% filter(presenca_terr_20_revisada %in% "fora do escopo") %>%
  count(cat_ameaca_br,
        cat_ameaca_cncflora,
        cat_ameaca_cr_lac,
        cat_ameaca_iucn,
        cat_ameaca_mpo_sp,
        cat_ameaca_sp) %>% View()

p3_corr_filt <- p3_corr_sf %>% filter(!presenca_terr_20_revisada %in% c("0", "fora do escopo"))

p3_corr_filt %>%
  count(nm_mun == "NA", is.na(nm_mun))
  filter(municipio_padronizado_final == "NA")

####duplicados
p3_etiqueta <- p3_corr_sf %>%
  mutate(etiqueta = paste0(especie, nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta)) %>%
           mutate(etiqueta = tolower(etiqueta)) %>%
  mutate(nota_etiqueta = if_else(numero_coletor_padronizado1 == "s.n.", "registro_sem_numero_coletor", NA_character_))
length(unique(p3_etiqueta$etiqueta))#13958 #14841 sin lo de escopo


#tem duplicados?
ha_dupl <- p3_etiqueta %>% count(etiqueta) %>% filter(n != 1) #9702
p3_etiqueta <- p3_etiqueta %>%
  mutate(status_dupl = case_when(
  !etiqueta %in% ha_dupl$etiqueta ~ "NO_DUPL",
  etiqueta %in% ha_dupl$etiqueta ~ "DUPL",
))
p3_etiqueta
no_dupl <- p3_etiqueta %>% filter(status_dupl == "NO_DUPL")
si_dupl <- p3_etiqueta %>% filter(status_dupl == "DUPL")
si_dupl_which <-   si_dupl %>%
  mutate(which_dupl = if_else(is.na(lat_original) | is.na(long_original), "no", "this")) %>%
  filter(which_dupl == "this")
si_dupl_salen <-   si_dupl %>%
  mutate(which_dupl = if_else(is.na(lat_original) | is.na(long_original), "no", "this")) %>%
  filter(which_dupl == "no")
st_drop_geometry(si_dupl_which)#12990
test_join <- left_join(st_drop_geometry(p3_etiqueta), st_drop_geometry(si_dupl_which))
count(test_join, status_dupl, which_dupl)
test_join %>% select(-AREA_KM2) %>% count(presenca_terr_20_revisada, status_dupl, which_dupl)
ready_to_filter <- test_join %>% filter(presenca_terr_20_revisada != "fora do escopo")
#todas
write_csv(ready_to_filter, "output/p3/p3_ready_to_filter.csv")
dim(ready_to_filter)
length(unique(p3_etiqueta$etiqueta))#10677 en total 13958 si se separa los s.n 14841 si no se filtra todavia
length(unique(no_dupl$etiqueta))#3363 sin 4256 - 4020 si no se filtra todavía
length(unique(si_dupl$etiqueta)) #7314 con, la suma bate 9702 - 10821
length(unique(si_dupl_which$etiqueta)) #6208 quitando los duplicados que no sirven #8016 #9005 sin filtro ainda
faltam <- setdiff(unique(si_dupl$etiqueta), unique(si_dupl_which$etiqueta))
p3_faltam <- p3_etiqueta %>% filter(etiqueta %in% faltam)#2440 #4174
nrow(p3_faltam)#3895
p3_sem_dupl <- p3_etiqueta %>%
  filter(id %in% c(no_dupl$id, si_dupl_which$id, p3_faltam$id))# 21141 ##22826

ambiguidade <- p3_sem_dupl %>% st_drop_geometry() %>% group_by(etiqueta) %>%
  select(etiqueta, presenca_terr_20_revisada) %>%
  #mutate(presenca_ambigua = paste(presenca_terr_20_revisada, collapse = "-"))
  mutate(presenca_ambigua = if_else(any(presenca_terr_20_revisada == 1) & any(presenca_terr_20_revisada == 0), "amb", "not_amb"))
ambiguidade %>% distinct() %>% filter(presenca_ambigua == "amb") %>%
count(presenca_ambigua) %>% write_csv("output/p3/presenca_ambigua.csv")
.Last.value %>% View()
identical(names(p3_etiqueta), names(p3_sem_dupl))

duplicados_general <- p3_etiqueta %>% filter(id %in% si_dupl$id)#24902
duplicados_general %>% count(si_dupl$status_dupl)
write.csv(duplicados_general, "output/p3/p3_duplicados_general.csv")

p3_sem_dupl %>% count(status_dupl) #18806, 4020
p3_sem_dupl %>% count(is.na(lat_original)) #6483
p3_sem_dupl %>% count(is.na(long_original))#6483
p3_sem_dupl %>% count(municipio_padronizado_final == "NA",
                      is.na(nm_mun),
                      is.na(localidade))#4990 paila eran 3345 ahora 4990 no estamos tan mal sin filtro
p3
### 3345 sin municipio
p3_sem_dupl %>% count(is.na(nm_mun))
p3_sem_dupl %>% count(municipio_padronizado_final == "NA")
p3_sem_dupl %>% count(municipio_padronizado_final == "NA", is.na(nm_mun), is.na(lat_original))#4480

index_duplicados <- p3_etiqueta %>% select(id, etiqueta) %>%
  mutate(id = as.numeric(id)) %>% arrange(id) %>%
  group_by(etiqueta) %>% mutate(ids = paste(id, collapse = "-")) %>% st_drop_geometry()
length(unique(index_duplicados$etiqueta))#14841
write_csv(index_duplicados, "output/p3/index_duplicados.csv")
index_duplicados %>% select(-id) %>% distinct() %>% write_csv( "output/p3/index_duplicados_unico.csv")


p3_sem_dupl %>% count(presenca_terr_20_revisada)#12280 2, 8861 1
soh_na <- p3_sem_dupl %>%
  filter(is.na(lat_original) | is.na(long_original) | lat_original == 0 | long_original == 0 ) %>% filter(municipio_padronizado_final == "NA") %>%
  filter(localidade == "NA") %>% filter(verbatim_locality == "NA")#2617
soh_na %>% select(id) #2617
id_na <- index_duplicados %>% arrange(etiqueta) %>% filter(id %in% soh_na$id)
unique(id_na$etiqueta)
filter(p3_sem_dupl, etiqueta %in% unique(id_na$etiqueta)) %>% select(id, especie, localidade) %>% count(localidade == "NA")
amb <- read_csv("output/p3/presenca_ambigua.csv")
p3_sem_dupl_escopo <- p3_sem_dupl %>%
  filter(presenca_terr_20_revisada != "fora do escopo")
write_sf(p3_sem_dupl_escopo, "p3_sem_dupl_sem_escopo.csv")



p3_sem_dupl_t_20 <- p3_sem_dupl %>% filter(municipio_padronizado_final %in% c(mpos_t20$municipio_padronizado) | nm_mun %in% mpos_t20$municipio_padronizado)
p3_sem_dupl_t_20 <- p3_sem_dupl_t_20 %>% filter(presenca_terr_20_revisada != "fora do escopo")#14999
write_csv(p3_sem_dupl_t_20, "output/p3/p3_sem_duplicados_t_20.csv")#15025
)

