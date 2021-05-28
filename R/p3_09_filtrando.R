rtf <- write_csv(rtf, "output/p3/p3_rtf_27MAY.csv")

mpots <- rtf %>% filter(T20 == "FICA_ETIQUETA_DUPLICADO_IN_T20") %>% pull(municipio_final)
all(mpots %in% mpos_t20$municipio_padronizado, na.rm = T)
save.image()

rtf <- readr::read_csv("output/p3/p3_rtf_27MAY.csv", guess_max = 100000)
dim(rtf)
library(readr)
library(dplyr)
rtf %>% filter(especie %in%
                 c("Apium prostratum", "Brugmansia suaveolens", "Hedychium coronarium")) %>% write_csv("output/p3/p3_invasoras.csv")
rtf_filt <- rtf %>% filter(!especie %in%
                 c("Apium prostratum", "Brugmansia suaveolens", "Hedychium coronarium"))



ha_dupl <- rtf_filt %>% count(etiqueta_duplicaDO) %>% filter(n != 1)
count(rtf_filt, status_dupl)
rtf_filt <- rtf_filt %>%
  mutate(status_dupl = case_when(
    !etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "NO_DUPL",
    etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "DUPL"
  ))

si_dupl <- rtf_filt %>% filter(status_dupl == "DUPL")
no_dupl <- rtf_filt %>% filter(status_dupl == "NO_DUPL")

#tira os duplicados que não têm coordenadas
si_dupl %>% group_by(etiqueta_duplicaDO) %>%  mutate(which_dupl = if_else(is.na(lat_original) | is.na(long_original), "no_coordinates", "this")) %>% filter(which_dupl == "no_coordinates") %>% write_csv("output/p3/p3_no_coordinates.csv")

#fica os duplicados que têm coordenadas (vai ter duplicados)
si_dupl_fica <- si_dupl %>% group_by(etiqueta_duplicaDO) %>%  mutate(which_dupl = if_else(is.na(lat_original) | is.na(long_original), "no_coordinates", "this")) %>% filter(which_dupl == "this")

no_dupl
nrow(si_dupl_fica)
nrow(no_dupl)
no_dupl <- bind_rows(si_dupl_fica, no_dupl)
length(unique(rtf_filt$etiqueta_duplicaDO))
length(unique(no_dupl$etiqueta_duplicaDO))
#no sé...


# rtf_filt %>%
#   filter(categoria_final == 0, T20 == "FICA_ETIQUETA_DUPLICADO_IN_T20") %>%
#   select(starts_with("mun")) %>%
#   select(ends_with("final")) %>%
#   distinct() %>% View()

#discrepancias municipio
rtf_filt <- rtf_filt %>%
  group_by(etiqueta_duplicaDO) %>%
  mutate(fica = if_else(all(municipio_final %in% mpos_t20$municipio_padronizado, na.rm = T), "fica_mesmo", "")) %>%
  mutate(sai = if_else(all(!municipio_final %in% mpos_t20$municipio_padronizado, na.rm = T),
                        "sai_mesmo", "")) %>% ungroup()

rtf_filt %>% filter(sai == "sai_mesmo") %>% write_csv("output/p3_all_T20_out.csv")
rtf_filt_fica <- rtf_filt %>% filter(!sai == "sai_mesmo") #####fica mesmo ou conflito municipio

rtf_filt_fica %>% count(sai, fica)
rtf_filt_fica %>% distinct(etiqueta_duplicaDO, sai , fica) %>% count()
9237-5501
rtf_filt_fica %>% distinct(etiqueta_duplicaDO, sai , fica) %>% count(fica)
FICA_MESMO <- rtf_filt_fica %>% filter(fica == "fica_mesmo")
count(FICA_MESMO, fica)

rtf_NA_ou_disrepancia <- rtf_filt_fica %>% filter(fica == "", sai == "")
rtf_NA_ou_disrepancia <- rtf_NA_ou_disrepancia %>%  #distinct(etiqueta_duplicaDO, municipio_final, fica, sai)
  group_by(etiqueta_duplicaDO) %>%
  mutate(check_municipios = paste(municipio_final, collapse = "-"))
rtf_NA_ou_disrepancia$check_municipios2 <- gsub(pattern = "-NA", replacement = "", x = rtf_NA_ou_disrepancia$check_municipios)
rtf_NA_ou_disrepancia$check_municipios2 <- gsub(pattern = "NA-", replacement = "", x = rtf_NA_ou_disrepancia$check_municipios2)
# tira NAs vamos depender do municipio que tem
rtf_NA_ou_disrepancia %>% ungroup() %>% count(sai, fica)

rtf_NA_ou_disrepancia$tinha_NA <- stringr::str_detect(rtf_NA_ou_disrepancia$check_municipios2, pattern = "-", negate = T)
rtf_NA_ou_disrepancia %>% ungroup() %>% count(tinha_NA)
#perfect 5348 5711
#check_mun2a %>% filter(sai == "", fica == "")
rtf_NA_ou_disrepancia <- rtf_NA_ou_disrepancia %>%
  mutate(fica2 = if_else(check_municipios2 %in% mpos_t20$municipio_padronizado, "fica_mesmo", ""))
rtf_NA_ou_disrepancia %>% ungroup() %>% count(fica2, tinha_NA)
FICA_MESMO2 <- rtf_NA_ou_disrepancia %>% filter(fica2 == "fica_mesmo")
checar_municipios <- rtf_NA_ou_disrepancia %>% filter(fica2 != "fica_mesmo")
checar_municipios %>% ungroup() %>% count(sai, fica, fica2)
checar_municipios$check_municipios2

eti_problem <- checar_municipios %>% select(etiqueta_duplicaDO) %>% distinct()
names(rtf_filt)
filter(rtf_NA_ou_disrepancia, etiqueta_duplicaDO %in% eti_problem$etiqueta_duplicaDO) %>% select(etiqueta_duplicaDO, municipio_final) %>% distinct() %>% View()

any(checar_municipios$etiqueta_duplicaDO %in% FICA_MESMO$etiqueta_duplicaDO)


ungroup() %>% tidyr::separate(., check_municipios2, sep = "-", into = LETTERS[1:26], remove = F)




  mutate(sai2 = if_else(!check_municipios2 %in% mpos_t20$municipio_padronizado, "sai_mesmo", ""))
a <- check_mun3fica %>% count(etiqueta_duplicaDO, check_municipios2, fica, fica2)
distinct(check_mun3fica, etiqueta_duplicaDO)
b <- check_mun3sai %>% count(etiqueta_duplicaDO,check_municipios2, sai, sai2)
full_join(a, b, by = "etiqueta_duplicaDO") %>% count(etiqueta_duplicaDO, fica2, sai2) %>% arrange(desc(n)) %>% mutate(resumen = case_when(sai2 == "" & fica2 == "fica_mesmo" ~"fica"))
check_mun2b
check_mun3 %>% filter(sai2 == "sai mesmo", fica == "")


check_mun2fica <- check_mun %>% ungroup() %>% tidyr::separate(., check_municipios2, sep = "-", into = LETTERS[1:4], remove = F) %>%
  select(etiqueta_duplicaDO, check_municipios2, sai, fica, A, B, C, D) %>% distinct() %>%
  mutate(across(LETTERS[1:4],.fns = function(x) if_else(x %in% mpos_t20$municipio_padronizado, "TRUE",x)))

check_mun2sai <- check_mun %>% ungroup() %>% tidyr::separate(., check_municipios2, sep = "-", into = LETTERS[1:4], remove = F) %>%
  select(etiqueta_duplicaDO, check_municipios2, sai, fica, A, B, C, D) %>% distinct() %>%
  mutate(across(LETTERS[1:4],.fns = function(x) if_else(!x %in% mpos_t20$municipio_padronizado, "FALSE",x)))
