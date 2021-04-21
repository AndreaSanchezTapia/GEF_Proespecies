library(readxl)
library(dplyr)
library(readr)
#le a tabela revisada com coluna "ameacada"
spp <- read_excel("output/p2/Lista de espécies-geral para analise andrea- PR-14-04.xlsx", sheet = 2, na = "NA")
names(spp)
any(is.na(spp$nome_aceito_correto))

spp$nome_aceito_correto[spp$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"

names(spp)


#cria "entra" para saber quem vai de fato e organiza por nomes_aceito_correto
  entra <- spp %>%
  select(nome_aceito_correto, `ENTRA OU NAO`) %>%
  count(nome_aceito_correto, `ENTRA OU NAO`) %>%
  group_by(nome_aceito_correto) %>%
  mutate(ameacada = paste(`ENTRA OU NAO`, collapse = "-")) %>%
  arrange(nome_aceito_correto) %>%
  ungroup() %>%
  #count(ameacada)
  mutate(cat_ameaca_geral = if_else(ameacada %in% c("AMEACADA", "AMEACADA-NAO"), "entra", "nao entra")) %>%
  select(nome_aceito_correto, cat_ameaca_geral) %>%
  distinct()
entra
readr::write_csv(entra, "output/p2/03_entra.csv")

#cats
#cria "entra" para saber quem vai de fato e organiza por nomes_aceito_correto
cat_ameaca <-
  spp %>%
  select(nome_aceito_correto, starts_with("cat_ameaca")) %>%
  count(nome_aceito_correto, cat_ameaca_cr_lac, cat_ameaca_iucn, cat_ameaca_mpo_sp, cat_ameaca_br,    cat_ameaca_cncflora, cat_ameaca_sp) %>%
  group_by(nome_aceito_correto) %>%
  mutate(across(starts_with("cat_ameaca"), .fns = function(x) paste(x, collapse = "-"))) %>%
  arrange(nome_aceito_correto) %>%
  ungroup() %>%
  #  count(across(starts_with("cat_ameaca"))) %>% arrange(desc(n))
  #mutate(cat_ameaca_geral = if_else(ameacada %in% c("AMEACADA", "AMEACADA-NAO"), "entra", "nao entra")) %>%
  #select(nome_aceito_correto, cat_ameaca_geral) %>%
    select(-n) %>%
  distinct()

readr::write_csv(cat_ameaca, "output/p2/04_cat_ameaca.csv")

notas <- spp %>%
  select(nome_aceito_correto, notas) %>%
  count(nome_aceito_correto, notas) %>%
  #arrange(desc(n)) %>% View()
  group_by(nome_aceito_correto) %>%
  mutate(notas_all = paste(notas, collapse = "-")) %>%
  arrange(nome_aceito_correto) %>%
  ungroup() %>%
  select(nome_aceito_correto, notas_all) %>%
  distinct()
readr::write_csv(notas, "output/p2/05_notas.csv")



tax <- spp %>%
  select(1:10) %>%
  #group_by(nome_aceito_correto) %>%
  count(across(everything())) %>%
  #arrange(desc(n))
  select(-n) %>%
  distinct()
write_csv(tax, file = "output/p2/10_base_P1.csv")
fontes <- spp %>%
  select(nome_aceito_correto, fontes) %>%
    group_by(nome_aceito_correto) %>%
  mutate(fontes = paste(fontes, collapse = "-")) %>%
  count(across(everything())) %>% select(-n)
  fontes
  write_csv(fontes, file = "output/p2/12_fontes.csv")
