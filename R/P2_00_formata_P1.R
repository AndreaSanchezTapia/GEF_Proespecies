library(readxl)
library(dplyr)
library(readr)
#le a tabela revisada com coluna "ameacada"
spp <- read_excel("output/p2/Lista de espécies-geral para analise andrea- PR-14-04.xlsx", sheet = 2, na = "NA")
names(spp)
any(is.na(spp$nome_aceito_correto))

spp$nome_aceito_correto[spp$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"

names(spp)

#cats
cat_ameaca <-
  spp %>%
  select(nome_aceito_correto, starts_with("cat_ameaca")) %>%
  count(nome_aceito_correto, cat_ameaca_cr_lac, cat_ameaca_iucn, cat_ameaca_mpo_sp, cat_ameaca_br,    cat_ameaca_cncflora, cat_ameaca_sp) %>%
  group_by(nome_aceito_correto) %>%
  mutate(across(starts_with("cat_ameaca"), .fns = function(x) paste(x, collapse = "-"))) %>%
  arrange(nome_aceito_correto) %>%
  ungroup() %>%
    select(-n) %>%
  distinct()

readr::write_csv(cat_ameaca, "output/p2/04_cat_ameaca.csv")

#toca crear aqui cat_geral

CR <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "CR"), .names = "{.col}_CR")) %>%
  select(ends_with("CR")) %>% rowSums(na.rm = T)
EN <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "EN"), .names = "{.col}_EN")) %>%
  select(ends_with("EN")) %>% rowSums(na.rm = T)
VU <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "VU"), .names = "{.col}_VU")) %>%
  select(ends_with("VU")) %>% rowSums(na.rm = T)
DD <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "DD"), .names = "{.col}_DD")) %>%
  select(ends_with("DD")) %>% rowSums(na.rm = T)
EX <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "EX"), .names = "{.col}_EX")) %>%
  select(ends_with("EX")) %>% rowSums(na.rm = T)
NT <- cat_ameaca %>% mutate(across(-1, ~stringr::str_detect(.x,
                                                     pattern = "NT"), .names = "{.col}_NT")) %>%
  select(ends_with("NT")) %>% rowSums(na.rm = T)


cat_ameaca_geral <- tibble(nome_aceito_correto = cat_ameaca$nome_aceito_correto, CR, EN, VU, DD, EX, NT)
cat_ameaca_geral <- cat_ameaca_geral %>%
  rowwise %>%
  mutate(cat_ameaca_geral = sum(across(-1))) %>%
  mutate(across(-1, ~if_else(.x > 0, 1, 0)))
cat_ameaca_geral %>%
  write_csv("output/p2/04_cat_ameaca_geral.csv")
#old <- read_csv(file = "output/p2/03_entra.csv")
#count(old, cat_ameaca_geral)
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


names(spp)
tax <- spp %>%
  select(c(1:3, 6:10)) %>%
  group_by(nome_aceito_correto) %>%
  count(across(everything())) %>%
  arrange(desc(n)) %>%
  select(-n) %>%
  distinct()
tax %>% View()
write_csv(tax, file = "output/p2/10_base_P1.csv")

fontes <- spp %>%
  select(nome_aceito_correto, fontes) %>%
    group_by(nome_aceito_correto) %>%
  mutate(fontes = paste(fontes, collapse = "-")) %>%
  count(across(everything())) %>% select(-n)
  fontes
  write_csv(fontes, file = "output/p2/12_fontes.csv")
