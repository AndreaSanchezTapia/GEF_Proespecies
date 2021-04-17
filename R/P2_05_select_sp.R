library(readxl)
library(dplyr)
library(readr)
#le a tabela revisada com coluna "ameacada"
spp <- read_excel("output/p2/Lista de espécies-geral.xlsx", sheet = 1, na = "NA")
names(spp)
any(is.na(spp$nome_aceito_correto))

spp$nome_aceito_correto[spp$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"

dim(spp)

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


#produto 2
p2 <- readr::read_csv("output/p2/p2_inicial.csv")
names(p2)
dim(p2)
setdiff(entra$nome_aceito_correto, p2$nome_aceito_correto)
setdiff(p2$nome_aceito_correto, entra$nome_aceito_correto)

#cruza
entra
cruza <- read_csv("output/p2/01_cruzam_shapes.csv") %>%
  rename(nome_aceito_correto = sp)

resumo <- p2 %>%
  left_join(notas) %>%
  left_join(cat_ameaca) %>%
  left_join(entra) %>%
  left_join(cruza)
names(resumo)
resumo %>% write_csv("output/p2/02_resumo.csv")

resumo %>% count(cat_ameaca_geral)

resumo %>%
  count(elegivel_produto_1 == "examinar",sum > 0,str_detect(notas_all, "ocorre")) %>% View()
library(stringr)
resumo %>%
  mutate(especialista =
           case_when(
             sum > 0  & str_detect(notas_all, "ocorre") ~ "FBnao- OCCScruzam",
             sum == 0 & elegivel_produto_1 == "apta" ~ "FBsim- OCCSnaocruzam")
         ) %>%
   write_csv("output/p2/03_resumo_anotado.csv")


#a primeira versao do resumo ainda usava spp como base,
#junto <- left_join(spp, resumo)
#write_csv(junto, "output/p2/P1_atualizado.csv")
