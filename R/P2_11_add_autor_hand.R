library(flora)
sessioninfo::session_info()#checkin flora from ast

p3_selecionadas
base <- readxl::read_xlsx("Base de dados Territorio 20 - 27-04-2021 para revisao 1.xlsx", skip = 1)
count(base, fonte_dados)
especie <- unique(base$especie...6)

autor <- get.taxa(especie, drop = "threat.status") %>% select(original.search, authorship) %>% rename("especie...6" = original.search, autor_nome_cient = authorship)

base <- left_join(base, autor, by = "especie...6") %>% mutate(autor_nome_cient.x = autor_nome_cient.y) %>% rename(autor_nome_cient = autor_nome_cient.x) %>% select(-autor_nome_cient.y)

resumo <- read_csv("output/p2/02_resumo.csv")
sinonimia2 <- left_join(especies, resumo) %>% select(nome_aceito_correto, nomes_originais) %>% rename(sinonimia_2 = nomes_originais) %>% mutate(fonte_sinonimia = "FB2020") %>% rename(especie...6 = nome_aceito_correto)
base <- left_join(base, sinonimia2)
write_csv(base, "Base de dados T20.csv")

t20
library(sf)
read_sf2(base, column = "ellos")



base <- readxl::read_xlsx("data/dados_formatados/p2/Base de dados Territorio 20 - 27-04-2021 - HCL (3).xlsx", guess_max = 100000, skip = 1)

names(base)
count(base, fonte_dados)
especie <- unique(base$especie...6)

autor <- get.taxa(especie, drop = "threat.status") %>% select(original.search, authorship) %>% rename("especie...6" = original.search, autor_nome_cient = authorship)

base <- left_join(base, autor, by = "especie...6") %>% mutate(autor_nome_cient.x = autor_nome_cient.y) %>% rename(autor_nome_cient = autor_nome_cient.x) %>% select(-autor_nome_cient.y)

resumo <- read_csv("output/p2/02_resumo.csv")
sinonimia2 <- left_join(especies, resumo) %>% select(nome_aceito_correto, nomes_originais) %>% rename(sinonimia_2 = nomes_originais) %>% mutate(fonte_sinonimia = "FB2020") %>% rename(especie...6 = nome_aceito_correto)
base <- left_join(base, sinonimia2)
write_csv(base, "Base de dados T20.csv")

t20
library(sf)
read_sf2(base, column = "ellos")


library(dplyr)
base <- readxl::read_xlsx("data/dados_formatados/p2/Base de dados Territorio 20 - 27-04-2021 - HCL (3).xlsx", guess_max = 100000, skip = 1)

cat <- readr::read_csv("data/dados_formatados/p2/Lista de espécies-geral.xlsx - ATUAL -Lista Andrea 859 spp - Produto 2.csv")
cats <- cat %>% select(nome_aceito_correto, starts_with("cat_ameaca")) %>% select(-ends_with("geral"))

cat_ameaca <- readr::read_csv("output/p2/04_cat_ameaca.csv")
cat_ameaca <- cat_ameaca %>% select(one_of(names(cats)))
wcat <- base %>% rename(nome_aceito_correto = especie...6) %>% select(-starts_with("cat_ameaca")) %>% left_join(cat_ameaca)
wcat2 <- base %>% rename(nome_aceito_correto = especie...6) %>% select(-starts_with("cat_ameaca")) %>% left_join(cats)
identical(wcat, wcat2)

cat1 <- wcat %>% select(starts_with("cat_ameaca"))
cat1
cats
setdiff(cat1$cat_ameaca_cr_lac.x, cat2$cat_ameaca_cr_lac.y)

readr::write_csv(wcat, "output/p2/Base_w_cat_ameaca.csv")
