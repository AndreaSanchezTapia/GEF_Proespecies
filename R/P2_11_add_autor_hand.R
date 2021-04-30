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
