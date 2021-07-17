#library("googlesheets4")
library(dplyr)
library(readxl)
#googlesheets4::gs4_browse(ss = )
#dados_p4 <- read_sheet(ss = "https://docs.google.com/spreadsheets/d/1qkaAzzlEM0XlryckvcKFIg37QY1Ng0U-/edit#gid=1564349686", sheet = 3)

dados <- read_excel("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx", sheet =2)
names(dados)
nrow(dados)
count(dados, IPF)
count(dados, `NOVA AVALIACÃO`)

new_spp <- dados %>%
#  filter(`NOVA AVALIACÃO` == "NAO - conferir - se apta - incluir") %>%
  pull(especie)

library(flora)
tudo <- read_csv("output/p1/08_lista_categoria_atualizada.csv") %>%
  janitor::clean_names()
nrow(tudo)
any(new_spp %in% tudo$nome_aceito_correto)
new_spp[which(new_spp %in% tudo$nome_aceito_correto)]
notas <- filter(tudo, nome_aceito_correto %in% new_spp) %>%
  filter(!is.na(nome_aceito_correto)) %>%
  select(nome_aceito_correto, apta, fontes, notas) %>%
  rename(especie = nome_aceito_correto)
dir.create("output/p4")
left_join(dados, notas) %>% write_csv("output/p4/01_notas_apta.csv")



library(flora)
get.taxa(new_spp , drop = NULL) %>% write_csv("output/p4/02_new_spp_flora.csv")
