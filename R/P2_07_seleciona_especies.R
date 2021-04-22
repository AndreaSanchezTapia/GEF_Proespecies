library(readr)
library(knitr)
library(Rocc)
library(stringr)
library(dplyr)
# checa entra atual
library(readxl)
library(dplyr)
library(readr)

#le a lista velha
lista_velha <- read_excel("data/dados_formatados/p2/Lista de espécies-geral.xlsx", sheet = 2, na = "NA")

p2 <- read_csv("output/p2/02_resumo.csv")

p2_new <- p2 %>% #5625
  filter(elegivel_produto_1 != "inapta") %>%   #5607
  filter(cat_ameaca_geral > 0) %>% #3659
  filter(gbif_t20 | splink_t20) %>% #917
filter(entra_revisado != "NAO" | is.na(entra_revisado)) #855
p2_new

final_them <- lista_velha %>%
  rename(entra_revisado = `NOVA LISTA - 20-04`) %>%
  filter(entra_revisado == "SIM")

faltam <- final_them$nome_aceito_correto[which(!final_them$nome_aceito_correto %in% p2_new$nome_aceito_correto)]

#as que faltam tem ococrrencias?
gb <- basename(list.files("output/p2/occs/gbif/")) %>% stringr::str_remove(".csv")
sl <- basename(list.files("output/p2/occs/splink/")) %>% stringr::str_remove(".csv")
faltam %in% gb
faltam %in% sl

especies <- p2_new$nome_aceito_correto

p2 %>%
filter(nome_aceito_correto %in% especies) %>%
  count(splink_sp | gbif_sp, elegivel_produto_1, cat_ameaca_geral)



  p2_selecionadas <- p2 %>%
  filter(nome_aceito_correto %in% especies)
write_csv(p2_selecionadas, "output/p2/p2_selecionadas.csv")
names(p2_selecionadas)
p2_selecionadas %>%
  count(elegivel_produto_1, cat_ameaca_geral, gbif_sp, splink_sp) %>% arrange(desc(n))
#### hasta aqui seleccion

#selecion por territorio 20

p2_selecionadas %>% filter(splink_t20 | gbif_t20) %>%
  write_csv("output/p2/p3_territorio20.csv")

