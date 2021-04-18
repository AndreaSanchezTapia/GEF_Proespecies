library(readr)
library(knitr)
library(Rocc)
library(stringr)
library(dplyr)
# p2 <- read_csv("output/p2/02_resumo.csv")
# checa as categorias atuais
#  nop <- p2 %>%
#    filter(cat_ameaca_geral == "nao entra" &
#             elegivel_produto_1 != "inapta")
#  nop %>% count(cat_ameaca_iucn)
#
# # #corrige cat_ameaca_br
#  nop %>% count(cat_ameaca_br)#2
#  p2$cat_ameaca_geral[which(p2$elegivel_produto_1 != "inapta" &
#                              p2$cat_ameaca_br  == "EN" &
#                              p2$cat_ameaca_geral == "nao entra")] <- "entra"
#
# nop %>% count(cat_ameaca_cncflora)
# nop %>% count(cat_ameaca_sp)
# nop %>% count(cat_ameaca_mpo_sp)
# nop %>% count(cat_ameaca_cr_lac)
#
#write_csv(p2, "output/p2/03_resumo_anotado.csv")

p2 <- read_csv("output/p2/03_resumo_anotado.csv")

especies <- p2 %>%
  filter(elegivel_produto_1 != "inapta",
         cat_ameaca_geral == "entra") %>%
  pull(nome_aceito_correto)
length(especies)
#tira as que vamos ignorar
ignorar <- str_detect(especies, "subsp\\.") |
  str_detect(especies, "var\\.")
sum(ignorar)
bajar <- especies[ignorar] ####fica anota
especies <- especies[!ignorar]

#tira as que nao tem em SP

no_SP <- p2 %>%
  filter(elegivel_produto_1 == "examinar" &
         ((splink_sp == FALSE & gbif_sp == FALSE) |
         (is.na(splink_sp) & gbif_sp == FALSE) |
         (splink_sp == FALSE & is.na(gbif_sp)) |
           (is.na(splink_sp) & is.na(gbif_sp)))) %>%
  pull(nome_aceito_correto)
no_SP
#1643 especies que eram examinar e saem pelas ocorrencias
#1912!!!!! com nA!
#2065
especies <- especies[!especies %in% no_SP]
length(especies)

setdiff(pat_haroldo, especies)
setdiff(especies, pat_haroldo)

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

write_csv(p2_selecionadas, "output/p2/p2_selecionadas.csv")


p2_selecionadas %>% filter(splink_t20 | gbif_t20) %>%
  write_csv("output/p2/p3_territorio20.csv")

