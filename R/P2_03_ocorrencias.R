#selecionar especies
library(dplyr)
library(readr)

p2 <- read_csv("output/p2/03_resumo_anotado.csv")
entra <- p2 %>% filter(cat_ameaca_geral == "entra") %>% select(nome_aceito_correto, cat_ameaca_geral)
