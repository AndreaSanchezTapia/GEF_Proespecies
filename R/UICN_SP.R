library(tidyverse)
library(readr)
SP_UICN_Flora <- read_csv("./data/UICN_SP_flora.csv")
glimpse(SP_UICN_Flora)
count(SP_UICN_Flora, taxon.status)
count(SP_UICN_Flora, notes)
names(SP_UICN_Flora)
Resumo <- data.frame(
  Nome_original = SP_UICN_Flora$search.str,
  Nome = SP_UICN_Flora$scientific.name,
  Notas = SP_UICN_Flora$notes)
View(Resumo)
Resumo %>%  count(is.na(Nome_aceito))
