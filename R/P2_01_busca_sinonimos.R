#busca sinonimos
remotes::install_github("liibre/Rocc")
library(Rocc)
library(readr)
library(stringr)
library(readxl)
library(dplyr)

p1 <- read_xlsx("data/dados_formatados/produto1/produto1_Lista_de_espécies.xlsx",
                 sheet = 2)
count(p1, notas)
count(p1, elegivel)
count(p1, notas, elegivel) %>% View()
