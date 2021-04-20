#busca sinonimos
#versão nova
remotes::install_github("liibre/Rocc")

library(Rocc)
library(readr)
library(stringr)
library(readxl)
library(dplyr)
library(purrr)

#le tabela produto 1
p1 <- read_xlsx("output/p2/Lista de espécies-geral para analise andrea- PR-14-04.xlsx",
                 sheet = 2)
p1$nome_aceito_correto[p1$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"
names(p1)
p1 %>% arrange(grupo, familia, especie_original) %>% write_csv("output/p2/p1_final.csv")
#formata lista com os nomes bons
p2 <- p1 %>%
  rename(elegivel = `elegivel - Produto 1`) %>%
  select(especie_original,
         nome_aceito_correto,
         fontes,
         elegivel
         #`elegivel - Produto 2`,
         #notas
         ) %>%
         group_by(nome_aceito_correto) %>%
  mutate(
    nomes_originais = paste(especie_original, collapse = "-"),
    elegivel_produto_1 = paste(elegivel, collapse = "-"),
    #elegivel_produto_2 = paste(`elegivel - Produto 2`, collapse = "-"),
    #notas = paste(notas, collapse = "-"),
    fontes_originais = paste(fontes, collapse = "-")) %>%
  select(-especie_original, fontes, -elegivel) %>%
  distinct() %>%
  ungroup()
  #isto eh feio para caramba-
p2 <- p2 %>% mutate(elegivel_produto_1 = case_when(
    elegivel_produto_1 %in% c("apta",
                              "apta-apta",
                              "apta-apta-apta",
                              "apta-apta-apta-apta",
                              "apta-apta-apta-apta-apta") ~ "apta",
    elegivel_produto_1 %in% c("apta-examinar", "examinar-apta") ~ "examinar",
    stringr::str_detect(elegivel_produto_1, pattern = "inapta") ~ "inapta",
    elegivel_produto_1 %in% c("examinar",
                              "examinar-examinar",
                              "examinar-examinar-examinar") ~ "examinar")
) %>%
  select(-fontes_originais, -fontes ) %>%
  distinct()
names(p2)
p2 %>% count(elegivel_produto_1)
length(unique(p2$nome_aceito_correto))

count(p2, elegivel_originais)




dir.create("output/p2")
write_csv(p2, "output/p2/p2_inicial.csv")

count(p2, elegivel_originais)



p2 %>%
  filter(elegivel_originais != "inapta") %>%
  select(elegivel_originais, notas) %>%
  count(elegivel_originais, notas) %>%
  View()

# checar sinonimos aptas

especies <- p2 %>%
  filter(elegivel_produto_1 != "inapta") %>%
  #select(nome_aceito_correto) %%
  filter(!is.na(nome_aceito_correto)) %>%
  distinct(nome_aceito_correto) %>%
  pull()
length(especies)

ignorar <- str_detect(especies, "subsp\\.") | str_detect(especies, "var\\.")

sum(ignorar)

#get_sinonimos <- purrr::map(especies,
 #                           ~check_flora(.x, get_synonyms = T))

library("furrr")
plan(multisession, workers = 15)
especies
#cria funcao que vai salvar no hd o resultado de check_flora para nao repetirmos a busca
source("../../../Andre/R_packages/Rocc/R/check_flora.R")
save_check <- function(x) {
  #if (!file.exists(fs::path("output/p2/check_flora", x, ext = "rda"))) {
  a <- check_flora(x, get_synonyms = T, infraspecies = T)
  save(a,
       file = fs::path("output/p2/check_flora", x, ext = "rda"))
  return(a)
  #} else {
  #message("file already in disk")
#}
}
#o objto ficou salvo como a, vai ter que rolar um rename com assign

furtest <- furrr::future_map(especies,
                             .progress = T,
                      ~save_check(.x))
plan(sequential)

#checar o que rodou
ya <- list.files("output/p2/check_flora/")
ya <- stringr::str_remove(ya, "\\.rda$")
#para ver o que precisa rodar de novo... istodeveria ser com um tryCatch mas sem tempo
again <- setdiff(especies, ya)

furtest <- furrr::future_map(again[1],
                             .progress = T,
                             ~save_check(.x))
