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
p1 <- read_xlsx("data/dados_formatados/produto1/produto1_Lista_de_espécies.xlsx",
                 sheet = 2)

#formata lista com os nomes bons
p2 <- p1 %>%
  select(especie_original, nome_aceito_correto, fontes, elegivel, notas) %>%
         group_by(nome_aceito_correto) %>%
  mutate(
    nomes_originais = paste(especie_original, collapse = "-"),
    elegivel_originais = paste(elegivel, collapse = "-"),
         fontes_originais = paste(fontes, collapse = "-")) %>%
  select(-especie_original, fontes, -elegivel) %>%
  distinct() %>%
  ungroup() %>%
  #isto eh feio para caramba
  mutate(elegivel_originais = case_when(
    elegivel_originais %in% c("apta",
                              "apta-apta",
                              "apta-apta-apta",
                              "apta-apta-apta-apta",
                              "apta-apta-apta-apta-apta") ~ "apta",
    elegivel_originais %in% c("apta-examinar", "examinar-apta") ~ "apta-examinar",
    stringr::str_detect(elegivel_originais, pattern = "inapta") ~ "inapta",
    elegivel_originais %in% c("examinar",
                              "examinar-examinar",
                              "examinar-examinar-examinar") ~ "examinar")
) %>%  distinct()
p2
count(p2, elegivel_originais)

dir.create("output/p2")
#write_csv(p2, "output/p2/p2_inicial.csv")
write_csv(p2, "output/p2/p2_segundo.csv")
count(p2, elegivel_originais)



p2 %>%
  filter(elegivel_originais != "inapta") %>%
  select(elegivel_originais, notas) %>%
  count(elegivel_originais, notas) %>%
  View()

# checar sinonimos aptas

especies <- p2 %>%
  filter(elegivel_originais != "inapta") %>%
  #select(nome_aceito_correto) %%
  filter(!is.na(nome_aceito_correto)) %>%
  distinct(nome_aceito_correto) %>%
  pull()
length(especies)

ignorar <- str_detect(especies, "subsp.") | str_detect(especies, "var.")

especies <- especies[!ignorar]
#get_sinonimos <- purrr::map(especies,
 #                           ~check_flora(.x, get_synonyms = T))

library("furrr")
plan(multisession, workers = 14)
especies
#cria funcao que vai salvar no hd o resultado de check_flora para nao repetirmos a busca
save_check <- function(x) {
  if (!file.exists(fs::path("output/p2/check_flora", x, ext = "rda"))) {
  a <- Rocc::check_flora(x, get_synonyms = T)
  save(a,
       file = fs::path("output/p2/check_flora", x, ext = "rda"))
  return(a)
  } else {
  message("file already in disk")
}}
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
furtest <- furrr::future_map(again,
                             .progress = T,
                             ~save_check(.x))
#o seguinte passo vai ser 1. carregar
load("output/p2/check_flora/Abarema acreana.rda")
#2. renomear
assign(ya[1], a)
rm(a)
#decidir se append ou conerter a dataframe
res <- get(ya[1])
#3.
res$taxon$verbatimSpecies == unique(res$synonyms$species_base)

lista_completa <- append(get_sinonimos, get_sinonimos_examinar)
syn_todas <- purrr::map(lista_completa,
                   ~.x$synonyms)
tax_todas <- purrr::map(lista_completa,
                   ~.x$taxon)
tax_syn_yes_no <- purrr::map(tax_todas,
                             ~unique(.x$synoyms))

especies_todas <- purrr::map(syn_todas,
                             ~unique(.x$species_base))
especies_todas <- unlist(especies_todas)

unlist(Rocc::check_string(syn2[[13]]$scientificName)$species)
sin_df3 <- purrr::map(syn2[[13]],
                     ~unlist(Rocc::check_string(.x$scientificName)$species))

sin_df <- purrr::map(syn2,
                     ~data.frame(
  especie = .x$species_base,
  syn = .x$scientificName,
  ))

write.csv(data.frame(nome = sp, sinonimos = syn), file = paste0("output/p2/", unique(sp), ".csv"))

install.packages("furrr")

sinonimos_df <- function(data) {

}
