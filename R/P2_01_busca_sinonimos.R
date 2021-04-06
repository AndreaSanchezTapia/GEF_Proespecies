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
plan(multisession, workers = 3)
test <- especies[1:10]
furtest <- furrr::future_map(test,.progress = T,
                      ~check_flora(.x, get_synonyms = T))

length(get_sinonimos)
#2492 e 2493
for (i in 2494:length(especies)) {
  get_sinonimos[[i]] <- check_flora(especies[i], get_synonyms = T)
  print(i)
}
i

for (i in 2678:length(especies)) {
  get_sinonimos[[i]] <- check_flora(especies[i], get_synonyms = T)
  print(i)
}
for (i in 3202:length(especies)) {
  get_sinonimos[[i]] <- check_flora(especies[i], get_synonyms = T)
  print(i)
}
especies[3201]
get_sinonimos[3201]
#resolvendo check_flora para rodar as que faltaram
source("../../R_packages/Rocc/R/check_flora.R")
for (i in c(2492, 2493, 2677, 3200, 3201)) {
    get_sinonimos[[i]] <- check_flora(especies[i], get_synonyms = T)
  print(i)
}
especies[c(2492, 2493, 2677, 3200, 3201)]

glimpse(get_sinonimos)

#ah mas eu deveria ter escrito os arquivos, duh
#por enquanto salvo a lista
for (i in seq_along(get_sinonimos)) {
get_sinonimos[[1]]$taxon
}
save(get_sinonimos, file = "output/p2/get_sinonimos_inicial.rda")

load("output/p2/get_sinonimos_inicial.rda")
length(get_sinonimos)
length(especies)

#saca la tbla de sinonimos
syn2 <- purrr::map(get_sinonimos,
           ~.x$synonyms)
length(get_sinonimos)
especies_antes <- purrr::map(syn2,
           ~unique(.x$species_base))
especies_antes <- unlist(especies_antes)
a <- setdiff(especies, especies_antes)
b <- setdiff(especies_antes, especies)
especies_faltantes <- c(a, b)
get_sinonimos_examinar <- list()
for (i in seq_along(especies_faltantes)) {
  get_sinonimos_examinar[[i]] <- check_flora(especies_faltantes[i], get_synonyms = T)
  print(i)
}
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

length(lista_completa)
length(syn_todas)
length(especies_todas)

dims <- unlist(dims)
head(dims)
which(dims ==9)
syn2[which(unlist(dims) ==1)]
nrow(syn2[[9]])
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
