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
count(p1, especie_original, nome_aceito_correto) %>% arrange(desc(n))
p1 %>% View()

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
#    elegivel_originais %in% c("apta-examinar", "examinar-apta") ~ "apta-examinar",
    stringr::str_detect(elegivel_originais, pattern = "inapta") ~ "inapta",
    elegivel_originais %in% c("examinar",
                              "examinar-examinar",
                              "examinar-examinar-examinar") ~ "examinar")
) %>%  distinct()

count(p2, elegivel_originais)

#primeira passada> tem 3056 especies que aparecem uma vez soh, aptas, 329+66+6+3 especies que aparecem varias vezes, sempre aptas
#8 e 4 que aparecem apta-examinar (sinonimos?)
#2321 52 e 1 examinar
#inapta always inapta, cool
#segunda passada com case_when 3460 aptas, 2374 examinar, 12 apta-examinar, 16
 inapta
dir.create("output/p2")
write_csv(p2, "output/p2/p2_inicial.csv")

count(p2, elegivel_originais)



p2 %>%
  filter(is.na(elegivel_originais)) %>%
  select(elegivel_originais, notas)


# checar sinonimos aptas
library(Rocc)
library(purrr)
p2 %>% count(elegivel_originais)
especies <- p2 %>%
  filter(elegivel_originais == "apta") %>%
  select(nome_aceito_correto) %>%
  distinct() %>% pull()

ignorar <- str_detect(especies, "subsp.") | str_detect(especies, "var.")

especies <- especies[!ignorar]

get_sinonimos <- list()
for (i in seq_along(especies)) {
  get_sinonimos[[i]] <- check_flora(especies[i], get_synonyms = T)
  print(i)
}
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


#saca la tbla de sinonimos
syn2 <- purrr::map(get_sinonimos,
           ~.x$synonyms)
dims <- purrr::map(syn2,
           ~nrow(.x))
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
