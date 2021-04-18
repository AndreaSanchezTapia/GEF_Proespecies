library(readr)
library(dplyr)
library(stringr)

#lee las tablas de ocurrencias
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas$nome_aceito_correto
occs <- list.files("output/p2/t20_raw/",recursive = T, full.names = T, pattern = ".csv")
library(furrr)
plan(multisession, workers = 10)
read_raw2 <- furrr::future_map(occs,
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
nrow_raw <- furrr::future_map(read_raw2, ~nrow(.x), .progress = T)
names_raw <- furrr::future_map(read_raw2, ~names(.x), .progress = T)
todos_os_nomes <- simplify(names_raw) %>% unique() %>% sort()

plan(sequential)


#checar arquivos vazios

gbif <- occs[stringr::str_detect(occs, "splink", negate = T)]
nomes_gbif   <- basename(gbif)   %>% stringr::str_remove("_gbif_c.csv")
splink <- occs[stringr::str_detect(occs, "gbif", negate = T)]
nomes_splink <- basename(splink) %>% stringr::str_remove("_splink.csv")

names(gbif) <- nomes_gbif
names(splink) <- nomes_splink

oc_df <- tibble(sp = especies) %>%
  full_join(tibble(sp = nomes_gbif, gbif_file = gbif)) %>%
  full_join(tibble(sp = nomes_splink, splink_file = splink), by = "sp") %>%
  arrange(sp)
oc_df
head(oc_df)
#library("remotes")
#install_github("LimaRAF/plantR")
#library(plantR)
devtools::load_all("../../R_packages/plantR/")

pasta_all <- fs::path("output/p2/t20_join")
dir.create(pasta_all)
#then with furrr
library(furrr)

plan(multisession, workers = 15)
library(dplyr)
sp <- oc_df[1,1]
gbb <- oc_df[1,2]
spp <- oc_df[1,3]

#furrr::future_pmap(oc_df, ~{
sp <- ..1
gbb <- ..2
spp <- ..3
file <- fs::path(pasta_all, sp, ext = "csv")

if (!is.na(spp)) {
  spl <- vroom::vroom(spp, guess_max = 100000) %>%
    mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.double), .fns = function(x) as.character(x)))

} else spl <- NULL
if (!is.na(gbb)) {
  gbf <- vroom::vroom(gbb, guess_max = 100000) %>%
    mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.double), .fns = function(x) as.character(x)))
} else gbf <- NULL
names(gbf) %in% names(spl)
names(spl) %in% names(gbf)

#  if (!file.exists(file))
tryCatch({all <- formatDwc(splink_data = spl,
                           gbif_data = gbf,
                           drop.empty = T)
#      write_csv(all, file = file)
#  },
#  error = function(e) {
#    message(paste(sp, "problem"))
#    message(e)
#  })

}, .progress = TRUE)

plan(sequential)


raw <- list.files(pasta_all) %>%
  basename() %>%
  str_remove(".csv") %>%
  str_remove("_gbif_c") %>%
  str_remove("_splink") %>% unique()
setdiff(especies, raw)
zipfile <- 't20_raw.zip'
zip(zipfile, 'output/p2/t20_raw/')

lista_completa <- furrr::future_map2(spp_files, spp, ~load_and_rename(.x, .y))
lista_completa[1]
names(lista_completa) <- spp


#extrai a tabela de sinonimos, pode ser NULL
syn_todas <- furrr::future_map(lista_completa,
                               ~.x$synonyms,
                               .progress = T)
