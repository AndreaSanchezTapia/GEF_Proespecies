library(readr)
library(dplyr)
library(stringr)

#lee las tablas de ocurrencias
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas$nome_aceito_correto
occs <- list.files("output/p2/occs",recursive = T, full.names = T, pattern = ".csv")
#checar arquivos vazios
empty <- file.info(occs)[["size"]] == 0
which(empty)
occs[which(empty)]
#unlink(empty)

gbif <- occs[stringr::str_detect(occs, "splink", negate = T)]
splink <- occs[stringr::str_detect(occs, "gbif", negate = T)]

#seleciona as limpas de gbif e as nao limpas de specieslink
gbif <- gbif[gbif %>% str_detect("clean")] #ya son 786
sp_gb <- basename(gbif) %>%
  stringr::str_remove("_clean") %>%
  stringr::str_remove(".csv")
sel_gb <- gbif[sp_gb %in% especies]
identical(sel_gb, gbif)

splink <- splink[splink %>% str_detect("clean", negate = T)]
sp_sl <- basename(splink) %>%
  stringr::str_remove(".csv")
sel_sl <- splink[sp_sl %in% especies]

nomes_splink <- basename(sel_sl) %>% stringr::str_remove(".csv")
nomes_gbif   <- basename(sel_gb)   %>% stringr::str_remove("_clean.csv")
names(sel_sl) <- nomes_splink
names(sel_gb) <- nomes_gbif
occs <- tibble(sp = especies) %>%
  full_join(tibble(sp = nomes_gbif, gbif_file = sel_gb)) %>%
  full_join(tibble(sp = nomes_splink, splink_file = sel_sl), by = "sp") %>%
  arrange(sp)
occs
head(occs) %>% View()
write_csv(occs, "output/p2/06_occ_files.csv")
#library("remotes")
#install_github("LimaRAF/plantR")
#library(plantR)
#devtools::load_all("../../R_packages/plantR/")

pasta_all <- fs::path("output/p2/t20_raw")
dir.create(pasta_all)
#then with furrr
library(furrr)

plan(multisession, workers = 15)
library(dplyr)
sp <- occs[1,1]
gbb <- occs[1,2]
spp <- occs[1,3]
furrr::future_pmap(occs, ~{
  sp <- ..1
  gbb <- ..2
  spp <- ..3
  file <- fs::path(pasta_all, sp, ext = "csv")
  sl_file <- fs::path(pasta_all, paste0(sp, "_splink"), ext = "csv")
  gb_file <- fs::path(pasta_all, paste0(sp, "_gbif_c"), ext = "csv")

  if (!is.na(spp)) {
    spl <- vroom::vroom(spp, guess_max = 100000) %>%
      mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
      mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
      write_csv(sl_file)

  } #else spl <- NULL
  if (!is.na(gbb)) {
    gbf <- vroom::vroom(gbb, guess_max = 100000) %>%
      mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
      mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
      write_csv(gb_file)
  } else gbf <- NULL
  if (!file.exists(file))
 tryCatch({all <- formatDwc(splink_data = spl,
                             gbif_data = gbf,
                             drop.empty = T)
      write_csv(all, file = file)
  },
  error = function(e) {
    message(paste(sp, "problem"))
    message(e)
  })

}, .progress = TRUE)

plan(sequential)
