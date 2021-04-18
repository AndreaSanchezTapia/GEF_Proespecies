library(readr)
library(dplyr)
#lee las tablas de ocurrencias
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas$nome_aceito_correto
occs <- list.files("output/p2/occs",recursive = T, full.names = T, pattern = ".csv")
occs <- occs[occs %>% str_detect("clean")]
#checar arquivos vazios
empty <- file.info(occs)[["size"]] == 0
which(empty)
occs[which(empty)]
#unlink(empty)
oc_sp <- basename(occs) %>% stringr::str_remove("_clean.csv")
selecionadas <- occs[oc_sp %in% especies]
gbif <- selecionadas[stringr::str_detect(selecionadas, "splink", negate = T)]
splink <- selecionadas[stringr::str_detect(selecionadas, "gbif", negate = T)]

nomes_splink <- basename(splink) %>% stringr::str_remove("_clean.csv")
nomes_gbif   <- basename(gbif)   %>% stringr::str_remove("_clean.csv")
names(splink) <- nomes_splink
names(gbif) <- nomes_gbif
occs <- tibble(sp = especies) %>%
  full_join(tibble(sp = nomes_gbif, gbif_file = gbif)) %>%
  full_join(tibble(sp = nomes_splink, splink_file = splink), by = "sp") %>%
  arrange(sp)
occs
head(occs) %>% View()
write_csv(occs, "output/p2/06_occ_files.csv")
#library("remotes")
#install_github("LimaRAF/plantR")
#library(plantR)
devtools::load_all("../../R_packages/plantR/")

pasta_all <- fs::path("output/p2/t20_clean")
dir.create(pasta_all)
#then with furrr
library(furrr)

plan(multisession, workers = 15)
library(dplyr)


j <- length(list.files(pasta_all))
j

furrr::future_pmap(occs[2,], ~{
  sp <- ..1
  gbb <- ..2
  spp <- ..3

  if (!is.na(spp)) {
    spl <- vroom::vroom(spp, guess_max = 100000) %>%
      mutate(across(where(is_integer), .fns = function(x) as.character(x))) %>%
      mutate(across(where(is_double), .fns = function(x) as.character(x))) %>%
      data.frame()

  } else spl <- NULL
  if (!is.na(gbb)) {
    gbf <- vroom::vroom(gbb, guess_max = 100000) %>%
      mutate(across(where(is_integer), .fns = function(x) as.character(x))) %>%
      mutate(across(where(is_double), .fns = function(x) as.character(x))) %>%
      data.frame()
  } else gbf <- NULL
  tryCatch({all <- formatDwc(splink_data = spl,
                             gbif_data = gbf,
                             drop.empty = T)
  file <- fs::path(pasta_all, sp, ext = "csv")
  if (!file.exists(file)) {
    write_csv(all, file = file)
  }
  },
  error = function(e) {
    message(paste(sp, "problem"))
    message(e),
  })
}, .progress = TRUE)

plan(sequential)
