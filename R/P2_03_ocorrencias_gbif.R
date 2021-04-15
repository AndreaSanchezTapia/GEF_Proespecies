# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
library(furrr)

gbif <- "output/p2/gbif/"
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)

nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
names(df_sinonimos) <- nomes
#para cada especie
pasta_occs <- "output/p2/occs/"

plan(multisession, workers = 15)
df <- furrr::future_map(df_sinonimos, ~read_csv(.x), .progress = T)
#plan(sequential)

pasta_out <- paste0(pasta_occs, nomes)
#purrr::map(pasta_out, ~dir.create(.x, recursive = T))
length(df)
names(df)

#baja ocurrencias de species link

download_rgbif <- function(oc) {
  sp <- unique(oc$especie)
  pasta_out <- paste0(pasta_occs, sp, "/gbif/")
  dir.create(pasta_out, recursive = T)
  search <- oc$nomes#[stringr::str_detect(string = df[[i]]$nomes, "var\\.", negate = T)]

      if (!file.exists(fs::path(pasta_out, sp, ext = "csv"))) {
      key <- purrr::map(search, ~rgbif::name_backbone(name = .x))
  key2 <- purrr::map(key,
                     ~select(.x, one_of(c("acceptedUsageKey", "usageKey"))))
  key3 <- modify_if(key2, .p = ~ncol(.x) > 0, .f = ~select(.x, 1))
  keys_final <- bind_rows(key3) %>% tidyr::unite("key", na.rm = T) %>%
    mutate(key = as.numeric(key)) %>% na.omit() %>% distinct() %>% pull()
  message(paste(sp, keys_final))

  out <- tryCatch (
    {
    res_gbif <- purrr::map(keys_final,
                         ~rgbif::occ_search(taxonKey = .x,
                                     limit = 100000)$data)
  res <- bind_rows(res_gbif)
  write_csv(res, fs::path(pasta_out, sp, ext = "csv"))
  print(sp)
    },
  error = function(e) {
    message(paste(sp, key, "problem"))
    message(e)
  },
  warning = function(w) {
    message(paste(sp, key, "caused a warning"))
    message(w)
  })
  return(out)
      }
  }

#plan(multisession, workers = 15)
gf <- furrr::future_map(df, ~download_rgbif(.x), .progress = T)
plan(sequential)

