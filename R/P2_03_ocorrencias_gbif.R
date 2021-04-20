# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
library(furrr)
library(purrr)


df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
names(df_sinonimos) <- nomes
syn_df <- tibble(nome_aceito_correto = nomes,
       sinonimo_file = df_sinonimos)
#para cada especie
entra <- read_csv("output/p2/p2_inicial.csv") %>% select(nome_aceito_correto, cat_ameaca_geral)
syn_df <- left_join(entra, syn_df)

pasta_occs <- "output/p2/occs/"

plan(multisession, workers = 15)
df <- furrr::future_map(syn_df$sinonimo_file, ~read_csv(.x), .progress = T)

#plan(sequential)
pasta_out <- paste0(pasta_occs, "gbif")
dir.create(pasta_out)
df

download_rgbif <- function(oc) {
  sp <- unique(oc$especie)
  search <- oc$nomes#[stringr::str_detect(string = df[[i]]$nomes, "var\\.", negate = T)]

      if (!file.exists(fs::path(pasta_out, sp, ext = "csv"))) {
      key <- purrr::map(search, ~rgbif::name_backbone(name = .x))
  key2 <- purrr::map(key,
                     ~select(.x, any_of(c("acceptedUsageKey", "usageKey"))))
  key3 <- modify_if(key2, .p = ~ncol(.x) > 0, .f = ~select(.x, 1))
  keys_final <- bind_rows(key3) %>% tidyr::unite("key", na.rm = T) %>%
    mutate(key = as.numeric(key)) %>% na.omit() %>% distinct() %>% pull()
  message(paste(sp, keys_final))

  out <- tryCatch (
    {
    res_gbif <- purrr::map(keys_final,
                         ~rgbif::occ_search(taxonKey = .x, basisOfRecord = "PRESERVED_SPECIMEN",#SHOULD HAVE
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
hay <- list.files(pasta_out) %>% stringr::str_remove(".csv")
setdiff(hay, syn_df$nome_aceito_correto)
falta <- setdiff(syn_df$nome_aceito_correto, hay)
falta_df <- syn_df %>% filter(nome_aceito_correto %in% falta)
df_falta <- furrr::future_map(falta_df$sinonimo_file, ~read_csv(.x), .progress = T)
plan(multisession, workers = 15)
gf <- furrr::future_map(df_falta, ~download_rgbif(.x), .progress = T)
future::plan(sequential)


#filtra e ressalva
#nombres de archivos que necesitan ser filtrados
file_data_frame <- function(folder) {
  path <- list.files(folder, full.names = T)
  name <- basename(path) %>% stringr::str_remove(".csv")
  fdf  <- tibble(names = name,
                 paths = path)
  return(fdf)
}
gbifs_df <- file_data_frame(pasta_out)
syn_df <- file_data_frame("output/p2/sinonimos/")
#el archivo que filtra

este <- left_join(gbifs_df, syn_df, by = "names")
rm(dir)
le_filtra <- function(occs, syn, dir = outdir) {
  if (!file.exists(paste0(dir,"/", sp, ".csv"))) {
  syn <- read_csv(syn)
  sp <- syn %>% pull(especie) %>% unique()
  nomes <- syn %>% pull(nomes)
  x <- read_csv(occs, guess_max = 105000)
  unique(x$species)
  x <- filter(x, species %in% nomes)
    write_csv(x, file = paste0(dir,"/", sp, ".csv"))
  }
}

le_filtra(occs = este$paths.x[10], syn = este$paths.y[10])

outdir <- paste0(pasta_out, "/clean")
dir.create(dir)
le_filtra(gbifs[[1]],
          syn_dfs[[1]],
          dir = dir)

plan(multisession, workers = 15)
View(este)
gbifs <- furrr::future_map2(.x = este$paths.x,
                            .y = este$paths.y,
                            ~le_filtra(occs = .x, syn = .y, dir = outdir),
                            .progress = T)
plan(sequential)
