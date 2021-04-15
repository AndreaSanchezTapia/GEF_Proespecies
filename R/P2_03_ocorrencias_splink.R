# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
splink <- "output/p2/specieslink/"
gbif <- "output/p2/gbif/"
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
names(df_sinonimos) <- nomes
#para cada especie
pasta_occs <- "output/p2/occs/"

library(furrr)
plan(multisession, workers = 14)
df <- furrr::future_map(df_sinonimos, ~read_csv(.x), .progress = T)
warnings()
plan(sequential)

df[[1]]$nomes
names(df)

length(df)
names(df)


#baja ocurrencias de species link
download_splink <- function(oc, syn = T) {
  sp <- unique(oc$especie)
  pasta_out <- paste0(pasta_occs, sp, "/splink/")
  dir.create(pasta_out, recursive = T)
  if (syn) {
    search <- oc$nomes[stringr::str_detect(string = oc$nomes, "var\\.", negate = T)]
  } else search <- sp
  if (!file.exists(fs::path(pasta_out, sp, ext = "csv"))) {

    message(paste(sp))

    out <- tryCatch (
      {
        sl <- rspeciesLink(dir = paste0(pasta_out),
                     filename = sp,
                     species = search,
                     Scope = "plants",
                     Synonyms =  "flora2020")
        #write_csv(res, fs::path(pasta_out, sp, ext = "csv"))
        print(sp)
      },
      error = function(e) {
        message(paste(sp, "problem"))
        message(e)
      },
      warning = function(w) {
        message(paste(sp, "caused a warning"))
        message(w)
      })
    return(out)
  }
}

gf <- furrr::future_map(df, ~download_splink(.x, syn = F), .progress = T)


#para busca en jabot
dir.create("data/dados_formatados/hv/")
paste(nomes, collapse = ",") %>% writeLines("data/dados_formatados/hv/nomes.txt")

