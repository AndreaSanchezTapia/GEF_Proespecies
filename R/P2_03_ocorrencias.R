# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
splink <- "output/p2/specieslink/"
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
df <- read_csv(df_sinonimos[1])
df$nomes
pasta_occs <- "output/p2/occs/"
pasta_out <- paste0(pasta_occs, nomes[1])
dir.create(pasta_out, recursive = T)
sp <- rspeciesLink(dir = paste0(pasta_out, "/splink/"),
             filename = nomes[1],
             species = df$nomes,
             Scope = "plants",
             Synonyms =  "flora2020")
sp2 <- rgbif2(dir = paste0(pasta_out, "/gbif/"),
              filename = nomes[1],
              species = df$nomes,
              force = T,
              remove_na = F)


