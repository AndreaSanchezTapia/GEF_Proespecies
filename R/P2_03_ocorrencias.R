# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
splink <- "output/p2/specieslink/"
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")

#para cada especie
pasta_occs <- "output/p2/occs/"
df <- purrr::map(df_sinonimos, ~read_csv(.x))
df[[1]]$nomes
pasta_out <- paste0(pasta_occs, nomes)
purrr::map(pasta_out, ~dir.create(.x, recursive = T))
length(df)


#baja ocurrencias de species link
for (i in j) {
  search <- df[[i]]$nomes[stringr::str_detect(string = df[[i]]$nomes, "var\\.", negate = T)]

  sp <- rspeciesLink(dir = paste0(pasta_out[i], "/splink/"),
                     filename = nomes[i],
                     species = nomes[i],
                     Scope = "plants",
                     Synonyms =  "flora2020")
writeLines(search, "search.txt")

  Sys.sleep(1)
print(i)
}
beepr::beep(8)
#errores puntuales
2034#nome original
3426#nome original
3442 #Error in readBin(3L, raw(0), 32768L) :
#GnuTLS recv error (-110): The TLS connection was non-properly terminate nomes original

read_csv(paste0(pasta_out[j], "/splink/",nomes[j], ".csv"))



#para busca en jabot
dir.create("data/dados_formatados/hv/")
paste(nomes, collapse = ",") %>% writeLines("data/dados_formatados/hv/nomes.txt")


# bajar registros de rgbif
sp2 <- rgbif2(dir = paste0(pasta_out, "/gbif/"),
              filename = nomes[1],
              species = df$nomes,
              force = T,
              remove_na = F)
