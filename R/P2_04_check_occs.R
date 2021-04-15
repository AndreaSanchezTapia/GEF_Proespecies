library(readr)
library(dplyr)
library(Rocc)
library(stringr)
library(sf)
library(tmap)
library(furrr)

#lee las tablas de ocurrencias
occs <- list.files("output/p2/occs/",recursive = T, full.names = T, pattern = ".csv")
gbif <- occs[stringr::str_detect(occs, "splink", negate = T)]
splink <- occs[stringr::str_detect(occs, "gbif", negate = T)]

nomes_splink <- basename(splink) %>% stringr::str_remove(".csv")
nomes_gbif   <- basename(gbif)   %>% stringr::str_remove(".csv")
names(splink) <- nomes_splink
names(gbif) <- nomes_gbif

#le os shapes
rbcv <- read_sf("data/dados_crus/RBCV_Limite/RBCV_Limite_datageo_jan2020.shp")
t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")

#checa se cruza
cruza_shape <- function(tabela, shape) {
  ss <- basename(tabela) %>% stringr::str_remove(".csv")
  st <- tryCatch({
    shp <- st_read(tabela, options = c("X_POSSIBLE_NAMES=decimalLongitude",
                                "Y_POSSIBLE_NAMES=decimalLatitude"))
      },
      error = function(e) {
        message(paste(ss,"problem"))
        #message(e)
        })
    if ("sf" %in% class(st) ) {
      st <- st_set_crs(st, st_crs(shape))
      joins <- st_join(st, shape, left = F)#join = st_within)
      return(tibble(sp = ss,
                    inside = if_else(nrow(joins) == 0, FALSE, TRUE)))

    }
}


plan(multisession, workers = 15)
#cruza_t20_splink  <- furrr::future_map(splink,
#                                       ~cruza_shape(.x, t20), .progress = T)
#cruza_rbcv_splink <- furrr::future_map(splink, ~cruza_shape(.x, rbcv), .progress = T)
#cruza_rbcv_gbif   <- furrr::future_map(gbif,   ~cruza_shape(.x, rbcv), .progress = T)
#cruza_t20_gbif   <- furrr::future_map(gbif,   ~cruza_shape(.x, t20), .progress = T)

plan(sequential)

# resume

a <- cruza_t20_splink %>% bind_rows() %>% rename(splink_t20 = inside)
b <- cruza_t20_gbif %>% bind_rows() %>% rename(gbif_t20 = inside)
c <- cruza_rbcv_splink %>% bind_rows() %>% rename(splink_rbcv = inside)
d <- cruza_rbcv_gbif %>% bind_rows() %>% rename(gbif_rbcv = inside)

#salva quem cruza e quem nao
full_join(a, b) %>%
  full_join(c) %>%
  full_join(d) %>%
  arrange(sp) %>%
  mutate(sum = rowSums(across(where(is.logical)), na.rm = T)) %>%
  write_csv("output/p2/01_cruzam_shapes.csv")
