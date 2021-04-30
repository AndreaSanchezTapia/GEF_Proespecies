library(dplyr)
library(sf)
library(readr)
#takes species names fro folder paths
file_data_frame <- function(folder) {
  path <- list.files(folder, full.names = T)
  name <- basename(path) %>% stringr::str_remove(".csv")
  fdf  <- tibble(names = name,
                 paths = path)
  return(fdf)
}
#reads tableas as sf
read_sf2 <- function(x, column = "XY",  ...) {
  if (column == "XY") y <- read_sf(x, options = c("X_POSSIBLE_NAMES=X", "Y_POSSIBLE_NAMES=Y"), ...)
  if (column == "latlon") y <- read_sf(x, options = c("X_POSSIBLE_NAMES=decimalLongitude", "Y_POSSIBLE_NAMES=decimalLatitude"), ...)
  if (column == "ellos") y <- read_sf(x, options = c("X_POSSIBLE_NAMES=long_original", "Y_POSSIBLE_NAMES=lat_original"), ...)
  return(y)
  }
#writes tables as sf
write_sf2 <- function(x, ...) {
  st_write(x, layer_options = "GEOMETRY=AS_XY", ...)
}

mutate_para_bind <- function(x) {
  x %>%
    mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.logical), .fns = function(x) as.character(x)))
}


plantr_SP <- function(x) {
  data.frame(x) %>%
    plantR::fixLoc(loc.levels = c("country", "stateProvince")) %>%
    filter(country.new == "brazil" | is.na(country.new)) %>%
    filter(stateProvince.new == "sao paulo" | is.na(stateProvince.new)) %>%
    tibble()
}

check_n <- function(lista) {
  nrow_gb <- furrr::future_map(lista, ~nrow(.x), .progress = T)
  purrr::simplify(nrow_gb) %>% sum()
}
#CAMPOS
sel_fields <- read_csv("output/p2/08_campos_originales.csv") %>% filter(select == T)
#sorts fields
campos_ordenados <- read_csv("output/p2/11_camposP2.csv") %>% rename(field = campos)
setdiff(campos_ordenados$field, sel_fields$field)
setdiff(sel_fields$field, campos_ordenados$field)
ord_fields <- full_join(campos_ordenados, sel_fields) %>% pull(field)
ord_fields <- setdiff(ord_fields, c("decimalLatitude", "decimalLongitude"))
#CRUZA SHAPE


