library(readr)
library(dplyr)
library(Rocc)
library(stringr)
library(sf)
library(tmap)
library(furrr)

#lee la tabla original de sinomimos
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")

#lee las tablas de specieslink
splink <- list.files("output/p2/occs/",recursive = T, full.names = T, pattern = ".csv")
splink <- splink[str_detect(splink, "gbif", negate = T)]
head(splink)

#checa NAs nao precisa refazer
refazer <- splink[stringr::str_detect(splink, "NA.csv")] %>% str_remove("output/p2/occs//") %>%
  str_remove("/splink/NA.csv")
refazer
sp1 <- basename(splink)
sp1 <- stringr::str_remove(sp1, ".csv")

setdiff(nomes, sp1)
setdiff(sp1, nomes) #myrcia speldens nao e ameacada


plan(multisession, workers = 14)
df <- furrr::future_map(splink, ~vroom::vroom(.x, guess_max = 1000, delim = ","), .progress = T)
plan(sequential)
a <- map(df, ~nrow(.x)) %>% simplify()
splink[which(a ==0)]
unlink(splink[which(a ==0)])
#cruza com os shapes
rbcv <- read_sf("data/dados_crus/RBCV_Limite/RBCV_Limite_datageo_jan2020.shp")
t20 <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")

cruza_shape <- function(tabela, shape) {
  tt <- st_read(tabela, options = c("X_POSSIBLE_NAMES=decimalLongitude",
                                     "Y_POSSIBLE_NAMES=decimalLatitude"))
  if ("sf" %in% class(tt) ) {
  tt <- st_set_crs(tt, st_crs(shape))
  joins <- st_join(tt, shape, left = F)#join = st_within)
  return(if_else(nrow(joins) == 0, FALSE, TRUE))
  }
}


plan(multisession, workers = 14)
cruza_t20 <- furrr::future_map(splink, ~cruza_shape(.x, t20), .progress = T)
cruza_rbcv <- furrr::future_map(splink, ~cruza_shape(.x, rbcv), .progress = T)
table(unlist(cruza_t20))
table(unlist(cruza_rbcv))
warnings()
tibble(t20 = unlist(cruza_t20), rbcv = unlist(cruza_rbcv)) %>% count(t20, rbcv)
table(unlist(cruzarbcv))
plan(sequential)


data(World)
tm_shape(World, bbox = "Brazil") +
  tm_borders() +
    tm_shape(shape) +
    tm_borders() +
    tm_shape(tt) +
    tm_dots()
