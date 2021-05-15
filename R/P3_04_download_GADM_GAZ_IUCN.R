#download de dados geograficos
devtools::install_github("liibre/Rocc")
library(Rocc)
library(dplyr)
countries <- c("Brazil")
#install.packages("countrycode")
library(countrycode)
source("R/functions.R")
library(sf)
library(rgdal)
library(tmap)

iso3 <- countrycode::countrycode(countries,
                                 "country.name",
                                 "iso3c")
# Check the downloading scheme:
df <- data.frame(codigo = rep(iso3, each = 5), pais = rep(4:0, length(countries)))
# we try the downloads at the finest scale first and ask best = TRUE to avoid downloading coarser levels. this is not beautiful, maybe the user should use EITHER level OR best internally. hmmm. if best = TRUE then default level is 4, then 3 etc. with a while loop.
destfolder = "data/GADM/"
purrr::walk2(.x = rep(iso3, each = 5),
             .y = rep(4:0, length(countries)),
             ~ getGADM(
               cod = .x,
               level = .y,
               best = TRUE,
               destfolder = destfolder
             ))
# download gazetteer
destfolder = "data/GAZ/"
iso3 %>% purrr::map(.x = ., ~ getGAZ(cod = .x, destfolder = destfolder))

# download UICN
destfolder = "data/WDPA/"
iso3 %>% purrr::map(.x = ., ~getWDPA(cod = .x, unzip = TRUE, destfolder = destfolder))


# joining
gadm_files <- list.files("data/GADM", pattern = paste(iso3, collapse = "|"), full.names = TRUE)
lev <- stringr::str_extract(string = gadm_files, pattern = "\\d+")



#wdpa----
wdpa_files <- list.files("data/WDPA", full.names = TRUE, recursive = T, pattern = ".zip$")
unzip(zipfile = wdpa_files[2], exdir = "data/WDPA/Federal") #2 to 4 porque el 1 es el general
unzip(zipfile = wdpa_files[3], exdir = "data/WDPA/Estadual") #2 to 4 porque el 1 es el general
unzip(zipfile = wdpa_files[4], exdir = "data/WDPA/Municipal") #2 to 4 porque el 1 es el general
wdpa_files <- list.files("data/WDPA", full.names = TRUE, recursive = T, pattern = ".shp$")


bra <- readRDS("data/GADM/BRA_3_sf.rds")
bra1_pts <- read_sf(wdpa_files[1])
bra1_pol <- read_sf(wdpa_files[2])
bra0_pts <- read_sf(wdpa_files[3])
bra0_pol <- read_sf(wdpa_files[4])
bra2_pts <- read_sf(wdpa_files[5])
bra2_pol <- read_sf(wdpa_files[6])
wdpa <- bind_rows(bra0_pol, bra0_pts,
          bra1_pol, bra1_pts,
          bra2_pol, bra2_pts)
wdpa <- distinct(wdpa)
head(wdpa$NAME)
wdpa <- clean_string(wdpa, NAME)
wdpa_sp <- st_crop()
#se puede escribir este gran shape
write_sf(bra_gaz2, "output/p3/p3_gazetteer_diva_sp.csv")


mapbra <- tm_shape(bra) +
  tm_polygons(col = "white") +
  tm_shape(bra0_pol) +
  tm_fill(col = "darkgreen") +
  tm_shape(bra1_pol) +
  tm_fill(col = "green") +
  tm_shape(bra1_pts) +
  tm_dots()
mapbra

bra <- readRDS(gadm_files[1])

#gazetteer
plot(bra[1])

gaz_files <- list.files("data/GAZ", pattern = ".dbf", full.names = TRUE)
bra_gaz <- read_sf(gaz_files[1]) #mk, sirve
bra_gaz <- st_as_sf(x = bra_gaz, coords = c("LONG", "LAT"))
bra_gaz <- clean_string(bra_gaz, NAME) %>% rename(string_check = mpo_check)
library(stringr)
which(grepl("parque", bra_gaz$string_check))
bra_gaz[grepl("^barra", bra_gaz$string_check),] %>% View()

sp <- read_sf("data/dados_crus/SP_UF_2020/SP_UF_2020.shp")
spbb <- st_bbox(sp)
st_crs(sp)
bra_gaz <- st_set_crs(bra_gaz, st_crs(sp))
bra_gaz2 <- st_crop(bra_gaz, spbb)
bra_gaz2 <- clean_string(bra_gaz2, ADM2)
count(bra_gaz2, ADM1, ADM2) %>% View()
write_sf(bra_gaz2, "output/p3/p3_gazetteer_diva_sp.csv")

mapbra +
  tm_shape(bra_gaz) +
  tm_dots()
