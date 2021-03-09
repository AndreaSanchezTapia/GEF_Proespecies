# lê da API da IUCN e salva em data/UICN_BR.csv
library(dplyr)
library(jsonlite)
token <- readLines("./accessory/token_IUCN")
url <- "https://apiv3.iucnredlist.org/api/v3/"
path <- "country/getspecies/"
co <- "BR"
get <- paste0(url,path, co, "?token=", token)
UICN_br_data <- fromJSON(get)

spp <- UICN_br_data$result
dir.create("data")
readr::write_csv(spp, "./data/UICN_BR.csv")

