library(readr)
library(dplyr)
library(stringr)
devtools::load_all("../../R_packages/flora/")
Rocc::update_flora()
cncflora_flora <- read_csv("../../R_packages/flora/data-raw/status_cnc.csv")

output <- "data/dados_formatados"
files <- list.files(ouuicn_sp_floratput, full.names = T, pattern = "format")

names <- simplify2array(strsplit(basename(files), ".csv"))
all_files <- purrr::map(files, readr::read_csv, na = "s.i.")
names(all_files) <- names
#all_files$UICN_SP_flora) %>% #SP FLora

#reads uicn_sp_flora names only
uicn_sp_flora <- list.files(output, full.names = T, pattern = "names_only")
uicn_sp_flora <- read_csv(uicn_sp_flora)

CR_Lac <- all_files$CR_Lac_format
sima <- all_files$IUCN_CNCFlora_BR_SP_format
SP_Oficial <- all_files$SP_Oficial_format
P443 <- all_files$especiesportaria443_format

names(CR_Lac)
dim(sima)                          #3771
lista_unificada <- full_join(sima, SP_Oficial) %>%    #3834
  full_join(CR_Lac) %>%            #3834
  left_join(P443) %>% arrange(especie)

write_csv(lista_unificada, "output/02_lista_suja.csv")
length(unique(lista_unificada$especie))
  full_join(uicn_sp_flora) %>% View()
mandar
comecar_aqui
  #compara com a flora do brasil cada dataset
flora_tudo <- get.taxa(unique(lista_unificada$especie),
                       states = TRUE,
                       suggest.names = T,
                       replace.synonyms = T,
                       life.form = T,
                       habitat = T,
                       vegetation.type = T,
                       vernacular = T,
                       establishment = T,
                       domain = T,
                       endemism = T)
names(flora_tudo)
count(flora_tudo, notes)
count(flora_tudo, threat.status)
count(flora_tudo, taxon.status)
readr::write_csv(flora_tudo, file = fs::path("output", "03_flora_tudo", ext = "csv"), na = "s.i.")

#juntar com a base unificada e examinar.

