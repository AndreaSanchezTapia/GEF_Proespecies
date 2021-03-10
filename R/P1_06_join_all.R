library(readr)
library(dplyr)
library(stringr)
devtools::load_all("../../R_packages/flora/")#dados de 8 de marco
#Rocc::update_flora(force_update = T)

#cncflora pagina ler e formatar
cncflora_flora <- read_csv("./data/dados_crus/status_cncflora.csv")
cncflora_format <- cncflora_flora %>%
  rename(especie = search.str,
         cat_ameaca_cncfloraweb = threat.status) %>%
  select(cat_ameaca_cncfloraweb, especie)
output <- "data/dados_formatados"
write_csv(cncflora_format, fs::path(output, "status_cncflora_format.csv"))

#le tudo formatado
files <- list.files(output, full.names = T, pattern = "format")

names_files <- simplify2array(strsplit(basename(files), ".csv"))
all_files <- purrr::map(files, readr::read_csv, na = "s.i.")
names(all_files) <- names_files


#reads uicn_sp_flora names only
uicn_sp_flora <- list.files(output, full.names = T, pattern = "names_only")
uicn_sp_flora <- read_csv(uicn_sp_flora)

names(all_files)
purrr::map(all_files, ~names(.x))
dados_merge <- purrr::map(all_files, ~select(.x, "especie", starts_with("cat_ameaca")))

CR_Lac       <- dados_merge$CR_Lac_format
sima         <- dados_merge$IUCN_CNCFlora_BR_SP_format
SP_Oficial   <- dados_merge$SP_Oficial_format
P443         <- dados_merge$especiesportaria443_format
cncflora_web <- dados_merge$status_cncflora_format


dim(sima)                          #3771
lista_unificada <- full_join(sima, SP_Oficial) %>%    #3834
  full_join(CR_Lac) %>%            #3834
  left_join(P443) %>%
  left_join(cncflora_web) %>%
  arrange(especie)

write_csv(lista_unificada, "output/02_lista_suja.csv", na = "s.i.")
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

