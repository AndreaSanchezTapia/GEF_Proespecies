library(readr)
library(dplyr)
library(stringr)
devtools::load_all("../../R_packages/flora/")#dados de 8 de marco
#Rocc::update_flora(force_update = T)


#le tudo formatado
files <- list.files(output, full.names = T, pattern = "format")
files <- files[-1]
names_files <- simplify2array(strsplit(basename(files), ".csv"))
all_files <- purrr::map(files, readr::read_csv, na = "s.i.")
names(all_files) <- names_files
names(all_files)

#mirar que columnas hay
purrr::map(all_files, ~names(.x))
dados_merge <- purrr::map(all_files, ~select(.x, "especie_original", starts_with("cat_ameaca")))

CR_Lac        <- dados_merge$CR_Lac_format
sima          <- dados_merge$IUCN_CNCFlora_BR_SP_format
SP_Oficial    <- dados_merge$SP_Oficial_format
P443          <- dados_merge$especiesportaria443_format
#cncflora_web <- dados_merge$status_cncflora_format
cncflora_SP   <- dados_merge$cncflora_SP_format
UICN_SP       <- dados_merge$UICN_SP_flora_format
mpo_SP        <- dados_merge$mpoSP_format
dup <- function(x) nrow(x) == nrow(distinct(x))
purrr::map_df(all_files, ~dup(.x)) #ninguna tiene duplicados internos


Lista_parcial <- full_join(sima, SP_Oficial) %>%    #3832
  full_join(CR_Lac) %>%            #3832
  full_join(cncflora_SP) %>%     #4810
  full_join(UICN_SP) %>%
  left_join(P443) %>%             #4810
#tocaria chequear que dice fb2020 sobre p443
    arrange(especie_original) %>%
  distinct() #pierde cinco, una locura
Lista_parcial[is.na(Lista_parcial$especie_original),]
sum(str_detect(Lista_parcial$especie_original, "var."))
write_csv(Lista_parcial, "output/02_lista_parcial.csv", na = "s.i.")







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

