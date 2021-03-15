library(readr)
library(dplyr)
library(stringr)
remotes::install_github("andreasancheztapia/flora")
library(flora)


#le tudo formatado
output <- "data/dados_formatados"
files <- list.files(output, full.names = T, pattern = "format")
names_files <- simplify2array(strsplit(basename(files), ".csv"))
all_files <- purrr::map(files, readr::read_csv)
names(all_files) <- names_files
names(all_files)

#mirar que columnas hay
purrr::map(all_files, ~names(.x))
dados_merge <- purrr::map(all_files,
                          ~select(.x,
                                  "especie_original",
                                  starts_with("cat_ameaca"),
                                  fonte)) %>%
  purrr::map(~distinct(.x))

CR_Lac        <- dados_merge$CR_Lac_format
sima          <- dados_merge$IUCN_CNCFlora_BR_SP_format
SP_Oficial    <- dados_merge$SP_Oficial_format %>% distinct()
P443          <- dados_merge$especiesportaria443_format
cncflora_SP   <- dados_merge$cncflora_SP_format
UICN_SP       <- dados_merge$UICN_SP_flora_format
mpo_SP        <- dados_merge$mpoSP_format
dup <- function(x) nrow(x) == nrow(distinct(x))
names(sima)
count(sima,
      cat_ameaca_br_sima,
      cat_ameaca_cncflora_sima,
      cat_ameaca_sp_sima
      ) %>% View()
purrr::map_df(dados_merge, ~dup(.x)) #SP_oficial tiene duplicados internos


Lista_parcial <- full_join(sima, SP_Oficial, by = "especie_original") %>%    #3820
  full_join(CR_Lac, by = "especie_original") %>%            #3820
  full_join(cncflora_SP, by = "especie_original") %>%     #4994
  full_join(UICN_SP, by = "especie_original") %>%         # 5987
  full_join(mpo_SP, by = "especie_original") %>%             #5991
  left_join(P443, by = "especie_original") %>%
    arrange(especie_original) %>%
  unite(fontes, starts_with("fonte"), na.rm = T, sep = "-") %>%
  relocate(especie_original, fontes)

write_csv(Lista_parcial, "output/02_lista_parcial.csv")


#compara com a flora do brasil cada dataset
# flora_tudo <- get.taxa(unique(Lista_parcial$especie_original),
#                        states = TRUE,
#                        suggest.names = T,
#                        replace.synonyms = T,
#                        life.form = F,
#                        habitat = F,
#                        vegetation.type = F,
#                        vernacular = F,
#                        establishment = F,
#                        domain = F,
#                        endemism = F)
# names(flora_tudo)
# flora_drop <- get.taxa(unique(Lista_parcial$especie_original),
#                        states = TRUE,
#                        suggest.names = T,
#                        drop = "authorship",
#                        replace.synonyms = T,
#                        life.form = F,
#                        habitat = F,
#                        vegetation.type = F,
#                        vernacular = F,
#                        establishment = F,
#                        domain = F,
#                        endemism = F)
# names(flora_drop)
# flora_tudo2 <- left_join(flora_tudo, flora_drop)
# names(flora_tudo2)
# readr::write_csv(flora_tudo2,
#                  file = fs::path("output", "03_flora_tudo", ext = "csv"))
#

#juntar com a base unificada e examinar.
flora_tudo <- readr::read_csv(file = fs::path("output", "03_flora_tudo", ext = "csv"))

#cria especie_original para juntar
flora_tudo$especie_original <- flora_tudo$original.search

Lista_all <- left_join(Lista_parcial, flora_tudo) %>%
  distinct()
readr::write_csv(Lista_all,
                 file = fs::path("output", "04_tudo_sem formato", ext = "csv"))

#agora só tem duplicados devido às fontes. vismia martiana em sp oficial e stenandrum diphyllum em sima
dupl <- which(duplicated(Lista_all$especie_original))
sp <- Lista_all$especie_original[dupl]

