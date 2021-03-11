library(readr)
library(dplyr)
library(stringr)
devtools::load_all("../../R_packages/flora/")#dados de 8 de marco
#Rocc::update_flora(force_update = T)


#le tudo formatado
output <- "data/dados_formatados"
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
SP_Oficial    <- dados_merge$SP_Oficial_format %>% distinct()

P443          <- dados_merge$especiesportaria443_format
#cncflora_web <- dados_merge$status_cncflora_format
cncflora_SP   <- dados_merge$cncflora_SP_format
UICN_SP       <- dados_merge$UICN_SP_flora_format
mpo_SP        <- dados_merge$mpoSP_format
dup <- function(x) nrow(x) == nrow(distinct(x))
purrr::map_df(dados_merge, ~dup(.x)) #SP_oficial tiene duplicados internos


Lista_parcial <- full_join(sima, SP_Oficial) %>%    #3820
  full_join(CR_Lac) %>%            #3820
  full_join(cncflora_SP) %>%     #4994
  full_join(UICN_SP) %>%         # 5987
  full_join(mpo_SP) %>%             #5991
  left_join(P443) %>%
    arrange(especie_original)
sum(str_detect(Lista_parcial$especie_original, "var."))
# en cuantas listas aparecen las especies?
binaria <- ifelse(is.na(Lista_parcial[,-1]), 0, 1)
quantos <- rowSums(binaria)
sum(table(rowSums(binaria)))


write_csv(Lista_parcial, "output/02_lista_parcial.csv", na = "s.i.")
#isto sao nomes originais, sem autor, sem correcao tipografica nem taxonomica


#compara com a flora do brasil cada dataset
flora_tudo <- get.taxa(unique(Lista_parcial$especie_original),
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
readr::write_csv(flora_tudo, file = fs::path("output", "03_flora_tudo", ext = "csv"), na = "s.i.")
#juntar com a base unificada e examinar.
#flora_tudo <- read_csv("output/03_flora_tudo.csv",na = "s.i.")

#cria especie_original para juntar
flora_tudo$especie_original <- flora_tudo$original.search
dim(Lista_parcial)
names(flora_tudo)

Lista_all <- left_join(Lista_parcial, flora_tudo)
readr::write_csv(Lista_all, file = fs::path("output", "04_tudo_sem formato", ext = "csv"), na = "s.i.")

#ainda ha duplicados nas fontes originais de dados. vismia martiana em sp oficial e stenandrum diphyllum em sima
dupl <- which(duplicated(Lista_all$especie_original))
sp <- Lista_all$especie_original[dupl]
filter(Lista_all, especie_original %in% sp)
filter(sima, especie_original %in% sp)
filter(SP_Oficial, especie_original %in% sp)
