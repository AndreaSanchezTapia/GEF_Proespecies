# cria coluna grupo
library(readr)
library(flora)
library(Rocc)

tudo <- read_csv("output/04_tudo_sem formato.csv") %>%
  janitor::clean_names()
names(tudo)

#formatando seriamente
# campos do produto 1 ----
campos_p1 <- c("grupo", "familia", "genero", "epiteto_especifico", "especie", "sinonimia", "fonte_sinonimia", "cat_ameaca_iucn", "cat_ameaca_br", "cat_ameaca_cncflora", "cat_ameaca_sp")


tudo <- tudo %>%
  relocate(family,
         especie_original,
         genus,
         specific_epiteth,
         taxon_rank,
         infra_epiteth,
         #scientific_name,
         starts_with("cat_ameaca"),
         fontes)

# ipt antigo da sara
#unzip("data/dados_crus/ipt/973412d572e928704ba483659033d9cae2e56489.zip", exdir = "data/dados_crus/ipt/" )
#spprofile <- vroom::vroom("data/dados_crus/ipt/973412d572e928704ba483659033d9cae2e56489/taxon.txt")
# grupo <- spprofile %>%
#   select(id, taxonID, scientificName, higherClassification) %>%
#   separate(higherClassification, into = LETTERS, remove = F) %>%
#   select(id, taxonID, scientificName, higherClassification, A, B) %>%
#   janitor::clean_names() %>%
#   rename(grupo = b)

# wgrupo <- grupo %>%
#   select(scientific_name, grupo) %>%
#   filter(scientific_name %in% tudo$scientific_name)
# write_csv(wgrupo, "data/dados_crus/ipt/grupo.csv")
grupo <- vroom::vroom("data/dados_crus/ipt/grupo.csv")
unique(grupo$grupo)
grupo$grupo[grupo$grupo == "Bri"] <- "Briófitas"
wgrupo <- grupo %>%
right_join(tudo) %>%
  relocate(grupo, family, especie_original)


write_csv(wgrupo, "output/06_lista_com_grupo.csv")
