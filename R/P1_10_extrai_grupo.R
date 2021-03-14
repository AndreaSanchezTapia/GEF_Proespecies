# cria coluna grupo
library(readr)
devtools::load_all("../../R_packages/flora/")
devtools::load_all("../../R_packages/Rocc/")
tudo <- read_csv("output/04_tudo_sem formato.csv", na = "s.i.")
names(tudo)

#formatando seriamente
tudo <- tudo %>%
  select(family,
         especie_original, scientific.name, starts_with("cat_ameaca"))
# ipt antigo da sara
unzip("data/dados_crus/ipt/973412d572e928704ba483659033d9cae2e56489.zip", exdir = "data/dados_crus/ipt/" )
spprofile <- vroom::vroom("data/dados_crus/ipt/973412d572e928704ba483659033d9cae2e56489/taxon.txt")

grupo <- spprofile %>%
  select(id, taxonID, scientificName, higherClassification) %>%
  separate(higherClassification, into = LETTERS, remove = F) %>%
  select(id, taxonID, scientificName, higherClassification, A, B) %>%
  janitor::clean_names() %>%
  rename(grupo = b)

tudo <- janitor::clean_names(tudo)

wgrupo <- grupo %>%
  select(scientific_name, grupo) %>%
  filter(scientific_name %in% tudo$scientific_name)
write_csv(wgrupo, "data/dados_crus/ipt/grupo.csv")
right_join(tudo) %>%
  relocate(grupo, family)
wgrupo %>% count(is.na(grupo), notes = "not found")
wgrupo %>% count(notes, taxon_status, grupo)
write_csv(wgrupo, "output/06_lista_com_grupo.csv")
