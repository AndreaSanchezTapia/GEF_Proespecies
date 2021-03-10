library(vroom)
library(dplyr)
library(tidyr)
library(stringr)

# LISTA 443
#download.file("http://ckan.jbrj.gov.br/dataset/23f2e24c-5676-4acd-83f0-03621cba4364/resource/11e8495f-3e37-4619-bb07-d77ed232b384/download/especiesportaria443.csv", destfile = "data/dados_crus/especiesportaria443.csv")

especiesportaria443 <- read_delim(
  "data/dados_crus/especiesportaria443.csv",
  "\t",
  escape_double = FALSE,
  locale = locale(encoding = "ISO-8859-1"),
  trim_ws = TRUE,
  skip = 4
)

especiesportaria443 <- especiesportaria443 %>%
  rename(especie = `Táxon`,
         cat_ameaca_br = Categoria) %>%
  mutate(familia = ifelse(is.na(cat_ameaca_br), str_to_sentence(especie), NA)) %>%
  fill(., "familia", .direction = "down") %>%
  select(familia, especie, cat_ameaca_br) %>% filter(!is.na(cat_ameaca_br)) %>%
  mutate(fonte = "Portaria 443/2014")

write_csv(especiesportaria443, "data/dados_formatados/especiesportaria443.csv")
P443 <- read_csv("data/dados_formatados/especiesportaria443.csv")

rocc_check1 <- check_string(P443$especie)
rocc_check2 <- check_string(rocc_check1$species)
rocc_check <- cbind(rocc_check1, rocc_check2)
rocc_check <- janitor::clean_names(rocc_check)
names(rocc_check)
count(rocc_check, species_status, species_status_2)

rocc_check %>% filter(species_status != "name_w_authors") %>% View()
#usa verbatim
rocc_check <- rocc_check %>%
  mutate(especie = case_when(
    species_status %in% c("subspecies", "variety", "forma") ~ verbatim_species,
    species_status_2 %in% c("possibly_ok") & species_status %in% c("name_w_authors") ~ species_2))

# modificar a mao os que faltam
l <- length(rocc_check$especie[rocc_check$species_status != "name_w_authors"])
for (i in seq_along(1:l)) {
  rocc_check$especie[rocc_check$species_status != "name_w_authors"][i] <- remove.authors(rocc_check$especie[rocc_check$species_status != "name_w_authors"][i])
}
names(P443)
P443 <- P443 %>% rename(especie_autor = especie)
P443$especie <- rocc_check$especie
names(P443)
write_csv(P443, "data/dados_formatados/especiesportaria443_format.csv")
