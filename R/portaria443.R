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
