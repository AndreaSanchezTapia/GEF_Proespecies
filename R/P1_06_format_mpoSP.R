library(readxl)
library(janitor)
library(readr)

mpoSP <- read_xlsx("data/dados_crus/Lista Ameaçadas Município SP.xlsx", skip = 2)
mpoSP <- janitor::clean_names(mpoSP)
names(mpoSP)
mpoSP_format <- mpoSP %>%
  select(-x1) %>%
  rename(
    especie_original = nome_cientifico_sem_autor,
    especie_autor = nome_cientifico_completo,
    cat_ameaca_mpoSP = categoria_do_risco_de_exticao) %>%
  mutate(fonte = "mpoSP")
mpoSP_format
write_csv(mpoSP_format, "data/dados_formatados/mpoSP_format.csv")
