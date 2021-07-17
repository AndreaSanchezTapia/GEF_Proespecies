library(dplyr)
library(readr)
backup <- readr::read_csv("data/dados_formatados/p3/patricia/BACKUPATUALIZADA - Base Produto 3 - 03-06-08-06-21-14-45hs.xlsx - Base 03-06.csv", guess_max = 13000)
template <- readr::read_csv("output/p3/p3_template.csv", guess_max = 13000) %>% arrange(especie, id)
wip <- read_csv("output/p3/wip - p3_WIP 9JUN.csv", guess_max = 13000)
head(backup$id)
head(wip$id)
head(template$id)

all(backup$id %in% wip$id)
all(wip$id %in% backup$id)
wip <- wip %>% select(id)
#backup %>% select(nome_instituicao, herbario_origem) %>% distinct() %>% readr::write_csv("output/p3/p3_herbarioinstituicao.csv")
herbarios_padronizacao <- read_csv("data/dados_formatados/p3/patricia/Padronizacao_herbario_v.2.xlsx - Planilha1.csv")
#organiza
backup <- left_join(wip, backup)#reorganiza general
template <- left_join(wip, template)#reorganiza general
herbarios_df <- left_join(wip, backup) %>%
  select(id, nome_instituicao, herbario_origem, herbario_receptor)
hh <- left_join(herbarios_df, herbarios_padronizacao)
hp <- herbarios_padronizacao %>% rename(
  herbario_receptor = herbario_origem,
  herbario_receptor_padronizado = herbario_padronizado)
HP <- hh %>% left_join(hp) %>% select(id,
                                      nome_instituicao,
                                      herbario_origem,
                                      herbario_origem_padronizado = herbario_padronizado,
                                      herbario_receptor,
                                      herbario_receptor_padronizado)
write_csv(HP, "output/p3/p3_HERBARIOS_PADRONIZADOS.csv")


read_csv("")
