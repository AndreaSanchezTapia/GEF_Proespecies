library(readxl)
library(readr)
library(dplyr)

#
rtf_o <- readr::read_csv("output/p3/p3_rtf_26MAY.csv", guess_max = 100000)
coord_original <- rtf_o %>% select(id,
                                   especie,
                                   lat_original, long_original,
                                   #localidade,
                                   #verbatim_locality,
                                   #observacao_localidade,
                                   #municipio_padronizado_final
)

#le leo
leo <- read_xls("data/dados_formatados/p3/leo/p3_Planilha_filtrada_30_05_End_day_leo.xls")
leo <- janitor::clean_names(leo)
names(leo)
cols_leo <- c("id",
              "especie",
              "ano_coleta",
              "municipio_pati",
              "municipio_corrigidos_leo",
              "municipio_final",
              "localidade_x",
              "lat_original",
              "long_original",
              #"lat_original_base",
              #"long_original_base",
              "coordenada_original_boa",
              "lat_corrigida",
              "long_corrigida",
              "situacao_coordenada",
              "nome_uc",
              "obs_leo")
leo <- leo %>% select(id, especie, cols_leo)

#le pr
PRHL <- read_xlsx("data/dados_formatados/p3/patricia/BASE DE DADOS PRODUTO 3 - ATUAL - offline 31-12hs.xlsx", guess_max = 13000)
names(PRHL)
PRHL <- PRHL %>%   rename(status_dupl = status_dupl...18,
         etiqueta_duplicaDO = etiqueta_duplicaDO...19,
         etiqueta_duplicaTA = etiqueta_duplicaTA...20,
         categoria_final = categoria_final...21,
         tombo_herbario_origem = tombo_herbario_origem...26,
         cod_barras_herbario_origem = cod_barras_herbario_origem...27,
         fonte_dados = fonte_dados...24,
         herbario_deposito = herbario_deposito...25,
         nome_coletor_padronizado2 = nome_coletor_padronizado2...28,
         numero_coletor_padronizado1 = numero_coletor_padronizado1...29,
         fonte_dados = fonte_dados...24,
         T20 = T20...22
         )
PRHL <- janitor::clean_names(PRHL) %>%
  rename(etiqueta_duplicado = etiqueta_duplica_do ) %>%
  rename(etiqueta_duplicata = etiqueta_duplica_ta )

names(final)

final <- PRHL %>%
  select(-any_of(cols_leo)) %>%
  left_join(leo) %>% left_join(coord_original)
names(final)
head(final$lat_original)
head(final$long_original)

names(final)
names(final_PAT)
final_PAT <- final %>% select(names_planilha_ordenado)
final_LEO <- final %>% select(cols_leo)
count(final, coordenada_original_boa)
write_csv(final_PAT, "output/p3/p3_sem_duplicados_leo_p_pat.csv")
write_csv(final_LEO, "output/p3/p3_sem_duplicados_leo_p_leo.csv")



