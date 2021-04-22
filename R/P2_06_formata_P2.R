library(readxl)
library(readr)
library(dplyr)
#cria entra_revisado
#le a tabela revisada com coluna "NOVA LISTA"
lista_velha <- read_excel("data/dados_formatados/p2/Lista de espécies-geral.xlsx", sheet = 2, na = "NA")

lista_velha %>% count(elegivel_produto_1 == "inapta", notas_all)
#cria "entra" para saber quem vai de fato e organiza por nomes_aceito_correto
entra_revisado <- lista_velha %>%
  select(nome_aceito_correto, `NOVA LISTA - 20-04`, ) %>%
  rename(entra_revisado = `NOVA LISTA - 20-04`)
entra_revisado
readr::write_csv(entra_revisado, "output/p2/15_entra_revisado.csv")



#com tudo atualizado

p2_i <- read_csv( "output/p2/p2_inicial.csv")
tax <- read_csv("output/p2/10_base_P1.csv")
notas <- read_csv("output/p2/05_notas.csv") #notas shrugs
fontes <- read_csv("output/p2/12_fontes.csv") #fontes
cat_ameaca <- read_csv("output/p2/04_cat_ameaca.csv") #velho
cat_ameaca_geral <- read_csv("output/p2/04_cat_ameaca_geral.csv")
cruza <- read_csv("output/p2/01_cruzam_shapes_new.csv") #vem do workflow anterior
entra <- read_csv("output/p2/15_entra_revisado.csv") #eles
tax %>% View()
resumo <- p2_i %>%
  full_join(tax) %>%
  left_join(notas) %>%
  left_join(fontes) %>%
  left_join(cat_ameaca) %>%
  left_join(cat_ameaca_geral) %>%
  left_join(cruza) %>%
  left_join(entra)

names(resumo)

write_csv(resumo, "output/p2/02_resumo.csv")
dim(resumo)
length(unique(resumo$nome_aceito_correto))
setdiff(resumo$nome_aceito_correto, tax$nome_aceito_correto)
tail(resumo)

