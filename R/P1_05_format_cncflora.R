#formats cncfloraweb
library(readxl)
#cncflora pagina web ler e formatar
#cncflora_flora <- read_csv("./data/dados_crus/status_cncflora.csv")
#agora usamos o excel oficial
cncflora <- read_xlsx("data/dados_crus/Lista de avaliadas CNCFlora até outubro de 2020.xlsx",
                      sheet = 4)
names(cncflora)

#corrige cattleya labiata 0 e 1
cncflora$Selecionar_ultima_avaliacao[cncflora$`Taxa avaliado` == "Cattleya labiata" & cncflora$Ano == 2013] <- 0

cncflora_format <- cncflora %>% rename(
  especie_original = `Taxa avaliado`,
  cat_ameaca_cncflora = Categoria,
  avaliacao = Selecionar_ultima_avaliacao
) %>% select(especie_original, cat_ameaca_cncflora, avaliacao) %>%
  distinct()
count(cncflora_format, avaliacao)
count(cncflora_format, especie_original, avaliacao) %>% arrange(desc(n))

#checando, tudo bem com a correcao
reavaliados <- cncflora_format %>% filter(avaliacao == 0) %>% pull(especie_original)
cncflora_format %>% filter(especie_original %in% reavaliados) %>% count(especie_original, avaliacao) %>% View()


#separar em duas colunas de ameaca
library(tidyr)
cncflora_format_dual <- pivot_wider(data = cncflora_format,
                                    id_cols = "especie_original",
                                      names_from = "avaliacao",
                                    values_from = "cat_ameaca_cncflora", names_prefix = "cat_ameaca_cncflora_")
cncflora_format_dual
cncflora_format_dual %>% filter(!is.na(cat_ameaca_cncflora_0))
#save
output <- "data/dados_formatados"
write_csv(cncflora_format_dual, fs::path(output, "cncflora_format.csv"))


# checa quais dessas especies estao em sao paulo
cncflora_flora
cncflora_flora <- get.taxa(cncflora_format_dual$especie_original,
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
#rename
cncflora_flora$especie_original <- cncflora_flora$original.search
#junta
cncflora_all <- full_join(cncflora_format_dual, cncflora_flora) #7518
cncflora_all <- distinct(cncflora_all)

#only SP
cncflora_SP <- cncflora_all %>%
  filter(!is.na(occurrence)) %>%
  filter(str_detect(occurrence, pattern = "SP"))
#2240
# segundo a pagina do cncflora e a flora do brasil, haveria 2100 especies que
#ja foram avaliadas pelo cncflora e estao na pagina eles que se encotram no estado de sao paulo
#segundo o cncflora ate outubro sao 2240! boa.


#salva so nome e cat_ameaca
names(cncflora_SP)
cncflora_SP <- cncflora_SP %>% select(especie_original, starts_with("cat_ameaca"))
write_csv(cncflora_SP, "data/dados_formatados/cncflora_SP_format.csv", na = "s.i.")
