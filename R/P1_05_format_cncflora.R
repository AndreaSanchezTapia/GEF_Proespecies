#formats cncfloraweb
library(readxl)
devtools::load_all("../../R_packages/flora/")

#cncflora pagina web ler e formatar
cncflora_web <- read_csv("./data/dados_crus/status_cncflora.csv") %>% select(-1)
cncflora_web <- janitor::clean_names(cncflora_web) %>%
  mutate(fonte = "cncflora_web")
#agora usamos o excel oficial
cncflora_out <- read_xlsx("data/dados_crus/Lista de avaliadas CNCFlora até outubro de 2020.xlsx",
                      sheet = 4)
cncflora_out <- janitor::clean_names(cncflora_out) %>%
  mutate(fonte = "cncflora_out")
cncflora_ckan <- read_csv("data/dados_crus/avaliacao-do-risco-de-extincao-de-arvore-nativas-do-brasil-2018-junho-2020.csv")
cncflora_ckan <- janitor::clean_names(cncflora_ckan) %>%
  mutate(fonte = "cncflora_ckan")

#corrige cattleya labiata 0 e 1
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2013]
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2018] <- "Cattleya lobata"

cncflora_format <- cncflora_out %>% rename(
  especie_original = taxa_avaliado,
  cat_ameaca_cncflora = categoria,
  avaliacao = selecionar_ultima_avaliacao
) %>% select(especie_original, cat_ameaca_cncflora, avaliacao, fonte) %>%
  distinct()
count(cncflora_format, avaliacao)
count(cncflora_format, especie_original, avaliacao) %>% arrange(desc(n))

#checando, tudo bem com a correcao
reavaliados <- cncflora_format %>% filter(avaliacao == 0) %>% pull(especie_original)
cncflora_format %>% filter(especie_original %in% reavaliados) %>% count(especie_original, avaliacao) %>% View()


#separar em duas colunas de ameaca
library(tidyr)
cncflora_format_dual <- pivot_wider(data = cncflora_format,
                                    id_cols = c("especie_original", "fonte"),
                                    names_from = "avaliacao",
                                    values_from = "cat_ameaca_cncflora",
                                    names_prefix = "cat_ameaca_cncflora_")
cncflora_format_dual
cncflora_format_dual %>% filter(!is.na(cat_ameaca_cncflora_0))
#save
output <- "data/dados_formatados"
write_csv(cncflora_format_dual, fs::path(output, "cncflora_out_format.csv"))


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
cncflora_all <- full_join(cncflora_format_dual, cncflora_flora) #7519
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
cncflora_SP <- cncflora_SP %>% select(especie_original, starts_with("cat_ameaca"), fonte)
write_csv(cncflora_SP, "data/dados_formatados/cncflora_SP_format.csv", na = "s.i.")



