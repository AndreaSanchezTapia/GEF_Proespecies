#formats cncfloraweb
library(readxl)
library(readr)
library(tidyr)
library(flora)

# FORMATA A TABELA DE OUTUBRO ----
#agora usamos o excel oficial
cncflora_out <-
  read_xlsx("data/dados_crus/Lista de avaliadas CNCFlora até outubro de 2020.xlsx",
            sheet = 4)
cncflora_out <- janitor::clean_names(cncflora_out) %>%
  mutate(fonte = "cncflora_out")
#corrige cattleya labiata 0 e 1
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2013]
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2018] <- "Cattleya lobata"

cncflora_format_out <- cncflora_out %>% rename(
  especie_original = taxa_avaliado,
  cat_ameaca_cncflora = categoria,
  avaliacao = selecionar_ultima_avaliacao
) %>% select(especie_original, cat_ameaca_cncflora, avaliacao, fonte) %>%
  distinct()

cncflora_format_out <- cncflora_format_out %>%
  mutate(fonte = if_else(avaliacao == 1, paste(fonte, "1", sep = "_"), paste(fonte, "0", sep = "_"))) %>%
  select(-avaliacao)
cncflora_format_out
#save
output <- "data/dados_formatados"
write_csv(cncflora_format_out, fs::path(output, "cncflora_out_format.csv"))

# FORMATA CNCFLORA WEB ----
#cncflora pagina web ler e formatar
cncflora_web <- read_csv("./data/dados_crus/status_cncflora_web.csv") %>% select(-1, -2)
cncflora_web <- cncflora_web %>%
  mutate(fonte = "cncflora_web") %>%
  rename(
  especie_original = species,
  cat_ameaca_cncflora = threat) %>%
  select(especie_original, cat_ameaca_cncflora, fonte) %>%
  filter(!is.na(especie_original)) %>%
  distinct()
cncflora_web
#save
output <- "data/dados_formatados"
write_csv(cncflora_web, fs::path(output, "cncflora_web_format.csv"))

#FORMAT CNCFLORA CKAN ----
cncflora_ckan <- read_csv("data/dados_crus/avaliacao-do-risco-de-extincao-de-arvore-nativas-do-brasil-2018-junho-2020.csv")
cncflora_ckan <- janitor::clean_names(cncflora_ckan) %>%
  mutate(fonte = "cncflora_ckan") %>%
  distinct() %>%
  rename(
  especie_original = especie_sem_autor,
  cat_ameaca_cncflora = categoria) %>%
  select(especie_original, cat_ameaca_cncflora, fonte) %>%
  distinct()

#save
output <- "data/dados_formatados"
write_csv(cncflora_ckan, fs::path(output, "cncflora_ckan_format.csv"))

cncflora <- full_join(cncflora_ckan, cncflora_web) %>% arrange(especie_original)
cncflora %>% distinct(especie_original) %>% tally()
library(tidyr)

cncflora <- cncflora %>%
  pivot_wider(id_cols = especie_original,
              names_from = "fonte",
              values_from = "cat_ameaca_cncflora") %>%
  distinct()

#junta e ve tudo cncflora ----
cncflora_format <- cncflora %>%
  mutate(cat_ameaca_cncflora =
           case_when(
             is.na(cncflora_ckan) & !is.na(cncflora_web) ~ cncflora_web,
             is.na(cncflora_web) & !is.na(cncflora_ckan) ~ cncflora_ckan,
             !is.na(cncflora_web) & !is.na(cncflora_ckan) ~ cncflora_ckan
                     )) %>%
  mutate(fonte =
           case_when(
             is.na(cncflora_ckan) & !is.na(cncflora_web) ~ "cncflora_web",
             is.na(cncflora_web) & !is.na(cncflora_ckan) ~ "cncflora_ckan",
             !is.na(cncflora_web) & !is.na(cncflora_ckan) ~ "cncflora_ckan")) %>%
  select(-cncflora_ckan, -cncflora_web)

write_csv(cncflora_format, "data/dados_formatados/cncflora/cncflora_format.csv")
cncflora_format <- read_csv("data/dados_formatados/cncflora/cncflora_format.csv")
# checa quais dessas especies estao em sao paulo
cncflora_flora <- get.taxa(cncflora_format$especie_original,
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
cncflora_all <- full_join(cncflora_format, cncflora_flora)
cncflora_all <- distinct(cncflora_all)

#only SP
cncflora_SP <- cncflora_all %>%
  filter(!is.na(occurrence)) %>%
  filter(stringr::str_detect(occurrence, pattern = "SP"))
#2256

# segundo a pagina do cncflora e a flora do brasil, haveria 2100 especies que
#ja foram avaliadas pelo cncflora e estao na pagina eles que se encotram no estado de sao paulo
#segundo o cncflora ate outubro sao 2240! boa.
#juntando ckan e web são 2256

#salva so nome e cat_ameaca
names(cncflora_SP)
cncflora_SP <- cncflora_SP %>%
  select(especie_original,
         starts_with("cat_ameaca"),
         fonte)
write_csv(cncflora_SP, "data/dados_formatados/cncflora_SP_format.csv")



