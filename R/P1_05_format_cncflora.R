#formats cncfloraweb
library(readxl)
library(readr)
library(tidyr)
devtools::load_all("../../R_packages/flora/")

# FORMATA A TABELA DE OUTUBRO ----
#agora usamos o excel oficial
cncflora_out <- read_xlsx("data/dados_crus/Lista de avaliadas CNCFlora até outubro de 2020.xlsx",
                          sheet = 4)
cncflora_out <- janitor::clean_names(cncflora_out) %>%
  mutate(fonte = "cncflora_out")
#corrige cattleya labiata 0 e 1
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2013]
cncflora_out$taxa_avaliado[cncflora_out$taxa_avaliado == "Cattleya labiata" & cncflora_out$ano == 2018] <- "Cattleya lobata"

cncflora_format_out <- cncflora_out %>% rename(
  especie_original = taxa_avaliado,
  cat_ameaca_out = categoria,
  avaliacao = selecionar_ultima_avaliacao
) %>% select(especie_original, cat_ameaca_out, avaliacao, fonte) %>%
  distinct()

cncflora_format_out <- cncflora_format_out %>%
  mutate(fonte = if_else(avaliacao == 1, paste(fonte, "1", sep = "_"), paste(fonte, "0", sep = "_"))) %>%
  select(-avaliacao)
summary(cncflora_format_out)
#save
output <- "data/dados_formatados"
write_csv(cncflora_format_out, fs::path(output, "cncflora_out_format.csv"))

# FORMATA CNCFLORA WEB ----
#cncflora pagina web ler e formatar
cncflora_web <- read_csv("./data/dados_crus/status_cncflora.csv") %>% select(-1)
cncflora_web <- janitor::clean_names(cncflora_web) %>%
  mutate(fonte = "cncflora_web")
cncflora_wformat <- cncflora_web %>% rename(
  especie_original = search_str,
  cat_ameaca_web = threat_status) %>%
  select(especie_original, cat_ameaca_web, fonte) %>%
  filter(!is.na(especie_original)) %>%
  distinct()
cncflora_wformat
#save
output <- "data/dados_formatados"
write_csv(cncflora_wformat, fs::path(output, "cncflora_web_format.csv"))

#FORMAT CNCFLORA CKAN ----
cncflora_ckan <- read_csv("data/dados_crus/avaliacao-do-risco-de-extincao-de-arvore-nativas-do-brasil-2018-junho-2020.csv")
cncflora_ckan <- janitor::clean_names(cncflora_ckan) %>%
  mutate(fonte = "cncflora_ckan") %>%
  distinct()
count(cncflora_ckan, especie_sem_autor) %>% arrange(desc(n))
names(cncflora_ckan)
cncflora_format <- cncflora_ckan %>%
  rename(
  especie_original = especie_sem_autor,
  cat_ameaca_ckan = categoria) %>%
  select(especie_original, cat_ameaca_ckan, fonte) %>%
  distinct()

#save
output <- "data/dados_formatados"
write_csv(cncflora_format, fs::path(output, "cncflora_ckan_format.csv"))

#junta e ve tudo cncflora ----
cncflora <- bind_rows(cncflora_format, cncflora_wformat) %>%
  group_by(especie_original) %>%
  arrange(especie_original) %>%
  mutate(fontes = paste(fonte, collapse = "|")) %>%
  #mutate(cats = paste(cat_ameaca_cncflora, collapse = "|")) %>%
  mutate(cat_ameaca_cncflora =
           case_when(
             is.na(cat_ameaca_ckan) & !is.na(cat_ameaca_web) ~ cat_ameaca_web,
             is.na(cat_ameaca_web) & !is.na(cat_ameaca_ckan) ~ cat_ameaca_ckan,
             !is.na(cat_ameaca_web) & !is.na(cat_ameaca_ckan) ~ cat_ameaca_ckan
                     )) %>%
  select(-cat_ameaca_ckan, -cat_ameaca_web, -fonte) %>%
  rename(fonte = fontes) %>%
  arrange(especie_original) %>%
  distinct() %>%
  ungroup()
View(cncflora)
nrow(cncflora_wformat) +  nrow(cncflora_format)

# checa quais dessas especies estao em sao paulo
cncflora_flora <- get.taxa(cncflora$especie_original,
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
cncflora_all <- full_join(cncflora, cncflora_flora)
cncflora_all <- distinct(cncflora_all)

#only SP
cncflora_SP <- cncflora_all %>%
  filter(!is.na(occurrence)) %>%
  filter(stringr::str_detect(occurrence, pattern = "SP"))
#2240
cncflora_SP %>% ungroup() %>% count(fontes) %>% View()
ineditas <- cncflora_SP %>%
  filter(fontes %in% c("cncflora_out_0", "cncflora_out_1"))
  write.csv(ineditas, "output/05_cncflora_inedita.csv")
cncflora_out
indeitas2 <- cncflora_out %>% filter(taxa_avaliado %in% ineditas$especie_original)
write.csv(indeitas2, "output/05_cncflora_inedita_planilha.csv")
# segundo a pagina do cncflora e a flora do brasil, haveria 2100 especies que
#ja foram avaliadas pelo cncflora e estao na pagina eles que se encotram no estado de sao paulo
#segundo o cncflora ate outubro sao 2240! boa.

#salva so nome e cat_ameaca
cncflora_SP <- cncflora_SP %>% select(especie_original, starts_with("cat_ameaca"), fonte)
write_csv(cncflora_SP, "data/dados_formatados/cncflora_web_SP_format.csv", na = "s.i.")



