#formats cncfloraweb

#cncflora pagina web ler e formatar
cncflora_flora <- read_csv("./data/dados_crus/status_cncflora.csv")
sum(is.na(cncflora_flora$threat.status))

#rename
cncflora_format <- cncflora_flora %>%
  rename(especie = search.str,
         cat_ameaca_cncfloraweb = threat.status) %>%
  select(cat_ameaca_cncfloraweb, especie)
#save
output <- "data/dados_formatados"
write_csv(cncflora_format, fs::path(output, "status_cncflora_format.csv"))

# checa quais dessas especies estao em sao paulo

cncfloraweb_flora <- get.taxa(cncflora_format$especie,
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
cncfloraweb_flora$especie <- cncfloraweb_flora$original.search
#junta
cncfloraweb_all <- full_join(cncflora_format, cncfloraweb_flora)
cncfloraweb_all <- distinct(cncfloraweb_all)

#only SP
cncfloraweb_SP <- cncfloraweb_all %>%
  filter(!is.na(occurrence)) %>%
  filter(str_detect(occurrence, pattern = "SP"))

# sgundo a pagina do cncflora e a flora do brasil, haveria 2100 especies que
#ja foram avaliadas pelo cncflora e estao na pagina eles que se encotram no estado de sao paulo

#temos sinonimos
cncfloraweb_SP %>% count(taxon.status, notes)

#
cncfloraweb_SP %>% filter(notes == "replaced synonym")
cncfloraweb_SP %>% count(threat.status)

# algumas especies mudaram de estatus de conservacao na hora de mudar o nome
cncflora_shift_status <- cncfloraweb_SP %>%
  filter(cat_ameaca_cncfloraweb != threat.status)
write_csv(cncflora_shift_status, "output/04_cncflora_shift_status.csv")

#salva so nome e cat_ameaca
cncfloraweb_SP %>% select(especie, cat_ameaca_cncfloraweb) %>%
  write_csv("data/dados_formatados/cncflora_SP_format.csv")
