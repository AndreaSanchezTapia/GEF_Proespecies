devtools::install_github("cadubio/splinkr")
library(splinkr)
dado$tombo_herbario_origem[1]
dado$Nome_coletor[1]
dado$numero_coletor[1]
lista2 <- list()
for (i in 1:length(unique(dado$especie))) {
  sp <- unique(dado$especie)[i]
  nome <- unique(dado$especie)[i]
  lista2[[i]] <- splinkr_records(
  scientificName = sp
                )

}
warnings()
names(lista2) <- unique(dado$especie)
listona <- bind_rows(lista2)
names(listona)
write_csv(listona, "output/p4/all_new.csv")
names(lista2[[1]])

purrr::map2(.x = lista2,
           .y = dado,
           ~filter(.x, )
           ~filter(recordNumber %in% .y))
names(dado)
names(listona)
test <- listona %>%
  filter(recordedBy %in% dado$Nome_coletor |
           year %in% dado$ano_coleta |
           catalogNumber %in% dado$tombo_herbario_origem |
           recordNumber %in% as.numeric(dado$numero_coletor) |
           as.character(recordNumber) %in% dado$numero_coletor)
           )
spp <- dado %>% filter(`NOVA AVALIACÃO` != "SIM") %>% arrange(especie) %>% distinct(especie) %>% pull(especie)
dado %>% arrange(especie) %>%
  #filter(`NOVA AVALIACÃO` != "SIM") %>%
  filter(especie == spp[41]) %>%
  select(especie, `NOVA AVALIACÃO`, Nome_coletor, numero_coletor, ano_coleta, tombo_herbario_origem, herbario_origem)
names(dado)
write_csv("output/p4/p4_narrow.csv")

dado %>% arrange(especie) %>% View()
