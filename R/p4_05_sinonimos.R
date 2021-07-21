p1 <- read_csv("output/p1/08_lista_categoria_atualizada.csv")
#p2 <- read_csv("output/p2/03_resumo_anotado.csv", guess_max = 70000)
#count(p2,nomes_originais)
#p2 %>% select(especie = nome_aceito_correto,
 #             nomes_originais) %>%
  #mutate(sinonimos = if_else(especie==nomes_originais, NA_character_, nomes_originais)) %>%
  #select(-nomes_originais) %>% count(sinonimos) %>% View()
p4 <- read_csv("output/p4/p4_updated.csv", guess_max = 12000)

p4 %>% select(id, especie) %>% left_join(p1)


p4 <- read_csv("output/p4/p4_updated.csv", guess_max = 12000)

nomes <- p1 %>% select(especie = especie_original,
                       sin = sinonimia,
                       nome_aceito_correto
) %>% distinct()
nomes2 <- p1 %>% select(especie = nome_aceito_correto,
                           sin = sinonimia,
                           especie_original) %>% distinct()
nomes3 <- nomes2[nomes2$especie %in% p4$especie,] %>% distinct()
avaliadas <- unique(p4$especie[unique(p4$especie) %in% nomes3$especie])
length(unique(nomes3$especie))
length(unique(p4$especie))

avaliados <- nomes3 %>%
  mutate(avaliado = case_when(
especie == especie_original ~ "nome_aceito_correto_avaliado",
#!especie %in% avaliadas ~ "nome_aceito_correto_nao_avaliado",
!is.na(sin) ~ paste("sinonimo_avaliado:", especie_original),
  )) %>% group_by(especie) %>% select(especie, avaliado) %>%
  arrange(avaliado) %>%
  filter(!is.na(avaliado)) %>%
  mutate(sinonimo_avaliado = paste(avaliado, collapse = "/")) %>%
  select(-avaliado) %>% distinct()
names(avaliados)

p4 %>% left_join(avaliados) %>% count(sinonimo_avaliado) %>% View()
p4 %>% left_join(avaliados) %>% write_csv("output/p4/p4_updated_sinonimos.csv")

t20 <- read_csv("data/dados_formatados/p4/PRODUTO 4 - Base de dados - Territorio 20 - São Paulo.xlsx - Lista_Especies_T20.csv")
t20 %>% left_join(avaliados) %>% write_csv("output/p4/p4_t20_updated.csv")
all(p4$especie %in% t20$especie)
all(t20$especie %in% p4$especie)
