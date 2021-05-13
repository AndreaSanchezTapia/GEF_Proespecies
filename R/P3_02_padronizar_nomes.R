#nome de coletor
p3_corr$Nome_coletor_original
devtools::load_all("../../3_coleguinhas/plantR/plantR/")
p3_corr$Nome_coletor_padronizado2 <- prepName(p3_corr$Nome_coletor_original,sep.out = "; ", special.char = TRUE)

p3_corr %>% count(Nome_coletor_original, Nome_coletor_padronizado, Nome_coletor_padronizado2) %>% View()
p3_corr %>% select(Nome_coletor_original, Nome_coletor_padronizado, Nome_coletor_padronizado2) %>% filter(Nome_coletor_padronizado != Nome_coletor_padronizado2) %>% View()
str_split(p3_corr$Nome_coletor_padronizado2, ";")


p3_corr <- p3_corr %>%
  mutate(Nome_coletor_padronizado3 = if_else(str_detect(Nome_coletor_padronizado, "et al."), paste(Nome_coletor_padronizado2, "; et.al"), Nome_coletor_padronizado2))

write_csv(p3_corr, "output/p3/p3_municipio_localidade_match_coletores.csv")

p3_corr$numero_coletor_padronizado1 <- colNumber(p3_corr$numero_coletor)
write_csv(p3_corr, "output/p3/p3_municipio_localidade_match_coletores_numero.csv")

count(p3_corr, numero_coletor, numero_coletor_padronizado1) %>% View()
names(p3_corr)

#año
names(p3_corr)
p3_corr$ano_coleta
ano_plantR <- getYear(p3_corr$ano_coleta)
data.frame(p= ano_plantR, a= p3_corr$ano_coleta) %>% count(p == a)
# plantR::prepDup(x = data.frame(p3_corr), col.names = c(family = "familia",
#                                            species = "especie",
#                                            col.name = "Nome_coletor_padronizado",
#                                            #col.name = "Nome_coletor_padronizado2",
#                                            col.number = "numero_coletor_padronizado1",
#                                            col.year = "ano_coleta"),
#                 rec.ID = "tombo_herbario_origem")
p3_corr %>% count(especie, Nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta) %>%
  filter(numero_coletor_padronizado1 != "s.n.") %>%
  filter(n > 1) %>%
arrange(desc(n)) %>% write_csv("output/p3/duplicatas_preliminar.csv")
p3_corr %>% count(especie, Nome_coletor_padronizado2, numero_coletor_padronizado1, ano_coleta, nome_instituicao) %>%
  filter(numero_coletor_padronizado1 != "s.n.") %>%
arrange(especie, Nome_coletor_padronizado2,numero_coletor_padronizado1, ano_coleta) %>%
  filter(n > 1) %>%
  write_csv("output/p3/duplicados_preliminar.csv")



