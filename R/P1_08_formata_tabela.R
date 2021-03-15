# formata tudo
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

campos_p1
lista <- read_csv("output/06_lista_com_grupo.csv")
#renomear
lista <- lista %>%
  rename(familia = family,
         especie_autor = scientific_name,
         genero = genus,
         epiteto_especifico = specific_epiteth,
         epiteto_infraespecifico = infra_epiteth,
         especie = search_str)
# conferir taxonrank
count(lista, taxon_rank)
#lista %>% filter(taxon_rank != "species") %>% View()

#checa a especies duplicadas
sp_dupl <- lista %>%
  count(especie_original) %>%
  arrange(desc(n)) %>%
  filter(n == 2) %>% pull(especie_original)
especies_duplicadas <- filter(lista, especie_original %in% sp_dupl)
lista <- lista %>% mutate(notas_dupl = if_else(especie_original %in% sp_dupl, "nome duplicado na base original", ""))

#lista com os nomes bons
especies_df <- lista %>%
  group_by(especie_autor) %>%
  mutate(especies_originais = paste(especie_original, collapse = ";")) %>%
  dplyr::select(-especie_original) %>%
  distinct() %>%
  ungroup()
names(especies_df)

#
filter(especies_df, is.na(especie_autor)) %>% write_csv("output/05_not_found_by_FB_correct.csv")
count(lista , notes, taxon_status)
filter(lista, notes == "not found") %>% write_csv("output/05_not_found_by_FB.csv")
#outros nomes que precisam ser checados
filter(lista,
       !notes %in% c("not found", "", "replaced synonym")) %>%
  filter(taxon_status %in% c(NA)) %>%
  #count(notas, taxon.status)
  select(especie_original, notes, taxon_status, starts_with("cat")) %>%
  arrange(especie_original) %>%
    write_csv("output/05_check_names.csv")


# A ESPECIE OCORRE EM SAO PAULO SEGUNDO A FLORA DO BRASIL?
lista <- lista %>%
  mutate(ocorrencia_sp_FB2020 = if_else(str_detect(string = occurrence, pattern = "SP"), T, F))
#importante: 2431 nomes nao parecem ocorrer em SP
  count(lista, ocorrencia_sp_FB2020)

lista <- lista %>%
  mutate(nota_ocorrencia = if_else(!ocorrencia_sp_FB2020,'A espécie não ocorre no Estado de São Paulo segundo a Flora do Brasil', ""))

# algum problema na grafia?
lista <- lista %>%
  mutate(nota_nome = if_else(str_detect(notes, "mispelled"), "Nome original mal escrito", "")) %>%
#Na taxonomia?
  mutate(nota_taxo = if_else(str_detect(notes, "synonym"), "Nome original era sinônimo", "")) %>%
  mutate(nota_found = if_else(str_detect(notes, "not found"), "Nome não encontrado na FB2020", "")) %>%
  mutate(nota_checar = if_else(str_detect(notes, "check"), "Nome ainda precisa ser checado", "")) %>%
  mutate(nota_sinonimo = if_else(str_detect(taxon_status, "synonym"), "Nome ainda é sinônimo, checar", ""))

# sinonimi
names(lista)
lista <- lista %>%
  mutate(sinonimia = if_else(notes == "replaced synonym", paste("É sinônimo de", especie), "")) %>%
  mutate(fonte_sinonimia = if_else(notes == "replaced synonym", "FB2020", ""))
names(lista)
campos_p1
#seleciona campos
lista %>%
    select(grupo, familia, especie_original, sinonimia, fonte_sinonimia,
                 especie, genero, epiteto_especifico, taxon_rank, epiteto_infraespecifico, starts_with("nota"))
write_csv(lista, "07_lista_anotada")
