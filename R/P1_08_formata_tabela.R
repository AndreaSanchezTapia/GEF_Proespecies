# formata tudo
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

lista <- read_csv("output/04_tudo_sem formato.csv", na = "s.i.")
flora <- read_csv("output/03_flora_tudo.csv", na = "s.i")

#renomear
lista <- lista %>%
  rename(familia = family,
         especie_autor = scientific.name,
         busca_FB = original.search,
         notas = notes)

#marco lista buenA
especies <- lista %>% select(starts_with("especie")) %>% distinct()
ameaca <- select(lista, starts_with(c("especie", "cat_ameaca")))

especies %>% count(especie_original, especie_autor) %>% arrange(desc(n))

especies_df <- especies %>%
  group_by(especie_autor) %>%
  mutate(especies_originais = paste(especie_original, collapse = ";")) %>%
  dplyr::select(-especie_original) %>%
  distinct() %>%
  ungroup()
names(especies_df)

filter(especies_df, is.na(especie_autor)) %>% write_csv("output/05_not_found_by_FB_correct.csv")
count(lista , notas, taxon.status)
filter(lista, notas == "not found") %>% write_csv("output/05_not_found_by_FB.csv")
#outros nomes que precisam ser checados
filter(lista,
       !notas %in% c("not found", "", "replaced synonym")) %>%
  filter(taxon.status %in% c(NA)) %>%
  #count(notas, taxon.status)
  select(especie_original, notas, taxon.status, starts_with("cat")) %>%
    write_csv("output/05_check_names.csv")

# A ESPECIE OCORRE EM SAO PAULO SEGUNDO A FLORA DO BRASIL?
lista <- lista %>%
  mutate(ocorrencia_sp_FB2020 = if_else(str_detect(string = occurrence, pattern = "SP"), T, F))
#importante: 2431 nomes nao parecem ocorrer em SP
  count(lista, ocorrencia_sp_FB2020)

lista <- lista %>%
  mutate(nota_ocorrencia = if_else(!ocorrencia_sp_FB2020,'A espécie não ocorre no Estado de São Paulo segundo a Flora do Brasil', ""))

# algum problema na grafia?
count(lista, notas, taxon.status)
lista <- lista %>%
  mutate(nota_nome = if_else(str_detect(notas, "mispelled"), "Nome original mal escrito", "")) %>%
#Na taxonomia?
  mutate(nota_taxo = if_else(str_detect(notas, "synonym"), "Nome original era sinônimo", "")) %>%
  mutate(nota_found = if_else(str_detect(notas, "not found"), "Nome não encontrado na FB2020", "")) %>%
  mutate(nota_checar = if_else(str_detect(notas, "check"), "Nome ainda precisa ser checado", "")) %>%
  mutate(nota_sinonimo = if_else(str_detect(taxon.status, "synonym"), "Nome ainda é sinônimo, checar", ""))

lista$numero_de_fontes <- lista %>% select(starts_with("cat_ameaca")) %>% mutate_all(.funs =  function(x) if_else(is.na(x), 0, 1)) %>% rowSums(.)
lista %>% count(numero_de_fontes, ocorrencia_sp_FB2020)


#nada do sima bate
all(lista$cat_ameaca_br == lista$cat_ameaca_br_sima)
count(lista, cat_ameaca_br, cat_ameaca_br_sima)
all(lista$cat_ameaca_sp == lista$cat_ameaca_sp_sima)
all(lista$cat_ameaca_cncflora_1 == lista$cat_ameaca_cncflora_sima)
all(lista$cat_ameaca_cncflora_0 == lista$cat_ameaca_cncflora_sima)
all(lista$cat_ameaca_mpoSP == lista$cat_ameaca_sp)

lista <- unite(lista, cat_ameaca_cncflora, cat_ameaca_cncflora_0, cat_ameaca_cncflora_1,remove = F,sep = "|", na.rm = T)
lista$cat_ameaca_cncflora

lista
lista %>% filter(str_detect(taxon.status, "synonym")) %>% write_csv("output/05_sinonimos_segundo_FB2020.csv")

lista %>%
  filter(especie_original == "Stenandrium diphyllum") %>% View()
lista %>%
  filter(especie_original == "Vismia martiana") %>% View()
