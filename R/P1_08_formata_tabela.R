# formata tudo
library(readr)
library(dplyr)
library(stringr)
library(tidyr)

campos_p1
lista <- read_csv("output/06_lista_com_grupo.csv")
names(lista)
lista %>%
  filter(!is.na(accepted_name)) %>%
  count(especie_original, scientific_name, accepted_name, taxon_status)
#renomear
lista <- lista %>%
  rename(familia = family,
         especie_autor = scientific_name,
         genero = genus,
         epiteto_especifico = specific_epiteth,
         epiteto_infraespecifico = infra_epiteth,
         nome_aceito_correto = search_str)
# conferir taxonrank
count(lista, taxon_rank)
count(lista, notes)
#lista %>% filter(taxon_rank != "species") %>% View()

#checa a especies duplicadas
sp_dupl <- lista %>%
  count(especie_original) %>%
  arrange(desc(n)) %>%
  filter(n == 2) %>% pull(especie_original)
especies_duplicadas <- filter(lista, especie_original %in% sp_dupl)
lista <- lista %>%
  mutate(notas_dupl = if_else(especie_original %in% sp_dupl, "nome duplicado com critério conflitante na base original", ""))


#outros nomes que precisam ser checados
check <- filter(lista,
       !notes %in% c("", "replaced synonym")) %>%
  filter(taxon_status %in% c(NA)) %>%
  #count(notas, taxon.status)
  select(especie_original, notes, taxon_status, starts_with("cat")) %>%
  arrange(especie_original)
write_csv(check, "output/05_check_names.csv")

# A ESPECIE OCORRE EM SAO PAULO SEGUNDO A FLORA DO BRASIL?
lista <- lista %>%
  mutate(ocorrencia_sp_FB2020 = if_else(str_detect(string = occurrence, pattern = "SP"), T, F))
#importante: 2431 nomes nao parecem ocorrer em SP
  count(lista, ocorrencia_sp_FB2020)

lista <- lista %>%
  mutate(nota_ocorrencia = if_else(!ocorrencia_sp_FB2020,'A espécie não ocorre no Estado de São Paulo segundo a Flora do Brasil', ""))

# algum problema na grafia?
count(lista, notes)
lista <- lista %>%
  mutate(nota_nome = if_else(str_detect(notes, "misspelled"), "Nome original mal escrito", "")) %>%
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

# apta
lista %>%
  mutate(apta)
#seleciona campos
View(lista_anotada)
lista_anotada <- lista %>%
    select(grupo, familia, especie_original, sinonimia, fonte_sinonimia,
                 especie, genero, epiteto_especifico, taxon_rank, epiteto_infraespecifico, starts_with("nota"), starts_with("cat_ameaca"),fontes)
)
lista_anotada[is.na(lista_anotada)] <- ""
write_csv(lista_anotada, "output/07_lista_anotada.csv")
