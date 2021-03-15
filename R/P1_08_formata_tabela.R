# formata tudo
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
remotes::install_github("andreasancheztapia/flora")
library(flora)
campos_p1
lista <- read_csv("output/06_lista_com_grupo.csv")
names(lista)

#nombres_especiales <- lista %>%
 # filter(!is.na(accepted_name)) %>%
  #select(accepted_name) %>% distinct() %>% pull()
#names(nombres_especiales) <- nombres_especiales
nams <- purrr::map(nombres_especiales, ~flora::remove.authors(.x)) %>% unlist()


lista %>% select(especie_original, #sin autor,
                 scientific_name, #autor
                 accepted_name, #autor
                 original_search, #no autor
                 taxon_status) %>% View()
#lista
lista <- lista %>%
  mutate(nome_aceito_correto =
           case_when(
             !is.na(accepted_name) ~ accepted_name,
             is.na(accepted_name) ~ search_str))
#renomear
lista <- lista %>%
  rename(familia = family,
         especie_autor = scientific_name,
         genero = genus,
         epiteto_especifico = specific_epiteth,
         epiteto_infraespecifico = infra_epiteth
#         nome_aceito_correto = search_str
)
# conferir taxonrank
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
count(lista, taxon.status)
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
  mutate(sinonimia = if_else(notes == "replaced synonym", paste("É sinônimo de",
                                                                nome_aceito_correto), "")) %>%
  mutate(fonte_sinonimia = if_else(notes == "replaced synonym", "FB2020", ""))


# apta
#seleciona campos
lista <- lista %>%
mutate(apta = if_else(ocorrencia_sp_FB2020 == TRUE & taxon_status == "accepted", "apta", ""))

lista_anotada <- lista %>%
    select(grupo, familia, especie_original, sinonimia, fonte_sinonimia,
                 nome_aceito_correto, genero, epiteto_especifico, taxon_rank, epiteto_infraespecifico, starts_with("nota"), starts_with("cat_ameaca"), fontes, apta)
lista_anotada$apta
lista_anotada[is.na(lista_anotada)] <- ""
write_csv(lista_anotada, "output/07_lista_anotada.csv")
