full_join(a, b) %>%
  #   full_join(c) %>%
  #   full_join(d) %>%
      arrange(sp) %>%
     mutate(sum = rowSums(across(where(is.logical)), na.rm = T)) %>% View()


library(readxl)
library(dplyr)
spp <- read_excel("output/p2/Lista de espécies-geral para analise andrea- PR-14-04.xlsx", sheet = 2)
names(spp)
spp$nome_aceito_correto[spp$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"
dim(spp)
spp %>%
  select(nome_aceito_correto,
         `elegivel - Produto 1`,
         `Produto 1 - final`,
         `elegivel - Produto 2`) %>%

  count( `elegivel - Produto 1`,
         `Produto 1 - final`,
         `elegivel - Produto 2`)

#produto 2
p2 <- readr::read_csv("output/p2/p2_inicial.csv")
names(p2)
dim(p2)
setdiff(spp$nome_aceito_correto, p2$nome_aceito_correto)
setdiff(p2$nome_aceito_correto, spp$nome_aceito_correto)

#cruza
cruza <- read_csv("output/p2/01_cruzam_shapes.csv") %>%
  rename(nome_aceito_correto = sp)
p2 %>% left_join(cruza) %>%
  count(elegivel_produto_1, sum)
