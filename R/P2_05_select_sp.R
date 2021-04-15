full_join(a, b) %>%
  #   full_join(c) %>%
  #   full_join(d) %>%
      arrange(sp) %>%
     mutate(sum = rowSums(across(where(is.logical)), na.rm = T)) %>% View()


library(readxl)
library(dplyr)
spp <- read_excel("output/p2/Lista de espécies-geral.xlsx", sheet = 1)
names(spp)

spp$nome_aceito_correto[spp$nome_aceito_correto == "Maxillaria meleagris Lindl."] <- "Maxillaria meleagris"

dim(spp)

entra <- spp %>%
  select(nome_aceito_correto, `ENTRA OU NAO`) %>%
  count(nome_aceito_correto, `ENTRA OU NAO`) %>%
  group_by(nome_aceito_correto) %>%
  mutate(ameacada = paste(`ENTRA OU NAO`, collapse = "-")) %>%
  arrange(nome_aceito_correto) %>%
  ungroup() %>%
  #count(ameacada)
  mutate(entra = if_else(ameacada %in% c("AMEACADA", "AMEACADA-NAO"), "entra", "nao entra")) %>%
  select(nome_aceito_correto, entra) %>%
  distinct()
entra
readr::write_csv(entra, "output/p2/03_entra.csv")

spp %>% filter(nome_aceito_correto %in% dupl) %>% View()
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
resumo <- p2 %>% left_join(cruza)
resumo %>% write_csv("output/p2/02_resumo.csv")
  resumo %>% View()
resumo %>% count(elegivel_produto_1, splink_t20, gbif_t20) %>% View()

resumo <- readr::read_csv("output/p2/02_resumo.csv")


junto <- left_join(spp, resumo)
write_csv(junto, "output/p2/P1_atualizado.csv")
