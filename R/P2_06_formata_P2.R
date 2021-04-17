
#produto 2
p2 <- readr::read_csv("output/p2/p2_inicial.csv")
names(p2)
dim(p2)
setdiff(entra$nome_aceito_correto, p2$nome_aceito_correto)
setdiff(p2$nome_aceito_correto, entra$nome_aceito_correto)


cruza <- read_csv("output/p2/01_cruzam_shapes.csv") %>%
  rename(nome_aceito_correto = sp)
entra <- read_csv("output/p2/03_entra.csv")
cat_ameaca <- read_csv("output/p2/04_cat_ameaca.csv")
notas <- read_csv("output/p2/05_notas.csv")

resumo <- p2 %>%
  left_join(notas) %>%
  left_join(cat_ameaca) %>%
  left_join(entra) %>%
  left_join(cruza)
names(resumo)


library(stringr)
resumo <- resumo %>%
  mutate(especialista =
           case_when(
             sum > 0  & str_detect(notas_all, "ocorre") ~ "FBnao- OCCScruzam",
             sum == 0 & elegivel_produto_1 == "apta" ~ "FBsim- OCCSnaocruzam")
  )

  write_csv(resumo, "output/p2/02_resumo.csv")


#a primeira versao do resumo ainda usava spp como base,
#junto <- left_join(spp, resumo)
#write_csv(junto, "output/p2/P1_atualizado.csv")
