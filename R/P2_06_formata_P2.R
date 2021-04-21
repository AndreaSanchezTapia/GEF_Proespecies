
cruza <- read_csv("output/p2/01_cruzam_shapes_new.csv")
tax <- read_csv("output/p2/10_base_P1.csv")
entra <- read_csv("output/p2/03_entra.csv")
cat_ameaca <- read_csv("output/p2/04_cat_ameaca.csv")
notas <- read_csv("output/p2/05_notas.csv")

resumo <- tax %>%
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
names(resumo)
resumo %>% count(elegivel_produto_1, splink_sp, gbif_sp) %>% View()
#a primeira versao do resumo ainda usava spp como base,
#junto <- left_join(spp, resumo)
#write_csv(junto, "output/p2/P1_atualizado.csv")
