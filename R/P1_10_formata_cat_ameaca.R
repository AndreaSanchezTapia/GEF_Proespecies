library(readr)
library(dplyr)
library(stringr)

tudo <- read_csv("output/07_lista_anotada.csv", guess_max = 6000)
# atualiza cat ameaca br
tudo_updated <- tudo %>%
  mutate(cat_ameaca_br_updated = case_when(!is.na(cat_ameaca_br) & !is.na(cat_ameaca_br_sima) ~ cat_ameaca_br,
              is.na(cat_ameaca_br) & !is.na(cat_ameaca_br_sima) ~ cat_ameaca_br_sima,
             !is.na(cat_ameaca_br) &  is.na(cat_ameaca_br_sima) ~ cat_ameaca_br))
#atualiza cat ameaca cncflora
tudo_updated <- tudo_updated %>%
  mutate(cat_ameaca_cncflora_updated =
           case_when(!is.na(cat_ameaca_cncflora) & !is.na(cat_ameaca_cncflora_sima) ~ cat_ameaca_cncflora,
              is.na(cat_ameaca_cncflora) & !is.na(cat_ameaca_cncflora_sima) ~ cat_ameaca_cncflora_sima,
             !is.na(cat_ameaca_cncflora) &  is.na(cat_ameaca_cncflora_sima) ~ cat_ameaca_cncflora))

#atualiza cat ameaca cncflora
tudo_updated <- tudo_updated %>%
  mutate(cat_ameaca_sp_updated =
           case_when(!is.na(cat_ameaca_sp) & !is.na(cat_ameaca_sp_sima) ~ cat_ameaca_sp,
              is.na(cat_ameaca_sp) & !is.na(cat_ameaca_sp_sima) ~ cat_ameaca_sp_sima,
             !is.na(cat_ameaca_sp) &  is.na(cat_ameaca_sp_sima) ~ cat_ameaca_sp))
tudo_updated <- tudo_updated %>% rename(
  cat_ameaca_br_new = cat_ameaca_br,
  cat_ameaca_br = cat_ameaca_br_updated,
  cat_ameaca_sp_new = cat_ameaca_sp,
  cat_ameaca_sp = cat_ameaca_sp_updated,
  cat_ameaca_cncflora_new = cat_ameaca_cncflora,
  cat_ameaca_cncflora = cat_ameaca_cncflora_updated
)
campos_p1
names(tudo)[1:10]
tudo_updated <- tudo_updated %>% select(any_of(names(tudo)[1:10]),
                        starts_with("nota"),
                        starts_with("cat_ameaca"),
                        -ends_with("new"),
                        -ends_with("sima"),
                        ) %>%
  unite(col = "notas", starts_with("nota"), na.rm = T, sep = "/")
write_csv(tudo_updated, "output/08_lista_categoria_atualizada.csv")


# atualiza cat_ameaca_br
# atualiza cat_ameaca_cncflora
# atualiza cat_ameaca_sp
# atualiza cat_ameaca_mposp

count(tudo, nota_ocorrencia)
Não_SP <- tudo %>% filter(str_detect(nota_ocorrencia, "não"))
Sim_SP <- tudo %>% filter(is.na(nota_ocorrencia))
Sim_SP %>% count(fontes) %>% View()



sima <- filter(tudo, str_detect(fontes, "sima")) %>% distinct()
SP <- filter(tudo, str_detect(fontes, "SP_oficial")) %>% distinct()
cncflora <- filter(tudo, str_detect(fontes, "cncflora")) %>% distinct()
portaria <- filter(tudo, str_detect(fontes, "Portaria")) %>% distinct()
uicn <- filter(tudo, str_detect(fontes, "API")) %>% distinct()
mpoSP <- filter(tudo, str_detect(fontes, "mpoSP")) %>% distinct()
CRLAC <- filter(tudo, str_detect(fontes, "CR_Lac")) %>% distinct()
