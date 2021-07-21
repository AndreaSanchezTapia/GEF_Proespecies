library(sf)
source("R/functions.R")
mposIBGE <- mpos %>% dplyr::select(NM_MUN)
csv <- "output/p4/p4_base.csv"
final_sf <- read_sf(csv,
                    options = c("X_POSSIBLE_NAMES=long_corrigida", "Y_POSSIBLE_NAMES=lat_corrigida"))


shapes_PMSP <- list.files("data/dados_formatados/veg/IPF_Dados_Espaciais_Produto3/UCs_PMSP_2020/",
                          full.names = T, pattern = "shp$")
shapes_SPFF <- list.files("data/dados_formatados/veg/IPF_Dados_Espaciais_Produto3/UCs_SP_FF_2021//",
                          full.names = T, pattern = "shp$")
shapes_SPIBT <- list.files("data/dados_formatados/veg/IPF_Dados_Espaciais_Produto3/UCs_SP_IBT_2020///",
                           full.names = T, pattern = "shp$")
shapes_veg <- list.files("data/dados_formatados/veg/IPF_Dados_Espaciais_Produto3/Regioes_Fitoecologicas_RDAM_Brasil_SP",
                         full.names = T, pattern = "shp$")
shapes <- c(shapes_PMSP, shapes_SPFF, shapes_SPIBT, shapes_veg)
SHAPES <- purrr::map(shapes, ~sf::read_sf(.x))
crs <- purrr::map(SHAPES, ~sf::st_crs(.x))
wip_sf <- st_set_crs(final_sf, st_crs(mposIBGE))


tr <- purrr::map(SHAPES,
                 ~st_transform(wip_sf, st_crs(.x))
)
jn <- purrr::map2(tr,SHAPES,
                  ~st_join(.x, .y)
)


a <- select(jn[[1]], id, NOME) %>% st_drop_geometry()
b <- select(jn[[2]], id, NOME) %>% st_drop_geometry()
c <- select(jn[[3]], id, NOME_PARQU) %>% st_drop_geometry()
d <- select(jn[[4]], id, NOME_PARQU) %>% st_drop_geometry()
e <- select(jn[[5]], id, NOME) %>% st_drop_geometry()
f <- select(jn[[6]], id, UC) %>% st_drop_geometry()
g <- select(jn[[7]], id, Nome) %>% st_drop_geometry()
h <- select(jn[[8]], id, NOME) %>% st_drop_geometry()
j <- select(jn[[9]], id, regiao) %>% st_drop_geometry()

library(dplyr)
ucs <- a %>%
  full_join(b, by = "id") %>%
  full_join(c, by = "id") %>%
  full_join(d, by = "id") %>%
  full_join(e, by = "id") %>%
  #full_join(f, by = "id") %>%
  full_join(g, by = "id") %>%
  full_join(h, by = "id") %>%
  full_join(j, by = "id")
shapes
names(ucs) <- c("id",
                "APA_Bororé_Colonia",
                "APA_Capivari_Monos",
                "PNM_Rodo",
                "PNMCC",
                "PNMFC",
                "UC_IBT_PEFI",
                "Rebio_Paranapiacaba",
                "RADAM")

write_csv(ucs, "output/p4/p4_cruzamento_shapes.csv")
f %>%
  group_by(id) %>%
  mutate(uc = paste(UC, collapse = "/")) %>%
  select(-UC) %>%
  distinct() %>%
  write_csv("output/p4/p4_ucs.csv")



#shapes_UCWDPA <- list.files("data/dados_crus/WDPA", pattern = ".shp$", recursive = T , full.names = T)[c(2, 4, 6)]
#SHAPES <- purrr::map(shapes_UCWDPA, ~sf::read_sf(.x))

# crs <- purrr::map(SHAPES, ~sf::st_crs(.x))
# tr <- purrr::map(SHAPES,
#                  ~st_transform(wip_sf, st_crs(.x))
# )
# jn <- purrr::map2(tr,SHAPES,
#                   ~st_join(.x, .y)
# )
# wdpa <- purrr::map(jn,
#                    ~select(.x, id, NAME) %>% st_drop_geometry())
# names(wdpa) <- c("Federal", "Estadual", "Municipal")
# wdp_summary <- purrr::map(wdpa,
#                           ~count(.x, NAME))
# UCS_WDPA <- purrr::map(wdpa, ~.x %>% group_by(id) %>%
#                          mutate(NOME_UC= paste(NAME, collapse = "/")) %>%
#                          select(-NAME) %>%
#                          distinct())
# }
# bind_cols(UCS_WDPA) %>% write_csv("output/p4/4p4_WDPA.csv")
#

ucs <- list.files("data/dados_crus/UC_TODAS_LEO/", pattern = ".shp$", recursive = T , full.names = T)
ucsdf <- list.files("data/dados_crus/UC_TODAS_LEO/", pattern = ".dbf$", recursive = T , full.names = T)
readr::guess_encoding(ucsdf)

test <- rgdal::readOGR(ucs, encoding = "windows-1250")
ucshape <- st_as_sf(test)
#ucshape <- read_sf(ucs)


sf::st_crs(ucshape)
#wip_sf <- st_set_crs(wip3, st_crs(mposIBGE))
ucshape <- st_set_crs(ucshape, st_crs(wip_sf))

ucjoin <- st_join(wip_sf,ucshape)

ucjoin2 <- select(ucjoin, id, names(ucshape)) %>% st_drop_geometry()
apply(ucjoin2, 2, function(x) sum(is.na(x), na.rm = T))
library(dplyr)
library(readr)
ucjoin2 <- ucjoin2 %>% group_by(id) %>%
  mutate(NOME_UC = paste(NOME_UC1, collapse = "/")) %>%
  mutate(CATEGORIA = paste(CATEGORI3, collapse = "/")) %>%
  mutate(GRUPO = paste(GRUPO4, collapse = "/")) %>%
  mutate(ESFERA = paste(ESFERA5, collapse = "/")) %>%
  mutate(NOME_ORG = paste(NOME_ORG12, collapse = "/")) %>%
  select(id, NOME_UC, ESFERA) %>% distinct()


write_csv(ucjoin2,  "output/p4/p4_ucs_todas.csv")
ucjoin2 %>% left_join(ucs)
ucs <- ucs %>% rename(mun1 = APA_Bororé_Colonia,
              mun2 = APA_Capivari_Monos,
              mun3 = PNM_Rodo,
              mun4 = PNMFC,
              est1 = UC_IBT_PEFI,
              est2 = Rebio_Paranapiacaba
              ) %>% select(id, starts_with("est"),
                           starts_with("mun"))
ucs %>% count(est1, est2)

#cria boraceia
boraceia <- sf::read_sf("data/dados_crus/PESM NPDor - EBB USP 1.kmz")
boraceia2 <- st_transform(boraceia, st_crs(mposIBGE))
bora <- wip_sf %>% st_join(boraceia2) %>% select(id, Name) %>% filter(!is.na(Name)) %>%
  rename(est = Name)


library(tidyr)
uc_tudo <- ucjoin2 %>%
  pivot_wider(id_cols = id, names_from = ESFERA, values_from = NOME_UC) %>%
  separate("estadual/municipal", into = c("estadual2", "municipal2"), sep = "/") %>%
  separate("estadual/estadual", into = c("estadual3", "estadual4"), , sep = "/") %>%
  separate("federal/estadual", into = c("federal2", "estadual5"), , sep = "/") %>%
  separate("estadual/federal", into = c("estadual6", "federal3"), , sep = "/") %>%
  separate("municipal/municipal", into = c("municipal3", "municipal4"), , sep = "/") %>%
  select(starts_with("fed"), starts_with("est"), starts_with("mun")) %>%
  left_join(ucs) %>%
  left_join(bora) %>%
  unite(remove = TRUE, col = "federal", starts_with("fed"), sep = "/", na.rm = T) %>%
  unite(remove = TRUE, col = "estadual", starts_with("est"), sep = "/", na.rm = T) %>%
  unite(remove = TRUE, col = "municipal", starts_with("mun"), sep = "/", na.rm = T) %>%
  mutate(federal = stringr::str_to_title(federal)) %>%
  mutate(estadual = stringr::str_to_title(estadual)) %>%
  mutate(municipal = stringr::str_to_title(municipal)) %>%
  mutate(UC_federal = if_else(federal != "", "SIM", "NAO")) %>%
  mutate(UC_estadual = if_else(estadual != "", "SIM", "NAO")) %>%
  mutate(UC_municipal = if_else(municipal != "", "SIM", "NAO")) %>%
  select(
    UC_federal, nome_UC_federal = federal,
    UC_estadual, nome_UC_estadual = estadual,
    UC_municipal, nome_UC_municipal = municipal
         )
uc_tudo[uc_tudo == "Área De Proteçăo Ambiental Municipal Do Capivari-Monos/Apa Capivari - Monos"] <- "Área De Proteçăo Ambiental Municipal Do Capivari-Monos"
uc_tudo[uc_tudo == "Parque Natural Municipal Fazenda Do Carmo/Parque Natural Municipal Fazenda Do Carmo"] <- "Parque Natural Municipal Fazenda Do Carmo"
uc_tudo[uc_tudo == "Área De Proteçăo Ambiental Bororé-Colônia/Parque Natural Municipal Itaim"] <- "Área De Proteçăo Ambiental Bororé-Colônia/Parque Natural Municipal Itaim"
uc_tudo[uc_tudo == "Parque Natural Municipal Jaceguava/Parque Natural Municipal Jaceguava"] <- "Parque Natural Municipal Jaceguava"

write_csv(uc_tudo, "output/p4/p4_ucs_todas_format.csv")


veg <- ucs %>% select(id, RADAM)
final_sf_veg <- final_sf %>% left_join(veg) %>%
  mutate(tipo_veg_RADAM =if_else(
             situacao_coordenada %in% c("CO", "CR", "COSR") &
               !coordenada_aplicada %in% c("Cent_Mun", "Cent_Dist"), RADAM, "")) %>%
  sf::st_drop_geometry()
final_sf_veg %>% pull(tipo_veg_RADAM)
write_csv(final_sf_veg, "output/p4/p4_veg.csv")

colss <- names(final_sf_veg)
final_sf_uc <- final_sf_veg %>%
  select(-starts_with("UC"), -starts_with("nome_UC")) %>%
  left_join(uc_tudo) %>%
  select(-tipo_veg_RADAM) %>%
  mutate(tipo_veg_RADAM = RADAM) %>%
  select(colss) %>%
  select(-RADAM) %>%
  select(-conflito_3_rodada) %>%
  rename(conflito = conflito_municipios_new_new_new) %>%
  left_join(bora) %>%
  rename(UC_USP = est)
write_csv(final_sf_uc, "output/p4/p4_updated.csv")



left_join(final_sf_veg, bora) %>% rename(UC_USP = est)

count(final_sf_veg, tipo_veg_RADAM)
