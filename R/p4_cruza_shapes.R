library(sf)
csv <- "output/p4/p4_conflitos.csv"
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
wip_sf <- st_set_crs(df, st_crs(mposIBGE))

tr <- purrr::map(SHAPES,
                 ~st_transform(wip_sf, st_crs(.x))
)
jn <- purrr::map2(tr,SHAPES,
                  ~st_join(.x, .y)
)
# ucs <- purrr::map(jn,
#                   ~select(.x, id, NOME) %>% st_drop_geometry())

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

write_csv(ucs, "output/p3/p3_cruzamento_shapes.csv")
f %>% group_by(id) %>% mutate(uc= paste(UC, collapse = "/")) %>% select(-UC) %>% distinct() %>% write_csv("output/p3/p3_ucs.csv")
jn[[6]] %>% count(Label
)



shapes_UCWDPA <- list.files("data/dados_crus/WDPA", pattern = ".shp$", recursive = T , full.names = T)[c(2, 4, 6)]
SHAPES <- purrr::map(shapes_UCWDPA, ~sf::read_sf(.x))

crs <- purrr::map(SHAPES, ~sf::st_crs(.x))
#wip_sf <- st_set_crs(wip3, st_crs(mposIBGE))

tr <- purrr::map(SHAPES,
                 ~st_transform(wip_sf, st_crs(.x))
)
jn <- purrr::map2(tr,SHAPES,
                  ~st_join(.x, .y)
)
wdpa <- purrr::map(jn,
                   ~select(.x, id, NAME) %>% st_drop_geometry())
names(wdpa) <- c("Federal", "Estadual", "Municipal")
wdp_summary <- purrr::map(wdpa,
                          ~count(.x, NAME))
UCS_WDPA <- purrr::map(wdpa, ~.x %>% group_by(id) %>%
                         mutate(NOME_UC= paste(NAME, collapse = "/")) %>%
                         select(-NAME) %>%
                         distinct())
}
bind_cols(UCS_WDPA) %>% write_csv("output/p3/p3_WDPA.csv")


ucs <- list.files("data/dados_crus/UC_TODAS_LEO/", pattern = ".shp$", recursive = T , full.names = T)
ucshape <- read_sf(ucs)

sf::st_crs(ucshape)
#wip_sf <- st_set_crs(wip3, st_crs(mposIBGE))
ucshape <- st_set_crs(ucshape, st_crs(wip3))

ucjoin <- st_join(wip3,ucshape)

ucjoin2 <- select(ucjoin, id, names(ucshape)) %>% st_drop_geometry()
apply(ucjoin2, 2, function(x) sum(is.na(x), na.rm = T))

ucjoin2 %>% group_by(id) %>%
  mutate(NOME_UC = paste(NOME_UC1, collapse = "/")) %>%
  mutate(CATEGORIA = paste(CATEGORI3, collapse = "/")) %>%
  mutate(GRUPO = paste(GRUPO4, collapse = "/")) %>%
  mutate(ESFERA = paste(ESFERA5, collapse = "/")) %>%
  mutate(NOME_ORG = paste(NOME_ORG12, collapse = "/")) %>%
  select(id, NOME_UC, CATEGORIA, GRUPO, ESFERA, NOME_ORG) %>% distinct() %>%
  write_csv("output/p3/p3_ucs_todas.csv")
library(sf)
library(rgdal)
liibrary(st)

IF2020 <- st_read("data/dados_crus/IF2020/datageowms-InventarioFlorestal2020.kml", )
