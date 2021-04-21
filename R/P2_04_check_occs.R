library(readr)
library(dplyr)
library(Rocc)
library(stringr)
library(sf)
library(tmap)
library(furrr)
plan(sequential)
#lee las tablas de ocurrencias
gbif <- list.files("output/p2/occs/clean",full.names = T, pattern = ".csv")
splink <- list.files("output/p2/occs/splink",full.names = T, pattern = ".csv")

#checar arquivos vazios
empty <- file.info(gbif)[["size"]] == 0
empty <- file.info(splink)[["size"]] == 0

#unlink(empty)
splinks <- file_data_frame("output/p2/occs/splink")
gbifs <- file_data_frame("output/p2/occs/clean") %>%
  mutate(names = stringr::str_remove(names, "_SYNONYM")) %>%
  mutate(names = stringr::str_remove(names, "_NAO_LIMPOU_CHECK_NOME"))

sp_link_entra
splinks
gbifs
df_all <- full_join(sp_link_entra, gbifs, by = "names") %>%
  rename(splink = paths.x , gbif = paths.y)
length(unique(df_all$names))
tail(df_all) %>% View()

#le os shapes
rbcv <- read_sf("data/dados_crus/RBCV_Limite/RBCV_Limite_datageo_jan2020.shp") %>% select(OID_, Name)
t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
sp  <- read_sf("data/dados_crus/SP_UF_2020/SP_UF_2020.shp")
shape <- sp
tabela <- df_all$splink[2]
names <- df_all$names[2]
test <- read_csv(tabela) %>% select(decimalLongitude, decimalLatitude) %>% filter(complete.cases(.))
#checa se cruza
destdir <- "output/p2/occs_cruza"
cruza_shape <- function(names, tabela, shape, destdir) {
  ss <- names
  if(!dir.exists(destdir)) dir.create(destdir, recursive = T)
  st <- tryCatch({
    shp <- read_sf(tabela, options = c("X_POSSIBLE_NAMES=decimalLongitude",
                                "Y_POSSIBLE_NAMES=decimalLatitude"),)
      },
      error = function(e) {
        message(paste(ss,"problem"))
        #message(e)
        })
    if ("sf" %in% class(st) ) {
      st <- st_set_crs(st, st_crs(shape))
      joins <- st_join(st, shape, left = F)


      if (nrow(joins) > 0) st_write(joins,
                                    paste0(destdir,"/", ss, "_occs.csv"), layer_options = "GEOMETRY=AS_XY")
      return(tibble(sp = ss,
                    inside = if_else(nrow(joins) == 0, FALSE, TRUE)))

    }
}
shape <- sp
tabela <- df_all$splink[2]
names <- df_all$names[2]
plan(multisession, workers = 15)

pasta_out_occs <- "output/p2/cruza_shape/t20/splink/"
cruza_t20_splink  <- furrr::future_map2(.x = df_all$splink,
                                        .y = df_all$names,
       ~cruza_shape(names = .y, tabela = .x, shape = t20,
                    destdir = pasta_out_occs), .progress = T)


pasta_out_occs <- "output/p2/cruza_shape/rbcv/splink/"
cruza_rbcv_splink  <- furrr::future_map2(.x = df_all$splink,
                                        .y = df_all$names,
                                        ~cruza_shape(names = .y, tabela = .x, shape = rbcv, destdir = pasta_out_occs), .progress = T)


pasta_out_occs <- "output/p2/cruza_shape/sp/splink/"
cruza_sp_splink <- furrr::future_map2(.x = df_all$splink,
                                        .y = df_all$names,
                                        ~cruza_shape(names = .y, tabela = .x, shape = sp, destdir = pasta_out_occs), .progress = T)



pasta_out_occs <- "output/p2/cruza_shape/t20/gbif/"
cruza_t20_gbif <- furrr::future_map2(.x = df_all$gbif,
                                      .y = df_all$names,
                                      ~cruza_shape(names = .y, tabela = .x, shape = t20, destdir = pasta_out_occs), .progress = T)

pasta_out_occs <- "output/p2/cruza_shape/rbcv/gbif/"
cruza_rbcv_gbif <- furrr::future_map2(.x = df_all$gbif[-2],
                                      .y = df_all$names[-2],
                                      ~cruza_shape(names = .y, tabela = .x, shape = rbcv, destdir = pasta_out_occs), .progress = T)

pasta_out_occs <- "output/p2/cruza_shape/sp/gbif/"
cruza_sp_gbif <- furrr::future_map2(.x = df_all$gbif,
                                      .y = df_all$names,
                                      ~cruza_shape(names = .y, tabela = .x, shape = sp, destdir = pasta_out_occs), .progress = T)


plan(sequential)

# resume

a <- cruza_t20_splink %>% bind_rows() %>% rename(splink_t20 = inside)
b <- cruza_t20_gbif %>% bind_rows() %>% rename(gbif_t20 = inside)
c <- cruza_rbcv_splink %>% bind_rows() %>% rename(splink_rbcv = inside)
d <- cruza_rbcv_gbif %>% bind_rows() %>% rename(gbif_rbcv = inside)
f <- cruza_sp_splink %>% bind_rows() %>% rename(splink_sp = inside)
e <- cruza_sp_gbif %>% bind_rows() %>% rename(gbif_sp = inside)

#salva quem cruza e quem nao
cruza_new <- full_join(a, b) %>%
 # full_join(c) %>%
#  full_join(d) %>%
  full_join(f) %>%
  full_join(e) %>%
  arrange(sp) %>%
  mutate(sum = rowSums(across(where(is.logical)), na.rm = T)) %>% rename(nome_aceito_correto = sp)
write_csv(cruza_new, "output/p2/01_cruzam_shapes_new.csv")

cruza_old <- read_csv("output/p2/01_cruzam_shapes.csv") %>% rename(nome_aceito_correto = sp)
old <- left_join(entra, cruza_old)
new <- left_join( entra, cruza_new)

left_join(old, new, by = "nome_aceito_correto") %>% View()

