library(readr)
library(dplyr)
library(stringr)
library(furrr)
#devtools::load_all("../../R_packages/plantR")
source("R/functions.R")
#cria os dataframes dos quatro grupos de dados----
biota <- file_data_frame("output/p2/occs/biota") %>%
  rename(biota = paths, nome_aceito_correto = names)
#cruza_biota <- file_data_frame("output/p2/cruza_shape/t20/biota/") %>%
 # rename(biota_cruza = paths, nome_aceito_correto = names)
splinks <- file_data_frame("output/p2/occs/splink") %>%
  rename(splink = paths, nome_aceito_correto = names)
#cruza_splinks <- file_data_frame("output/p2/cruza_shape/t20/splink/") %>%
 # rename(splink_cruza = paths, nome_aceito_correto = names)
gbifs <- file_data_frame("output/p2/occs/clean") %>%
  mutate(names = stringr::str_remove(names, "_SYNONYM")) %>%
  mutate(names = stringr::str_remove(names, "_NAO_LIMPOU_CHECK_NOME")) %>%
  rename(gbif = paths, nome_aceito_correto = names)
#cruza_gbif <- file_data_frame("output/p2/cruza_shape/t20/gbif/") %>%
 # rename(gbif_cruza = paths, nome_aceito_correto = names)
#cruza_gbif$nome_aceito_correto <- str_remove(cruza_gbif$nome_aceito_correto, "_occs")
#cruza_splinks$nome_aceito_correto <- str_remove(cruza_splinks$nome_aceito_correto, "_occs")

#so as selecionadas----
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas %>%
  select(nome_aceito_correto)

#omits biota here----
df_all <- left_join(especies, gbifs) %>%
  left_join(splinks) #%>%
#  left_join(cruza_gbif) %>%
 # left_join(cruza_splinks)


# PARA FORMATEAR!!!! -----
#cruza_all <- left_join(especies, cruza_gbif) %>%
#  left_join(cruza_splinks)
#plan(multisession, workers = 15)
#read_cruza_gbif <- furrr::future_map(na.omit(df_all$gbif_cruza),
 #                             ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
#                              .progress = T)
#names(read_cruza_gbif) <- df_all$nome_aceito_correto[-which(is.na(df_all$gbif_cruza))]
#read_cruza_splink <- furrr::future_map(na.omit(df_all$splink_cruza),
 #                             ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
  #                            .progress = T)
#names(read_cruza_splink) <- df_all$nome_aceito_correto[-which(is.na(df_all$splink_cruza))]

read_biota <- furrr::future_map(biota$biota,
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                             .progress = T)
names(read_biota) <- "CRbiota"

# gbif_dwc <- furrr::future_map(read_cruza_gbif,
#                               ~formatDwc(gbif_data = .x),
#                               .progress = T)
# gbif_dwc2 <- furrr::future_map(gbif_dwc,
#                                ~mutate(.x, across(where(is.integer), .fns = function(x) as.character(x))) %>%
#                                  mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
#                                  mutate(across(where(is.logical), .fns = function(x) as.character(x))),
#                                .progress = T)
# gbif_all2 <- bind_rows(gbif_dwc2)
# write_csv(gbif_all2, "output/p2/occs/CRUZA_GBIF_TUDO.csv")
# read_sf2("output/p2/occs/CRUZA_GBIF_TUDO.csv")
# dim(test)
# splink_dwc <- furrr::future_map(read_cruza_splink,
#                                 ~formatDwc(splink_data = .x),
#                                 .progress = T)
#
# splink_dwc2 <- furrr::future_map(splink_dwc,
#                                  ~mutate(.x, across(where(is.integer), .fns = function(x) as.character(x))) %>%
#                                    mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
#                                    mutate(across(where(is.logical), .fns = function(x) as.character(x))),
#                                  .progress = T)
# splink_all2 <- bind_rows(splink_dwc2)
# write_csv(splink_all2, "output/p2/occs/CRUZA_SPLINK_TUDO.csv")
# read_sf2("output/p2/occs/CRUZA_SPLINK_TUDO.csv")
#cruza_all <- bind_rows(gbif_all2, splink_all2)
#nrow(cruza_all)
#2856+10420
#write_csv(cruza_all, "output/p2/occs/CRUZA_TUDO.csv")
#read_sf2("output/p2/occs/CRUZA_TUDO.csv")

#separa los que no tienen coordenadas-----
#plan(multisession, workers = 15)

#lee todo
read_gbif <- furrr::future_map(na.omit(df_all$gbif),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_gbif) <- df_all$nome_aceito_correto[-which(is.na(df_all$gbif))]
read_splink <- furrr::future_map(na.omit(df_all$splink),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_splink) <- df_all$nome_aceito_correto[-which(is.na(df_all$splink))]

# numero de registros----
nrow_g <- furrr::future_map(read_gbif, ~nrow(.x), .progress = T)
purrr::simplify(nrow_g) %>% sum()
nrow_sp <- furrr::future_map(read_splink, ~nrow(.x), .progress = T)
purrr::simplify(nrow_sp) %>% sum()

# #no col----
# out_dir_sp <- "output/p2/occs/no_coord/splink"
# out_dir_gb <- "output/p2/occs/no_coord/gbig"
# dir.create(out_dir_gb, recursive = T)
# dir.create(out_dir_sp, recursive = T)
#
# #names gbif e specieslink
# long <- furrr::future_map(read_gbif, ~if_else("decimalLongitude" %in% names(.x), TRUE, FALSE), .progress = T)
# lat <- furrr::future_map(read_gbif, ~if_else("decimalLatitude" %in% names(.x), TRUE, FALSE), .progress = T)
#
# #lat[lat ==F]
# gbif_no_col <- df_all[df_all$nome_aceito_correto %in% no_cols,]
# gbif_col <- df_all[!df_all$nome_aceito_correto %in% no_cols,]
#
# long2 <- furrr::future_map(read_splink, ~if_else("decimalLongitude" %in% names(.x), TRUE, FALSE), .progress = T)
# lat2 <- furrr::future_map(read_splink, ~if_else("decimalLatitude" %in% names(.x), TRUE, FALSE), .progress = T)
# no_cols2 <- names(long2[long2 == F])
# splink_no_col <- df_all[df_all$nome_aceito_correto %in% no_cols2,]
# splink_col <- df_all[!df_all$nome_aceito_correto %in% no_cols2,]
#
# #leer y juntar esas
# g_no_col <- furrr::future_map(gbif_no_col$gbif,
#                                ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
#                                .progress = T)
# names(g_no_col) <- gbif_no_col$nome_aceito_correto
# s_no_col <- furrr::future_map(splink_no_col$splink,
#                                  ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
#                                  .progress = T)
# names(s_no_col) <- splink_no_col$nome_aceito_correto
#
# #mutate pre bind
# g_no_dwc <- furrr::future_map(g_no_col,
#                                 ~formatDwc(gbif_data = .x),
#                                 .progress = T)
# s_no_dwc <- formatDwc(splink_data = s_no_col$`Couepia meridionalis`)
# s_no_dwc2 <- s_no_dwc %>%
#   mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
#   mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
#   mutate(across(where(is.logical), .fns = function(x) as.character(x)))
#
# g_no_dwc2 <- furrr::future_map(g_no_dwc,
#                                  ~mutate(.x, across(where(is.integer), .fns = function(x) as.character(x))) %>%
#                                    mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
#                                    mutate(across(where(is.logical), .fns = function(x) as.character(x))),
#                                  .progress = T)
# all_no <- bind_rows(g_no_dwc2) %>% bind_rows(s_no_dwc2)
# nrow(all_no)
# write_csv(all_no, "output/p2/occs/ALL_NO_COLUMN.csv")
# read_sf2("output/p2/occs/ALL_NO_COLUMN.csv")
# #agora extrai as que nao tem coordenadas daquelas tabela grandes -----
# plan(multisession, workers = 15)
# any(is.na(gbif_col$nome_aceito_correto))
# any(gbif_col$nome_aceito_correto =="NA")
# any(is.na(gbif_col$gbif))
# read_gbif_tirar <- furrr::future_map(na.omit(gbif_col$gbif),
#                                ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
#                                .progress = T)
# names(read_gbif_tirar) <- gbif_col$nome_aceito_correto[which(!is.na(gbif_col$gbif))]
# read_splink_tirar <- furrr::future_map(na.omit(splink_col$splink),
#                                  ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
#                                  .progress = T)
# names(read_splink_tirar) <- splink_col$nome_aceito_correto[which(!is.na(splink_col$splink))]
#
# no_coord_gbif <- furrr::future_map(read_gbif_tirar,
#                                    ~filter(.x, (is.na(decimalLongitude) | is.na(decimalLatitude))),
#                                    .progress = T)
# no_coord_splink <- furrr::future_map(read_splink_tirar,
#                                      ~filter(.x, (is.na(decimalLongitude) | is.na(decimalLatitude))),
#                                      .progress = T)
# #mutate colunas
#
# plantr_SP <- function(x) {
#   data.frame(x) %>%
#     plantR::fixLoc(loc.levels = c("country", "stateProvince")) %>%
#   filter(country.new == "brazil" | is.na(country.new)) %>%
#   filter(stateProvince.new == "sao paulo" | is.na(stateProvince.new))
# }
#
# no_coord_SP <- furrr::future_map(no_coord_gbif,
#                   ~plantr_SP(.x),
#                   .progress = T)
# names(no_coord_SP)
#
# no_coord_SP_sl <- furrr::future_map(no_coord_splink,
#                   ~plantr_SP(.x),
#                   .progress = T)
# names(no_coord_SP_sl)
#
# #tudo filtradinho
# bind_rows(no_coord_SP)
# bind_rows(no_coord_SP_sl)
#
# mutate_para_bind <- function(x){
#   x %>%
#     mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
#     mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
#     mutate(across(where(is.logical), .fns = function(x) as.character(x)))
# }
# no_coord_SP <- furrr::future_map(no_coord_SP,
#                                  ~mutate_para_bind(.x),
#                                  .progress = T)
# no_coord_SP_sl <- furrr::future_map(no_coord_SP_sl,
#                                     ~mutate_para_bind(.x),
#                                     .progress = T)
# gb <- bind_rows(no_coord_SP) %>% mutate(data_source = "gbif")
# sl <- bind_rows(no_coord_SP_sl) %>% mutate(data_source = "splink")
# all_no_coord <- bind_rows(gb, sl)
# unique(all_no_coord$country.new)
# unique(all_no_coord$stateProvince)
# unique(all_no_coord$stateProvince.new)
# unique(all_no_coord$decimalLatitude)
# unique(all_no_coord$decimalLongitude)
# unique(all_no_coord$species)
#
# write_csv(all_no_coord, "output/p2/occs/ALL_NO_COORD.csv")
# test <- read_sf2("output/p2/occs/ALL_NO_COORD.csv")

t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos <-textclean::replace_non_ascii(tolower(t20$NM_MUNICIP))




#formatea cruza s[o]----
#pega todos os nomes para checar se tudo bem
all <- c(read_gbif, read_cruza_gbif, read_splink, read_cruza_splink, read_biota)
especies_all <- names(all)
names_raw <- furrr::future_map(all, ~names(.x), .progress = T)
todos_os_nomes <- purrr::simplify(names_raw) %>% unique() %>% sort()

# processamento a mao apra ver quais campos ficam em P2_10

plan(sequential)

campos_tax <- campos_ordenados[which(!campos_ordenados %in%  sel_fields)][-8]

format_all <- function(df, names) {
  df %>% select(one_of(sel_fields)) %>% #seleciona colunas
    mutate(nome_aceito_correto = names) %>%
}
fields_raw <- furrr::future_imap(all,
                                ~select(.y, one_of(sel_fields)) %>%
                                  mutate(nome_aceito_correto = .x), .progress = T)

add_nome <- furrr::future_map2(fields_raw,
                               especies_all,
                               ~mutate(.x, nome_aceito_correto = .y),
                               .progress = T)
add_nome <- furrr::future_map2(add_nome,
                               fonte_all,
                               ~mutate(.x, fonte = .y),
                               .progress = T)


dim_raw <- furrr::future_map(fields_raw, ~dim(.x), .progress = T)
dim_now <- furrr::future_map(add_nome, ~dim(.x), .progress = T)
names(dim_now) <- paste(especies_all, fonte_all, sep = "_")
dimensoes <- bind_rows(dim_now)
dimensoes_df <-  t(dimensoes) %>% data.frame() %>% tibble::rownames_to_column("dataset")
write_csv(dimensoes_df, "output/p2/09_dimensoes.csv")
plan(sequential)

names_now <- furrr::future_map(add_nome, ~names(.x), .progress = T)
names(names_now) <- paste(especies_all, fonte_all, sep = "_")

bind_rows(add_nome[1], add_nome[2])
unique(add_nome[[1]]$countryCode)

p1_base <- read_csv("output/p2/10_base_P1.csv")
flora_base <- read_csv("output/p1/03_flora_tudo.csv")
flora_base %>% rename(nome_aceito_correto = search.str) %>%
  names() %>%
  select()
names(p1_base)
tax <- p1_base %>%
  select(nome_aceito_correto, grupo, familia, genero, epiteto_especifico) %>%
  distinct()
####todo lo que era formatear la lista de tabelas salio para hablar de la table completa.
#########---
devtools::load_all("../../R_packages/plantR")
plan(multisession, workers = 15)
gbif_dwc <- furrr::future_map(read_gbif,
                  ~formatDwc(gbif_data = .x),
                  .progress = T)
devtools::load_all("../../R_packages/Rocc/")
splink_dwc <- furrr::future_map(read_splink,
                  ~formatDwc(splink_data = data.frame(.x)),
                  .progress = T)

gbif_dwc2 <- furrr::future_map(gbif_dwc,
                              ~mutate(.x, across(where(is.integer), .fns = function(x) as.character(x))) %>%
                              mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
                                mutate(across(where(is.logical), .fns = function(x) as.character(x))),
                               .progress = T)
gbif_all2 <- bind_rows(gbif_dwc2)
splink_dwc2 <-
  furrr::future_map(splink_dwc,
                    ~mutate(.x, across(where(is.integer), .fns = function(x) as.character(x))) %>%
                      mutate(across(where(is.double), .fns = function(x) as.character(x))) %>%
                      mutate(across(where(is.logical), .fns = function(x) as.character(x))),
                    .progress = T)
splink_all2 <- bind_rows(splink_dwc2)
dim(splink_all2)
gbif_all <- bind_rows(gbif_dwc2)

all <- bind_rows(gbif_all, splink_all)
all2 <- bind_rows(gbif_all2, splink_all2)
write_csv(all, "output/p2/occs/COORD_splink_gbif_all.csv")
a <- readr::read_csv("output/p2/occs/COORD_splink_gbif_all.csv")
read_sf2("output/p2/occs/COORD_splink_gbif_all.csv")
length(gbif_dwc2)
dim(gbif_all2)
write_csv(gbif_all2, "output/p2/ALL_GBIF_selected_species.csv")


write_csv(splink_all2, "output/p2/ALL_SPLINK_selected_species.csv")
write_csv(all2, "output/p2/ALL_selected_species.csv")
plan(sequential)
head(all2)
sel_fields$field %in% names(all2)
names(all2) %in% sel_fields$field[sel_fields$select == T]
fields <- sel_fields$field[sel_fields$select == T]
all2 %>% select(fields)
