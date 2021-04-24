library(readr)
library(dplyr)
library(stringr)
source("./R/file_data_frame.R")

#cria os dataframes dos quatro grupos de dados----
biota <- file_data_frame("output/p2/occs/biota") %>%
  rename(biota = paths, nome_aceito_correto = names)
cruza_biota <- file_data_frame("output/p2/cruza_shape/t20/biota/") %>%
  rename(biota_cruza = paths, nome_aceito_correto = names)
splinks <- file_data_frame("output/p2/occs/splink") %>%
  rename(splink = paths, nome_aceito_correto = names)
cruza_splinks <- file_data_frame("output/p2/cruza_shape/t20/splink/") %>%
  rename(splink_cruza = paths, nome_aceito_correto = names)
gbifs <- file_data_frame("output/p2/occs/clean") %>%
  mutate(names = stringr::str_remove(names, "_SYNONYM")) %>%
  mutate(names = stringr::str_remove(names, "_NAO_LIMPOU_CHECK_NOME")) %>%
  rename(gbif = paths, nome_aceito_correto = names)
cruza_gbif <- file_data_frame("output/p2/cruza_shape/t20/gbif/") %>%
  rename(gbif_cruza = paths, nome_aceito_correto = names)
cruza_gbif$nome_aceito_correto <- str_remove(cruza_gbif$nome_aceito_correto, "_occs")
cruza_splinks$nome_aceito_correto <- str_remove(cruza_splinks$nome_aceito_correto, "_occs")

#so as selecionadas----
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas %>% select(nome_aceito_correto)

#omits biota here----
df_all <- left_join(especies, gbifs) %>% left_join(splinks) %>% left_join(cruza_gbif) %>% left_join(cruza_splinks)
# PARA FORMATEAR!!!! -----
cruza_all <- left_join(especies, cruza_gbif) %>% left_join(cruza_splinks)
read_cruza_gbif <- furrr::future_map(na.omit(df_all$gbif_cruza),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_cruza_gbif) <- df_all$nome_aceito_correto[-which(is.na(df_all$gbif_cruza))]
read_cruza_splink <- furrr::future_map(na.omit(df_all$splink_cruza),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_cruza_splink) <- df_all$nome_aceito_correto[-which(is.na(df_all$splink_cruza))]

read_biota <- furrr::future_map(biota$biota,
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_biota) <- "CRbiota"

nrow_gb <- furrr::future_map(read_cruza_gbif, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% sum()
nrow_spl <- furrr::future_map(read_cruza_splink, ~nrow(.x), .progress = T)
purrr::simplify(nrow_spl) %>% sum()

#separa los que no tienen coordenadas-----
library(furrr)
plan(multisession, workers = 15)

#lee todo
read_gbif <- furrr::future_map(na.omit(df_all$gbif),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_gbif) <- df_all$nome_aceito_correto[-which(is.na(df_all$gbif))]
read_splink <- furrr::future_map(na.omit(df_all$splink),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
names(read_splink) <- df_all$nome_aceito_correto[-which(is.na(df_all$splink))]

#numero de registros
nrow_g <- furrr::future_map(read_gbif, ~nrow(.x), .progress = T)
purrr::simplify(nrow_g) %>% sum()
nrow_sp <- furrr::future_map(read_splink, ~nrow(.x), .progress = T)
purrr::simplify(nrow_sp) %>% sum()

#no coords----
out_dir_sp <- "output/p2/occs/no_coord/splink"
out_dir_gb <- "output/p2/occs/no_coord/gbig"
dir.create(out_dir, recursive = T)

#names gbif e specieslink
long <- furrr::future_map(read_gbif, ~if_else("decimalLongitude" %in% names(.x), TRUE, FALSE), .progress = T)
lat <- furrr::future_map(read_gbif, ~if_else("decimalLatitude" %in% names(.x), TRUE, FALSE), .progress = T)
no_cols <- names(long[long == F])
#lat[lat ==F]
gbif_no_col <- df_all[df_all$nome_aceito_correto %in% no_cols,]
gbif_col <- df_all[!df_all$nome_aceito_correto %in% no_cols,]

long2 <- furrr::future_map(read_splink, ~if_else("decimalLongitude" %in% names(.x), TRUE, FALSE), .progress = T)
lat2 <- furrr::future_map(read_splink, ~if_else("decimalLatitude" %in% names(.x), TRUE, FALSE), .progress = T)
no_cols2 <- names(long2[long2 == F])
splink_no_col <- df_all[df_all$nome_aceito_correto %in% no_cols2,]
splink_col <- df_all[!df_all$nome_aceito_correto %in% no_cols2,]

#agora extrai as que nao tem coordenadas daquelas tabela grandes -----
plan(multisession, workers = 15)
any(is.na(gbif_col$nome_aceito_correto))
any(gbif_col$nome_aceito_correto =="NA")
any(is.na(gbif_col$gbif))
read_gbif_tirar <- furrr::future_map(na.omit(gbif_col$gbif),
                               ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                               .progress = T)
names(read_gbif_tirar) <- gbif_col$nome_aceito_correto[which(!is.na(gbif_col$gbif))]
read_splink_tirar <- furrr::future_map(na.omit(splink_col$splink),
                                 ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                                 .progress = T)
names(read_splink_tirar) <- splink_col$nome_aceito_correto[which(!is.na(splink_col$splink))]

no_coord_gbif <- furrr::future_map(read_gbif_tirar,
                                   ~filter(.x, (is.na(decimalLongitude) | is.na(decimalLatitude))),
                                   .progress = T)
no_coord_gbif[[1]]

read_gbif[[1]]
read_cruza_gbif[[1]]
no_coord_splink <- furrr::future_map(read_splink_tirar,
                                     ~filter(.x, (is.na(decimalLongitude) | is.na(decimalLatitude))),
                                     .progress = T)
#salva as que nao tem coordenadas
furrr::future_imap(no_coord_splink,
                  ~readr::write_csv(.x, file = paste0("output/p2/occs/no_coord/splink/",.y, ".csv")),
                  .progress = T)
plantr_SP <- function(x) {
  data.frame(x) %>% plantR::fixLoc(loc.levels = c("country", "stateProvince")) %>%
  filter(country.new == "brazil" | is.na(country.new)) %>%
  filter(stateProvince.new == "sao paulo" | is.na(stateProvince.new))
}

no_coord_SP <- furrr::future_map(no_coord_gbif,
                  ~plantr_SP(.x),
                  .progress = T)
names(no_coord_SP)
furrr::future_imap(no_coord_SP,
                   ~readr::write_csv(.x, file = paste0("output/p2/occs/no_coord/gbif/",.y, ".csv")),
                   .progress = T)
no_coord_SP_sl <- furrr::future_map(no_coord_splink,
                  ~plantr_SP(.x),
                  .progress = T)
names(no_coord_SP)
furrr::future_imap(no_coord_SP_sl,
                   ~readr::write_csv(.x, file = paste0("output/p2/occs/no_coord/splink/",.y, ".csv")),
                   .progress = T)

t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos <-textclean::replace_non_ascii(tolower(t20$NM_MUNICIP))
  a <- plantR::fixLoc(data.frame(geo_splink[[1]]), loc.levels = c("country", "stateProvince", "municipality"))
filter(a, country.new == "brazil" | is.na(country.new)) %>% filter(stateProvince.new == "sao paulo" | is.na(stateProvince.new)) %>% filter(municipality.new %in% mpos)

furrr::future_imap(no_coord_gbif,
                  ~readr::write_csv(.x, file = paste0("output/p2/occs/no_coord/gbif/",.y, ".csv")),
                  .progress = T)

#salvar las que no cruzan


#formatea cruza s[o]----
#pega todos os nomes para checar se tudo bem
all <- c(read_gbif, read_cruza_gbif, read_splink, read_cruza_splink, read_biota)
especies_all <- names(all)
names_raw <- furrr::future_map(all, ~names(.x), .progress = T)
todos_os_nomes <- purrr::simplify(names_raw) %>% unique() %>% sort()

# processamento a mao apra ver quais campos ficam em P2_10
sel_fields <- read_csv("output/p2/08_campos_originales.csv")
sel_fields <- sel_fields %>% filter(select == T) %>% pull(field)
sel_fields %in% todos_os_nomes
#sorts fields
campos_ordenados <- read_csv("output/p2/11_camposP2.csv") %>% pull(1)
plan(sequential)

length(sel_fields)
campos_ordenados[which(!campos_ordenados %in%  sel_fields)]
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
plan(multisession, workers = 15)
add_tax <-  furrr::future_map(add_nome,
                              ~left_join(.x, tax),
                              .progress = T)
c(names(spp), names(tax), sel_fields) %>% tibble(campos= .) %>% write_csv("output/p2/11_camposP2.csv")
#reorganiza a mano
dir.create("output/p2/t20_format/")
add_tax <-  furrr::future_map(add_tax,
                              ~select(one_of()),
                              .progress = T)

furrr::future_map2(add_tax,
                   paste0("output/p2/t20_format/", names(names_now), ".csv"),
                   ~write_csv(x = .x, file = .y),
                   .progress = T)
zip("output/p2/t20_format/" ,zipfile = "Format_preliminar.zip")

names_now <- furrr::future_map(add_tax,
                               ~select(one_of(grupo, familia, nome_aceito_correto,
                                              genero, epiteto_especifico,
                                              collectionCode,
                                              collection_ID,
                                              barcode,
                                              .progress = T)

furrr::future_map2(add_tax,
                   paste0("output/p2/t20_format/", names(names_now), ".csv"),
                   ~write_csv(x = .x, file = .y),
                   .progress = T)

#library("remotes")
devtools::load_all("../../R_packages/plantR/")

pasta_all <- fs::path("output/p2/t20_join")
dir.create(pasta_all)
#then with furrr
library(furrr)

plan(multisession, workers = 15)
plan(sequential)
library(dplyr)
sp <- oc_df[1,1]
gbb <- oc_df[1,2]
spp <- oc_df[1,3]

#furrr::future_pmap(oc_df, ~{
sp <- ..1
gbb <- ..2
spp <- ..3
file <- fs::path(pasta_all, sp, ext = "csv")

if (!is.na(spp)) {
  spl <- vroom::vroom(spp, guess_max = 100000) %>%
    mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.double), .fns = function(x) as.numeric(x)))

} else spl <- NULL
if (!is.na(gbb)) {
  gbf <- vroom::vroom(gbb, guess_max = 100000) %>%
    mutate(across(where(is.integer), .fns = function(x) as.character(x))) %>%
    mutate(across(where(is.double), .fns = function(x) as.character(x)))
} else gbf <- NULL
names(gbf) %in% names(spl)
names(spl) %in% names(gbf)

#  if (!file.exists(file))
tryCatch({all <- formatDwc(splink_data = spl,
                           gbif_data = gbf,
                           drop.empty = T)
#      write_csv(all, file = file)
#  },
#  error = function(e) {
#    message(paste(sp, "problem"))
#    message(e)
#  })

}, .progress = TRUE)

plan(sequential)


raw <- list.files(pasta_all) %>%
  basename() %>%
  str_remove(".csv") %>%
  str_remove("_gbif_c") %>%
  str_remove("_splink") %>% unique()
setdiff(especies, raw)
zipfile <- 't20_raw.zip'
zip(zipfile, 'output/p2/t20_raw/')

lista_completa <- furrr::future_map2(spp_files, spp, ~load_and_rename(.x, .y))
lista_completa[1]
names(lista_completa) <- spp


#extrai a tabela de sinonimos, pode ser NULL
syn_todas <- furrr::future_map(lista_completa,
                               ~.x$synonyms,
                               .progress = T)
