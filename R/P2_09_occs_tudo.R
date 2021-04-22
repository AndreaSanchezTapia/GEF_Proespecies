library(readr)
library(dplyr)
library(stringr)

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

plan(sequential)
#lee las tablas de ocurrencias
p3_selecionadas <- read_csv("output/p2/p3_territorio20.csv")
especies <- p3_selecionadas %>% select(nome_aceito_correto)
df_all <- left_join(especies, gbifs) %>% left_join(splinks) %>% left_join(cruza_gbif) %>% left_join(cruza_splinks)

library(furrr)
plan(multisession, workers = 15)
#lee todo
plan(sequential)

read_gbif <- furrr::future_map(na.omit(df_all$gbif),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
read_splink <- furrr::future_map(na.omit(df_all$splink),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
read_cruza_gbif <- furrr::future_map(na.omit(df_all$gbif_cruza),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
read_cruza_splink <- furrr::future_map(na.omit(df_all$splink_cruza),
                              ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                              .progress = T)
which(is.na(df_all$gbif))
which(is.na(df_all$gbif_cruza))

read(sequential)
#nrow_g <- furrr::future_map(read_gbif, ~nrow(.x), .progress = T)
#nrow_sp <- furrr::future_map(read_splink, ~nrow(.x), .progress = T)
#purrr::simplify(nrow_g) %>% sum()
#purrr::simplify(nrow_sp) %>% sum()

cruza_gbif <- furrr::future_map(na.omit(df_all$gbif),
                                ~vroom::vroom(.x, guess_max = 100000) %>% distinct(),
                                .progress = T)
names_raw <- furrr::future_map(read_raw2, ~names(.x), .progress = T)
todos_os_nomes <- simplify(names_raw) %>% unique() %>% sort()

# processamento a mao apra ver quais campos ficam em P2_10
sel_fields <- read_csv("output/p2/08_campos_originales.csv")
sel_fields <- sel_fields %>% filter(select == T) %>% pull(field)
plan(multisession, workers = 15)
length(sel_fields)
fields_raw <- furrr::future_map(read_raw2, ~select(.x, one_of(sel_fields)), .progress = T)
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


gbif <- occs[stringr::str_detect(occs, "splink", negate = T)]
nomes_gbif   <- basename(gbif)   %>% stringr::str_remove("_gbif_c.csv")
splink <- occs[stringr::str_detect(occs, "gbif", negate = T)]
nomes_splink <- basename(splink) %>% stringr::str_remove("_splink.csv")

names(gbif) <- nomes_gbif
names(splink) <- nomes_splink

oc_df <- tibble(sp = especies) %>%
  full_join(tibble(sp = nomes_gbif, gbif_file = gbif)) %>%
  full_join(tibble(sp = nomes_splink, splink_file = splink), by = "sp") %>%
  arrange(sp)
oc_df
head(oc_df)
#library("remotes")
#install_github("LimaRAF/plantR")
#library(plantR)
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
    mutate(across(where(is.double), .fns = function(x) as.character(x)))

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
