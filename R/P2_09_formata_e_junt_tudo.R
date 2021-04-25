
t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
shape = t20
destdir = "corrigiendo"
names(df_all)
#devtools::load_all("../../R_packages/plantR")
cruza_shape <- function(nac = "Aphelandra squarrosa",
                        fonte = "gbif",
                        shape = t20,
                        destdir = "test_shape",
                        ...) {
  occs <- df_all[df_all$nome_aceito_correto == nac,fonte]
  if (!dir.exists(destdir)) dir.create(destdir, recursive = T)
  occ <- tryCatch({
    shp <- read_sf2(occs, column = "latlon") %>% distinct %>%
      mutate(nome_aceito_correto = nac,
             fonte_dados = fonte) %>% select(nome_aceito_correto)
      select(one_of(ord_fields))
  },
  error = function(e) {
    message(paste(name,"problem"))
    #message(e)
  })
  if ("sf" %in% class(occ)) {
    occ <- st_set_crs(occ, st_crs(shape))
    inter <- st_intersects(occ, shape)
    occ <- occ %>% mutate(t20 = lengths(inter) > 0)
    write_sf2(occ, dsn = fs::path(destdir, paste0(name,"_", fonte), ext = "csv"), ...)
    return(occ)
  }
}
library(furrr)
plan(multisession, workers = 15)
df_gbif <- df_all[!is.na(df_all$gbif),]
furrr::future_map(df_gbif$nome_aceito_correto,
                  ~cruza_shape(name = .x, fonte = "gbif", shape = t20, destdir = "test_gbif_2"),
                  .progress = T)
df_splink <- df_all[!is.na(df_all$splink),]
furrr::future_map(df_splink$nome_aceito_correto,
                  ~cruza_shape(name = .x, fonte = "splink", shape = t20, destdir = "test_splink"),
                  .progress = T)

a <- cruza_shape(name = "Aphelandra squarrosa",fonte = "splink", delete_dsn = T)
b <- cruza_shape(name = "Aphelandra squarrosa",fonte = "gbif", delete_dsn = T)
c <- bind_rows(a, b) %>% select(one_of(ord_fields))
gbif_list <- .Last.value
splink_list <- .Last.value

nrow_gb <- furrr::future_map(gbif_list, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()

nrow_gb <- furrr::future_map(splink_list, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()

furrr::future_map(gbif_list, ~is.null(.x), .progress = T)
null_gbif <- .Last.value
sum(simplify(null_gbif))

get_countries_filter <- furrr::future_map(gbif_list, ~if(!is.null(.x)) tibble(.x) %>% select("country") %>% distinct() %>% pull(), .progress = T)
unlist(get_countries_filter) %>% unique() %>% sort()

gbif_filter_countries <- furrr::future_map(gbif_list, ~if(!is.null(.x)) filter(.x, t20 == TRUE | (country == "Brazil" | is.na(country))) %>% distinct(), .progress = T)
nrow_gb <- furrr::future_map(gbif_filter_countries, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()
get_countries_filter <- furrr::future_map(gbif_filter_countries, ~if(!is.null(.x)) tibble(.x) %>% select("country") %>% distinct() %>% pull(), .progress = T)
get_estado <- furrr::future_map(gbif_filter_countries, ~if(!is.null(.x)) tibble(.x) %>% select("stateProvince") %>% distinct() %>% pull(), .progress = T)
unlist(get_estado) %>% unique() %>% sort()
gbif_filter_sampa <- furrr::future_map(gbif_filter_countries, ~if(!is.null(.x)) filter(.x, stateProvince %in% c("São Paulo", "Sao Paulo", "são paulo", "sp", "sao paulo", "SP", "Sp", "Sao paulo", "São paulo", "São Paulo (聖保羅州)","São Paulo State"  )| is.na(stateProvince)) %>% distinct(), .progress = T)
nrow_gb <- furrr::future_map(gbif_filter_sampa, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()#30580 -34700
sampa_gbif <- bind_rows(gbif_filter_sampa)
unique(sampa_gbif$stateProvince)
count(sampa_gbif, t20)
write_sf2(sampa_gbif, "sampa_gbif.csv", delete_dsn = T)


get_countries_filter_sl <- furrr::future_map(splink_list, ~if(!is.null(.x)) tibble(.x) %>% select("country") %>% distinct() %>% pull(), .progress = T)
unlist(get_countries_filter_sl) %>% unique() %>% sort()
splink_filter_countries <- furrr::future_map(splink_list, ~if(!is.null(.x)) filter(.x, t20 == TRUE | (country %in% c("Brazil", "BRASIL", "Brasil", "Brésil", "[Brazil]",
                                                                                                                     "BR", "brazil", "brasil", "brésil", "[brazil]", "br",
                                                                                                                     "[Bresil]","[Brésil]","Brasil&nf;Brasil",
                                                                                                                     "Brasilia",  "Brasilien", "Bresil",
                                                                                                                     "Brésil austral", "Brésil central",
                                                                                                                     "Brésil méridional")
                                                                                                      | is.na(country))) %>%
                                               distinct(), .progress = T)

nrow_gb <- furrr::future_map(splink_filter_countries, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()
get_estados_sl <- furrr::future_map(splink_filter_countries, ~if(!is.null(.x)) tibble(.x) %>% select("stateProvince") %>% distinct() %>% pull(), .progress = T)
get_county_sl <- furrr::future_map(splink_filter_countries, ~if(!is.null(.x)) tibble(.x) %>% select("county") %>% distinct() %>% pull(), .progress = T)
unlist(get_estados_sl) %>% unique() %>% sort()
unlist(get_county_sl) %>% unique() %>% sort()
splink_filter_sampa <- furrr::future_map(splink_filter_countries, ~if(!is.null(.x)) filter(.x, stateProvince %in% c("São Paulo", "Sao Paulo", "são paulo", "sp", "sao paulo", "SP", "Rio de Janeiro - Sao Paulo", "S.Paulo" , "S&#227;o Paulo" )| is.na(stateProvince)) %>% distinct(), .progress = T)
nrow_gb <- furrr::future_map(splink_filter_sampa, ~nrow(.x), .progress = T)
purrr::simplify(nrow_gb) %>% unlist() %>%  sum()
sampa_splink <- bind_rows(splink_filter_sampa)
unique(sampa_splink$stateProvince)
nrow(sampa_splink)
count(sampa_splink, t20)

write_sf2(sampa_splink, "sampa_splink.csv")

sampa <- bind_rows(sampa_gbif, sampa_splink)
write_sf2(sampa, "sampa.csv")
plan(sequential)
