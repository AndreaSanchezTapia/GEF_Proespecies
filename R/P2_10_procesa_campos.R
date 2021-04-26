library(readr)
library(dplyr)
source("R/functions.R")
sampa <- read_sf2("sampa.csv", column = "XY")
plot(sampa$Y, sampa$Y.1)
names(sampa)
no_col <- read_csv("output/p2/occs/ALL_NO_COLUMN.csv")
unique(no_col$country)

no_col <- no_col %>% select(one_of(names(sampa)))

no_col <- no_col %>% mutate(elevation = as.character(elevation),
                            elevationAccuracy = as.character(elevationAccuracy),
                            eventDate = as.character(eventDate),
                            dateIdentified = as.character(dateIdentified),
                            familyKey = as.character(familyKey),
                            record_id = as.character(record_id),
                            year = as.character(year),
                            month = as.character(month),
                            day = as.character(day)
) %>%
  filter(country %in% c("Brazil", "Brésil") | is.na(country)) %>%
  filter(stateProvince %in% c("São Paulo", "Sp", "Sao Paulo") | is.na(stateProvince))

sampa_w_no_col <- bind_rows(sampa, no_col)
sampa_w_no_col %>% count(t20, is.na(X))


tax <- read_csv("output/p2/10_base_P1.csv")
final_sampa <- left_join(sampa_w_no_col, tax) %>%
  select(one_of(c(ord_fields, "X", "Y", "t20")))
dim(final_sampa)
count(final_sampa, t20, country, stateProvince) %>% View()

t20


t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos <-textclean::replace_non_ascii(tolower(t20$NM_MUNICIP))

####DETECTA E FILTRA MUNICIPIOS

purrr::map(final_sampa$county, sum(str_detect(final_sampa$county, mpos[1]), na.rm = T)

vars <- c("verbatimLongitude", "verbatimLatitude", "country", "county","locality", "municipality")
vars %in% names(final_sampa)
test <- final_sampa %>% select(all_of(vars))
test <- test %>% mutate(across(any_of(vars),
                             .fns = function(x) tolower(x) %>% textclean::replace_non_ascii(.)))
whereee <- purrr::map(test, ~str_detect(string = .x, pattern = mpos)) %>% bind_rows
onde <- which(colSums(whereee, na.rm = T) > 0)
onde_linhas <- which(rowSums(whereee, na.rm = T) > 0)
final_sampa <- final_sampa %>%
  rename(decimalLongitude = X, decimalLatitude = Y) %>%
  mutate(DETECT_MUNICIPIO = if_else(rowSums(whereee, na.rm = T) > 0, TRUE, FALSE))

write_sf2(final_sampa, "sampa_final_unfiltered.csv", delete_dsn = T)

final_sampa_filtered <- final_sampa %>%
  #mutate(DETECT_MUNICIPIO = if_else(rowSums(whereee, na.rm = T) > 0, TRUE, FALSE)) %>%
  filter(t20 == 1 |  DETECT_MUNICIPIO == TRUE)
write_sf2(final_sampa_filtered, "sampa_final_filtered.csv", delete_dsn = T)



rb <- read_biota$CRbiota %>% select(one_of(names(final_sampa_filtered))) %>%
  mutate(institutionID = as.character(institutionID),
                                    catalogNumber = as.character(catalogNumber),
                                    locationID = as.character(locationID))

final <- bind_rows(final_sampa_filtered, rb) %>% left_join(tax)
  write_sf2(final, "output/p2/P2_sampa_final_filtered_cr_BIOTA.csv", delete_dsn = T)
count(final, t20, is.na(decimalLatitude), is.na(decimalLongitude))
final %>% pull(verbatimLatitude) %>% unique()
