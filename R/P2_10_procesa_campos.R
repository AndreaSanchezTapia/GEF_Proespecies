library(readr)
library(dplyr)
source("R/functions.R")
sampa <- read_sf2("sampa.csv", column = "XY")
plot(sampa$Y, sampa$Y.1)
dim(sampa)
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
nrow(sampa_w_no_col)
sampa_w_no_col %>% count(t20, is.na(X))

is(read_biota$CRbiota)
names(read_biota$CRbiota)
read_biota$CRbiota$scientificName

write_csv(read_biota$CRbiota, "output/p2/p2_CR_Biota.csv")
biota <- read_sf2("output/p2/p2_CR_Biota.csv", column = "latlon")
rb <- biota %>%
  mutate(nome_aceito_correto = scientificName) %>%
  select(any_of(names(sampa_w_no_col)), "decimalLatitude", "decimalLongitude", "geometry") %>%
  mutate(institutionID = as.character(institutionID),
         catalogNumber = as.character(catalogNumber),
         locationID = as.character(locationID),
         fonte_dados = "SisBIOTA" )

tax <- read_csv("output/p2/10_base_P1.csv")
final <- bind_rows(sampa_w_no_col, rb) %>% left_join(tax)
sort(names(final))
sort(ord_fields)
final <- final %>% select(one_of(c(ord_fields, "X", "Y", "t20", "decimalLatitude", "decimalLongitude")))
write_sf2(final, "output/p2/P2_sampa_final_unfiltered_cr_BIOTA_forp3.csv", delete_dsn = T)
count(final, fonte_dados)
filter(final, fonte_dados == "SisBIOTA") %>%
write_sf2("output/p2/P2_biota_data.csv", delete_dsn = T)
read_sf2("output/p2/P2_biota_data.csv")
dim(final)
###MUNICIPIOS
get_county_sl <- unique(final$county)


####DETECTA E FILTRA MUNICIPIOS
t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos <- textclean::replace_non_ascii(tolower(t20$NM_MUNICIP))
t20
munt20 <- tibble(mun= sort(mpos), t20 = TRUE)
vars <- c("stateProvince", "county", "municipality", "locality")
vars %in% names(final)
test <- final %>% mutate(across(any_of(vars),
                             .fns = function(x) tolower(x) %>% textclean::replace_non_ascii(.), .names = "{.col}_format"))
get_county <- unique(test$county_format) %>% sort()
get_mplit <- unique(test$municipality_format) %>% sort()
get_loc <- unique(test$locality_format) %>% sort()

#cria a tabela inteira para ver o que tem nos campos county e municipality
mpos_etiqueta <- c(get_county, get_mplit) %>% unique()
sum(mpos_etiqueta %in% mpos)
mun_eti <- tibble(mun = mpos_etiqueta, etiqueta = T)
all_mpo <- full_join(munt20, mun_eti)
all_mpo <- all_mpo %>% arrange(mun)
write_csv(all_mpo, "output/p2/17_municipios_p_filtrar.csv")
#editado a mano por los errores de ortografia / quitando otros municipios que claramente no entram
mpos_tabla <- read_csv("output/p2/17_municipios_p_fitrar_edited.csv")

filt <- mpos_tabla %>% rename(county_format = `mun+county`,
                              select_county = select,
                              in_t20_county = t20)
filt2 <- mpos_tabla %>% rename(municipality_format = `mun+county`,
                               select_municipality = select,
                               in_t20_mpo = t20)
tst <- test %>% left_join(filt) %>% left_join(filt2)
tst %>% View()
count(tst, t20, select_county, select_municipality)
count(tst, t20, select_county)

tst %>% filter(select_county != FALSE | select_municipality != FALSE) %>% count(select_county, select_municipality)

tst2 <- tst %>% mutate(final_selection = case_when(
  t20 == 0 & select_county == FALSE ~ "sai",
  t20 == 0 & select_municipality == FALSE ~ "sai",
  t20 == 1 ~ "entra",
  t20 == 0 & select_county == TRUE ~ "entra",
  t20 == 0 & select_municipality == TRUE ~ "entra",
  is.na(t20) & select_county == TRUE ~ "entra",
  is.na(t20) & select_municipality == FALSE ~ "sai",
  is.na(t20) & select_municipality == TRUE ~ "entra",
  is.na(t20) & is.na(select_county) ~ "check_locality",
  is.na(select_county) & is.na(select_municipality) ~ "check_locality"
  ))

count(tst2, t20, select_county, select_municipality, final_selection)
names(tst2)
ord_fields
final_sampa <- tst2 %>%
  #rename(decimalLatitude = Y, decimalLongitude = X) %>%
  mutate(situacao_coordenada = if_else(is.na(decimalLatitude)|is.na(decimalLongitude),"CA" ,"CO")) %>%
  mutate(ID = 1:nrow(tst2)) %>%
  select(-starts_with("in_t20"), -ends_with("format"), -etiqueta, -starts_with("acceptedNameUsage"), -institutionKey,
         -collectionKey, -elevationAccuracy, -countryCode, -familyKey) %>%
  select(ID, grupo, familia, genero, epiteto_especifico, nome_aceito_correto, nivel_taxonomico, epiteto_infraespecifico, #sinonimia, fonte_sinonimia,
         institutionCode, collectionCode, catalogNumber, barcode, recordedBy, record_id, recordNumber, fieldNumber, year, month, day, stateProvince, county, municipality, locality, decimalLongitude, decimalLatitude, verbatimLongitude, , verbatimLatitude, verbatimLocality, geodeticDatum, situacao_coordenada, elevation, identifiedBy, identifier, identifiers, dateIdentified, yearIdentified, monthIdentified, dayIdentified,  ends_with("Remarks"), habitat, fonte_dados, t20, select_county, select_municipality, final_selection)

write_sf2(final_sampa, "output/p2/P2_sampa_final_unfiltered.csv", delete_dsn = T)
sampa_final <- read_sf2("output/p2/P2_sampa_final_unfiltered.csv")
final_sampa_filtered <- final_sampa %>%
  filter(final_selection != "sai")
write_sf2(final_sampa_filtered, "output/p2/P2_sampa_final_filtered.csv", delete_dsn = T)
count(final_sampa_filtered, fonte_dados)


