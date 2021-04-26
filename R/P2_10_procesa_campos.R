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



tax <- read_csv("output/p2/10_base_P1.csv")
final_sampa <- left_join(sampa_w_no_col, tax) %>% select(one_of(c(ord_fields, "X", "Y", "t20")))
dim(final_sampa)
count(final_sampa, t20, country, stateProvince) %>% View()

t20


t20  <- read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
mpos <-textclean::replace_non_ascii(tolower(t20$NM_MUNICIP))

purrr::map(final_sampa$county, sum(str_detect(final_sampa$county, mpos[1]), na.rm = T)
