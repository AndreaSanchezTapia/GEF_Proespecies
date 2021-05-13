library(dplyr)
library(sf)
library(textclean)
source("R/functions.R")
library(stringr)
devtools::load_all("../../3_coleguinhas/plantR/plantR/")
#lee la tabla atual
p3 <- readr::read_csv("./data/dados_formatados/p3/Base de dados Territorio 20 - Produto 3.xlsx - Base de registros.csv", skip = 1, guess_max = 100000)
names(p3)
count(p3, `presenca TERR. 20 - original`, `presenca TERR. 20 - revisada`)
#p3 <- p3 %>% filter(`presenca TERR. 20 - revisada` == 1)


#como estamos de NAs
count(p3, is.na(county), is.na(municipio))
p3 %>% filter(is.na(municipio) & !is.na(county)) %>% View()
p3 %>% filter(!is.na(municipio) & !is.na(county)) %>% filter(municipio != county) %>% View()
count(p3, county, municipio)
p3 %>% filter(!is.na(municipio) & is.na(county)) %>% View()

#crea municipio from county and mpo
p3 <- p3 %>% mutate(mpo_county = case_when(
  is.na(municipio) & !is.na(county) ~ county,
  !is.na(municipio) & is.na(county) ~ municipio,
  !is.na(municipio) & !is.na(county) & municipio == county ~ municipio,
  !is.na(municipio) & !is.na(county) & municipio != county & county == "Ilha do Cardoso" ~ "Cananeia",
  !is.na(municipio) & !is.na(county) & municipio != county & county == "Sorocaba" ~ "Sorocaba",
  !is.na(municipio) & !is.na(county) & municipio != county & county == "Guarulhos" ~ "Guarulhos",
  !is.na(municipio) & !is.na(county) & municipio != county & county == "Subaúma" ~ "Iguape",
  !is.na(municipio) & !is.na(county) & municipio != county & !county %in% c("Subaúma",
                                                                            "Guarulhos",
                                                                            "Ilha do Cardoso") ~ municipio)

)
#checa que tenha ficado bom
p3 %>% filter(!is.na(municipio) & !is.na(county)) %>% filter(municipio != county) %>%
  select(municipio, county, mpo_county) %>% View()
names(p3)

#deja un string comparable: mpo_check
p3 <- clean_string(p3, mpo_county) #corrige el mpo county, no municipio
dir.create("output/p3", recursive = T)
readr::write_csv(p3, "./output/p3/p3_check_mun.csv")

# corrige mala ortografía
unique(p3$mpo_county)

#read shapes
mpos_t20 <-  read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")
#strings comparables
mpos_t20 <- clean_string(mpos_t20, NM_MUNICIP) %>%
  mutate(municipio = str_to_sentence(NM_MUNICIP))
mpos <- read_sf("data/dados_crus/BR_Municipios_2020/BR_Municipios_2020.shp")
mpos <- clean_string(mpos, NM_MUN)


#read_sf2(p3, "./output/p3/p3_check_mun.csv")
#lee los municipios
mun_table <- read_csv("output/p2/17_municipios_p_filtrar.csv")


#los nombres padrao
mpo_shape <-  mpos %>%
  select(mpo_check, NM_MUN) %>%
  sf::st_drop_geometry() %>% distinct()
mpo_shape
#junta con los nombres padrao
p3_corr <- left_join(p3, mpo_shape) %>%
  rename(municipio_padronizado = NM_MUN)

#neceistamos corregir anyway

#preenche a partir da localidade, match exato
#formata o stirng da localidade para achar ela
loc_detect <- p3_corr %>%
  select(ID, localidade) %>%
  clean_string(localidade)

p3_corr$municipio_localidade <-
  mpo_shape$NM_MUN[match(x = loc_detect$mpo_check, table = mpo_shape$mpo_check)]


#un match parcial con municipio?
#mpo_detect <- p3_corr %>%
 # select(ID, mpo_county) %>%
  #clean_string(mpo_county)
#p3_corr$mpo_county2 <- mpo_shape$NM_MUN[match(x = mpo_detect$mpo_check, table = mpo_shape$mpo_check)]
#p3_corr$mpo_county3 <- mpo_shape$NM_MUN[pmatch(x = mpo_detect$mpo_check, table = mpo_shape$mpo_check)]#el partial match solo resuelve inubia paulista y un pinheirinho
#mpo_detect$mpo_county[which(stringr::str_detect(pattern = mpo_shape$mpo_check[3315], string = mpo_detect$mpo_check))]
#test <- purrr::map(mpo_shape$mpo_check, ~which(stringr::str_detect(string = .x, pattern = mpo_detect$mpo_check)))
#purrr::map(mpo_shape$mpo_check, test, ~.x[.y])

p3_corr <- p3_corr %>%
  mutate(municipio_padronizado2 = if_else(is.na(mpo_county), municipio_localidade, municipio_padronizado))
count(p3_corr, mpo_county, municipio_localidade, municipio_padronizado, municipio_padronizado2) %>% arrange(desc(n)) %>% View()

write_csv(p3_corr, "output/p3/p3_municipio_localidade_match.csv")
names(p3_corr)
p3_corr %>%
  filter(`presenca TERR. 20 - revisada` == 1) %>%
  count(is.na(mpo_county), is.na(municipio), is.na(municipio_padronizado), is.na(municipio_localidade))

#y toca detectar parcialmente
#toca igual corregir ortografia

# falta cruzar con el shape del municipio de pais
# falta substituir por munucipios validos
Mongaguá
Monguaguá
Mongaguá #
p/ Mogi das Cruzes
Períbe
Sallesópolis
São Bernado do Campo
São Lorenço da Serra
Sete Barros
sp

mpos
min(p3_corr$long_original, na.rm = T)
min(p3_corr$lat_original, na.rm = T)
p3_corr[which.min(p3_corr$lat_original),]

max(p3_corr$lat_original, na.rm = T)

p3_corr[which.max(p3_corr$lat_original),]
read_sf2("output/p3/p3_municipio_localidade_match.csv", column = "ellos")


