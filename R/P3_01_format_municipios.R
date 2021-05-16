library(dplyr)
library(sf)
library(textclean)
source("R/functions.R")
library(stringr)

# lee la tabla atual----
p3 <- readr::read_csv("./data/dados_formatados/p3/Base de dados Territorio 20 - Produto 3.xlsx - Base de registros.csv", skip = 1, guess_max = 100000)
max(p3$lat_original, na.rm = T)
min(p3$long_original, na.rm = T)
# cuantos van a salir
count(p3, `presenca TERR. 20 - original`, `presenca TERR. 20 - revisada`)

# filtro de t20 vai ter que ser feito ao final----
#p3 <- p3 %>% filter(`presenca TERR. 20 - revisada` == 1)


#como estamos de NAs
count(p3, is.na(county), is.na(municipio))

p3 %>% filter(!is.na(municipio) & !is.na(county)) %>% filter(municipio != county) %>% head()
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
  select(municipio, county, mpo_county)
names(p3)

# crea un string comparable: mpo_check----
p3 <- clean_string(p3, mpo_county) #corrige el mpo county, no municipio
dir.create("output/p3", recursive = T)
p3 %>% select(mpo_county, mpo_check) %>% distinct() %>%
readr::write_csv("./output/p3/p3_check_mun.csv")

# corrige mala ortografía
p3 <- p3 %>% mutate(mpo_check2 = case_when(
  mpo_check == "arariquama" ~ "aracariguama",
  mpo_check == "embu" ~ "embudasartes",
  mpo_check == "ibiina" ~ "ibiuna",
  mpo_check == "idaiatuba" ~ "indaiatuba",
  mpo_check == "itapeciricadaserra" ~ "itapecericadaserra",
  mpo_check == "maripora" ~ "mairipora",
  mpo_check == "mataog" ~ "matao",
  mpo_check == "mogi" ~ "mogidascruzes",
  mpo_check == "mogicruzes" ~ "mogidascruzes",
  mpo_check == "mogidacruzes" ~ "mogidascruzes",
  mpo_check == "mogidascruzesestradausaca" ~ "mogidascruzes",
  mpo_check == "mojidascruzes" ~ "mogidascruzes",
  mpo_check == "mojiguacu" ~ "mogiguacu",
  mpo_check == "mongaga" ~ "mongagua",
  mpo_check == "monguagua" ~ "mongagua",
  mpo_check == "nubia" ~ "inubiapaulista",
  mpo_check == "inubia" ~ "inubiapaulista",
  mpo_check == "paruibe" ~ "peruibe",
  mpo_check == "peribe" ~ "peruibe",
  mpo_check == "pirssununga" ~ "pirassununga",
  mpo_check == "pmogidascruzes" ~ "mogidascruzes",
  mpo_check == "sallesopolis" ~ "salesopolis",
  mpo_check == "santaritadepassaquatro" ~ "santaritadopassaquatro",
  mpo_check == "santdaalegria" ~ "santoantoniodaalegria",
  mpo_check == "saobernadodocampo" ~ "saobernardodocampo",
  mpo_check == "saojosedobarreiros" ~ "saojosedobarreiro",
  mpo_check == "saolorencodaserra" ~ "saolourencodaserra",
  mpo_check == "saoluisdoparaitinga" ~ "saolourencodaserra",
  mpo_check == "saoluisdoparaitinga" ~ "saoluizdoparaitinga",
  mpo_check == "saoluizdaparaitinga" ~ "saoluizdoparaitinga",
  mpo_check == "sluisdoparaytinga" ~ "saoluizdoparaitinga",
  mpo_check == "saosebastiaobertioga" ~ "saosebastiao",#
  mpo_check == "setebarros" ~ "setebarras",
  mpo_check == "slocal" ~ "NA",
  mpo_check == "sp" ~ "saopaulo")) %>%
  mutate(mpo_check3 = if_else(is.na(mpo_check2), mpo_check, mpo_check2))

p3 %>% select(mpo_county, mpo_check, mpo_check2, mpo_check3) %>% distinct() %>%
  readr::write_csv("./output/p3/p3_check_mun.csv")
p3 <- p3 %>%
  select(-mpo_check, -mpo_check2) %>%
  rename(mpo_check = mpo_check3)
#read shapes
mpos_t20 <-  read_sf("data/dados_crus/Municipios_Territorio_20/Municipios_Territorio_20.shp")

mpos <- read_sf("data/dados_crus/BR_Municipios_2020/BR_Municipios_2020.shp")
mpos <- clean_string(mpos, NM_MUN)

#los nombres padrao ----
mpo_shape <-  mpos %>%
  select(mpo_check, NM_MUN) %>%
  sf::st_drop_geometry() %>% distinct()
mpo_shape
#strings comparables
mpos_t20 <- clean_string(mpos_t20, NM_MUNICIP) %>%
  left_join(mpo_shape) %>%
  rename(municipio_padronizado = NM_MUN)
#junta con los nombres padrao----
p3_corr <- left_join(p3, mpo_shape) %>%
  rename(municipio_padronizado = NM_MUN)

# preenche a partir da localidade, match exato----
#formata o stirng da localidade para achar ela
loc_detect <- p3_corr %>%
  select(ID, localidade) %>%
  clean_string(localidade)

p3_corr$municipio_localidade <-
  mpo_shape$NM_MUN[match(x = loc_detect$mpo_check, table = mpo_shape$mpo_check)]
write_csv(tibble(municipio_localidade = p3_corr$municipio_localidade), "output/p3/checks_municipio_localidade.csv")

#extrai sò municipios do t20
names(mpos_t20$mpo_check) <- mpos_t20$municipio_padronizado
test_purr <- purrr::imap(mpos_t20$mpo_check,
                        ~if_else(stringr::str_detect(loc_detect$mpo_check, .x), .y, NA_character_))
test_p <- bind_rows(test_purr)
library(tidyr)
detected_t20 <- unite(data = test_p, "detected_t20", sep = ",",na.rm = T)
write_csv(detected_t20, "output/p3/checks_municipio_detected.csv")
p3_corr$mun_detected_t20 <- detected_t20$detected_t20
p3_corr$mun_detected_t20[p3_corr$mun_detected_t20 == ""] <- NA

#crea el municipio final a partir de donde hay NAs o no.
p3_corr <- p3_corr %>%
  mutate(municipio_padronizado2 = if_else(is.na(mpo_county), municipio_localidade, municipio_padronizado))

count(p3_corr, mpo_county, municipio_localidade, municipio_padronizado, mun_detected_t20, municipio_padronizado2) %>%
  arrange(desc(n)) %>% View()
p3_corr <- p3_corr %>%
  mutate(municipio_padronizado_final =
           if_else(is.na(municipio_padronizado2) & !is.na(mun_detected_t20),
                                               mun_detected_t20,
                                               municipio_padronizado2))

count(p3_corr, municipio_padronizado2, mun_detected_t20, municipio_padronizado_final) %>% View()
count(p3_corr, mpo_county, municipio_padronizado2) %>% arrange(desc(n)) %>% View()
count(p3_corr, is.na(municipio_padronizado2))
write_csv(p3_corr, "output/p3/p3_municipio_localidade_match.csv")

p3_corr %>%
  #filter(`presenca TERR. 20 - revisada` == 1) %>%
  count(is.na(municipio) &
        is.na(mpo_county)&
        is.na(municipio_localidade)&
        is.na(municipio_padronizado)&
        is.na(municipio_padronizado2),
        is.na(mun_detected_t20),
        is.na(municipio_padronizado_final)
        ) %>% View()
p3_corr %>% filter(!is.na(mpo_county)) %>% select(mpo_county, mpo_check, municipio_padronizado) %>% distinct() %>% View()

#listo detectar parcialmente t20
