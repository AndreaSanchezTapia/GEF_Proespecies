#rsthemes::use_theme_auto(lat = 37, lon = -120)
library(finch)
library(dplyr)
library(readr)
library(stringr)
biota <- finch::dwca_read("https://ipt.biota.org.br/ipt/archive.do?r=biota_occurrences&v=1.3")
biota$data
biota <- vroom::vroom(biota$data, guess_max = 132000)
names(biota)
biota %>% count(scientificName, genus, specificEpithet) %>% distinct() %>% View()

count(biota, infraspecificEpithet)


cats <- readr::read_csv("./output/p2/04_cat_ameaca.csv")
cat_geral <- readr::read_csv("./output/p2/03_entra.csv")
cat <- left_join(cats, cat_geral)

CR <- cat %>% mutate(across(-1, ~stringr::str_detect(.x,
  pattern = "CR"), .names = "{.col}_CR")) %>%
  select(ends_with("CR")) %>% rowSums(na.rm = T)
CR <- tibble(nome_aceito_correto = cat$nome_aceito_correto, CR) %>% filter(CR > 0)
write_csv(CR, "output/p2/13_especies_CR.csv")


biota$scientificName[which(biota$scientificName %in% CR$nome_aceito_correto)]
CR$nome_aceito_correto[which(CR$nome_aceito_correto %in% biota$scientificName) ]

biota_CR <- biota %>% filter(scientificName %in%
                               CR$nome_aceito_correto)
dir.create("output/p2/occs/biota/")
write_csv(biota_CR, "output/p2/occs/biota/biotaCR.csv")

tibble(campos_biota = names(biota_CR)[who]) %>% write_csv("output/p2/14_campos_biota.csv")
who <- apply(biota_CR, MARGIN = 2, FUN = function(x)all(!is.na(x)))
