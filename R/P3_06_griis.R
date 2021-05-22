library(finch)
griis <- finch::dwca_read("https://cloud.gbif.org/griis/archive.do?r=brazil-griis-gbif&v=1.5")
taxon <- vroom::vroom(griis$data[1], guess_max = 1000000)


dist <- vroom::vroom(griis$data[2], guess_max = 1000000)
sp_profile <- vroom::vroom(griis$data[3], guess_max = 1000000)

filt <- read_csv("output/p3/p3_ready_to_filter.csv")
noauth <- purrr::map(taxon$scientificName,~flora::remove.authors(.x))
noauth <- unlist(noauth)
inv <- which(unique(filt$especie) %in% noauth)
invsp <- unique(filt$especie)[inv]
filt %>% filter(especie %in% invsp) %>% count(presenca_terr_20_revisada)

horus <- readxl::read_xlsx("data/dados_formatados/p3/Lista de espécies exóticas invasoras - Horus.xlsx", col_names = F )
horus$...1
filt %>% filter(especie %in% horus$...1) %>% distinct(especie)
