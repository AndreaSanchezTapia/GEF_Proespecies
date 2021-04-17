library(readr)
library(knitr)
library(Rocc)

p2 <- read_csv("output/p2/02_resumo.csv")
checa as categorias atuais
 nop <- p2 %>%
   filter(cat_ameaca_geral == "nao entra" &
            elegivel_produto_1 != "inapta")
 nop %>% count(cat_ameaca_iucn)

# #corrige cat_ameaca_br
 nop %>% count(cat_ameaca_br)#2
 p2$cat_ameaca_geral[which(p2$elegivel_produto_1 != "inapta" &
                             p2$cat_ameaca_br  == "EN" &
                             p2$cat_ameaca_geral == "nao entra")] <- "entra"
#
# nop %>% count(cat_ameaca_cncflora)
# nop %>% count(cat_ameaca_sp)
# nop %>% count(cat_ameaca_mpo_sp)
# nop %>% count(cat_ameaca_cr_lac)
#
#write_csv(p2, "output/p2/03_resumo_anotado.csv")

p2 <- read_csv("output/p2/03_resumo_anotado.csv")

especies <- p2 %>%
  filter(elegivel_produto_1 != "inapta",
         cat_ameaca_geral == "entra") %>%
  pull(nome_aceito_correto)

#tira as que vamos ignorar
ignorar <- str_detect(especies, "subsp.") |
  str_detect(especies, "var.")
bajar <- especies[ignorar]
especies <- especies[!ignorar]

#tira as que nao tem em SP
no_SP <- p2 %>% filter(elegivel_produto_1 == "examinar" &
         splink_sp == FALSE &
           gbif_sp == FALSE)%>%
  pull(nome_aceito_correto)
especies <- especies[!especies %in% no_SP]

p2 %>% filter(nome_aceito_correto %in% especies) %>% count(splink_sp | gbif_sp, elegivel_produto_1, cat_ameaca_geral)
p2_selecionadas <- p2 %>%
  filter(nome_aceito_correto %in% especies)
write_csv(p2_selecionadas, "output/p2/p2_selecionadas.csv")

#### hasta aqui seleccion


#lee las tablas de ocurrencias
occs <- list.files("output/p2/occs",recursive = T, full.names = T, pattern = ".csv")
#checar arquivos vazios
#empty <- file.info(occs)[["size"]] == 0
#which(empty)
#occs[which(empty)]
#unlink(empty)
oc_sp <- basename(occs) %>% stringr::str_remove(".csv")
selecionadas <- occs[oc_sp %in% especies]

gbif <- selecionadas[stringr::str_detect(selecionadas, "splink", negate = T)]
splink <- selecionadas[stringr::str_detect(selecionadas, "gbif", negate = T)]


nomes_splink <- basename(splink) %>% stringr::str_remove(".csv")
nomes_gbif   <- basename(gbif)   %>% stringr::str_remove(".csv")
names(splink) <- nomes_splink
names(gbif) <- nomes_gbif
occs <- tibble(sp = especies) %>%
  full_join(tibble(sp = nomes_gbif, gbif_file = gbif)) %>%
  full_join(tibble(sp = nomes_splink, splink_file = splink), by = "sp") %>%
  arrange(sp)
occs
#
library("remotes")
install_github("LimaRAF/plantR")
library(plantR)
library
occs %>% View()
   pasta_all <- fs::path("output/p2/all")
   dir.create(pasta_all)

for (i in 1:10) {
  spp <- occs$splink_file[i]
  if (!is.na(spp)) {
    spl <- read.csv(spp) %>%
      mutate(across(is_integer, .fns = function(x) as.character(x)))%>%
      mutate(across(is_double, .fns = function(x) as.character(x)))

  } else spl <- NULL
  gbb <- occs$gbif_file[i]
  if (!is.na(gbb)) {
    gbf <- read.csv(gbb) %>%
      mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
      mutate(across(is_double, .fns = function(x) as.character(x)))
  } else gbf <- NULL
   all <- formatDwc(splink_data = spl,
                 gbif_data = gbf,
                 drop.empty = T)
   sp <- occs$sp[i]
   file <- fs::path(pasta_all, sp, ext = "csv")
   if (!file.exists(file)) {
     write_csv(all, file = file)
     }
   rm(spl)
   rm(gbf)
   }

names(occs)
juntar_tudo <- function(.x) {
  spp <- ..3
  if (!is.na(spp)) {
       spl <- read.csv(spp) %>%
         mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
         mutate(across(is_double, .fns = function(x) as.character(x)))

     } else spl <- NULL
  gbb <- ..2
     if (!is.na(gbb)) {
       gbf <- read.csv(gbb) %>%
         mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
         mutate(across(is_double, .fns = function(x) as.character(x)))
     } else gbf <- NULL
     all <- formatDwc(splink_data = spl,
                      gbif_data = gbf,
                      drop.empty = T)
     sp <- ..1
     file <- fs::path(pasta_all, sp, ext = "csv")
     if (!file.exists(file)) {
       write_csv(all, file = file)
     }
     rm(spl)
     rm(gbf)
   }
occs[11,]
purrr::pmap(occs[14,], ~{
  sp <- ..1
  gbb <- ..2
  spp <- ..3

  if (!is.na(spp)) {
    spl <- read.csv(spp) %>%
      mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
      mutate(across(is_double, .fns = function(x) as.character(x)))

  } else spl <- NULL
  if (!is.na(gbb)) {
    gbf <- read.csv(gbb) %>%
      mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
      mutate(across(is_double, .fns = function(x) as.character(x)))
  } else gbf <- NULL
  tryCatch({all <- formatDwc(splink_data = spl,
                   gbif_data = gbf,
                   drop.empty = T)
    file <- fs::path(pasta_all, sp, ext = "csv")
    if (!file.exists(file)) {
      write_csv(all, file = file)
    }
  })
  })
