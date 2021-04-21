# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
library(furrr)
library(purrr)

#sinonimos
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
names(df_sinonimos) <- nomes
syn_df <- tibble(nome_aceito_correto = nomes,
       sinonimo_file = df_sinonimos)

#para cada especie que entra pelo criterio ameaca
entra <- read_csv("output/p2/p2_inicial.csv") %>%
  select(nome_aceito_correto, cat_ameaca_geral)
syn_df <- left_join(entra, syn_df)

pasta_occs <- "output/p2/occs/"

plan(multisession, workers = 15)
df <- furrr::future_map(syn_df$sinonimo_file, ~read_csv(.x), .progress = T)
#plan(sequential)
pasta_out <- paste0(pasta_occs, "gbif")
dir.create(pasta_out)
df

#cria funcao para fazer download de gbif
download_rgbif <- function(oc) {
  sp <- unique(oc$especie)
  search <- oc$nomes#[stringr::str_detect(string = df[[i]]$nomes, "var\\.", negate = T)]
  if (!file.exists(fs::path(pasta_out, sp, ext = "csv"))) {
    key <- purrr::map(search, ~rgbif::name_backbone(name = .x))
    key2 <- purrr::map(key,
                     ~select(.x, any_of(c("acceptedUsageKey", "usageKey"))))
    key3 <- modify_if(key2, .p = ~ncol(.x) > 0, .f = ~select(.x, 1))
    keys_final <- bind_rows(key3) %>% tidyr::unite("key", na.rm = T) %>%
      mutate(key = as.numeric(key)) %>% na.omit() %>% distinct() %>% pull()
    message(paste(sp, keys_final))
  out <- tryCatch (
    {
      res_gbif <- purrr::map(keys_final,
                             ~rgbif::occ_search(taxonKey = .x, basisOfRecord = "PRESERVED_SPECIMEN",#Foi
                                                limit = 100000)$data)
      res <- bind_rows(res_gbif)
      write_csv(res, fs::path(pasta_out, sp, ext = "csv"))
      print(sp)
      },
    error = function(e) {
      message(paste(sp, key, "problem"))
      message(e)
      },
    warning = function(w) {
      message(paste(sp, key, "caused a warning"))
      message(w)
      })
  return(out)
  }
  }
hay <- list.files(pasta_out) %>% stringr::str_remove(".csv")
setdiff(hay, syn_df$nome_aceito_correto)
falta <- setdiff(syn_df$nome_aceito_correto, hay)
falta_df <- syn_df %>% filter(nome_aceito_correto %in% falta)
df_falta <- furrr::future_map(falta_df$sinonimo_file, ~read_csv(.x), .progress = T)
plan(multisession, workers = 15)
gf <- furrr::future_map(df_falta, ~download_rgbif(.x), .progress = T)
future::plan(sequential)


# filtra as tabelas de gbif e ressalva elas em outro lugar
#nombres de archivos que necesitan ser filtrados
file_data_frame <- function(folder) {
  path <- list.files(folder, full.names = T)
  name <- basename(path) %>% stringr::str_remove(".csv")
  fdf  <- tibble(names = name,
                 paths = path)
  return(fdf)
}

gbifs_df <- file_data_frame("output/p2/occs/gbif/")
syn_df <- file_data_frame("output/p2/sinonimos/")

#el archivo que filtra

este <- left_join(gbifs_df, syn_df, by = "names") %>%
  rename(occs = paths.x,
         syns = paths.y)
#funcion que lee y filtra
le_filtra <- function(occs, syn, dir = outdir) {
  syn <- readr::read_csv(syn)
  sp <- syn %>% pull(especie) %>% unique()
  print(sp)
  nomes <- syn %>% pull(nomes)
  print(nomes)
  x <- readr::read_csv(occs, guess_max = 105000)
  print(unique(x$species))
  #busca todos os sinonimos
  x2 <- dplyr::filter(x, species %in% nomes)
  message(nrow(x), "original rows ", nrow(x2), "final rows")
  if (nrow(x2) != 0) {
    x2$data_cleaning_note <- "subset a sinonimos da FB2020"
    write_csv(x2, file = paste0(dir,"/", sp, ".csv"))
  }
  #si no encuentra los nombre o sinonimos en la lista, los busca en la columna de scientific name....
  if (nrow(x2) == 0) {
  detecta <-
    purrr::map(nomes,
               ~which(stringr::str_detect(string = x$scientificName,
                                        pattern = .))) %>% simplify()
  #...y busca los nombres correctos asociados
  correct <- unique(x$species[detecta])
  x3 <- x[x$species %in% correct,]
  if (nrow(x3) != 0) {
    x3$data_cleaning_note <- "subset a nome correto segundo GBIF"
    write_csv(x3, file = paste0(dir,"/", sp, "_SYNONYM",".csv"))
    message(nrow(x), "original rows ", nrow(x3), "final rows")
  }
  #si esto no sirve como en el caso de mala ortografia o de subespecies y variantes, mantiene igual pero deja la nota
  if(nrow(x3) == 0) {
    x$data_cleaning_note <- "sem subset: checar ortografia FB/GBIF"
    write_csv(x,
              file = paste0(dir,"/", sp, "_NAO_LIMPOU_CHECK_NOME",
                            ".csv"))
  }
  }
}
#salva aqui
outdir <- paste0("output/p2/occs/clean")


plan(multisession, workers = 15)
gbifs <- furrr::future_map2(.x = este$occs,
                            .y = este$syns,
                            ~le_filtra(occs = .x, syn = .y),
                            .progress = T, seed = T)
plan(sequential)

#quien limpio?
list.files("output/p2/occs/gbif/", pattern = ".csv") %>% length()
list.files(outdir) %>% length()

rodou <- furrr::future_map(este$names,
            ~any(stringr::str_detect(string = list.files(outdir),
                                     pattern = .)))
plan(sequential)
simplify(rodou) %>% sum()
simplify(rodou) %>% table()
nao_limpou <- este$names[!simplify(rodou)]
#no se por que tuve que limpiar el resto a mano pero limpio direitinho
for (i in c(2:214)) {
a <- este[which(este$names == nao_limpou[i]),]
le_filtra(occs = a$occs, syn = a$syns)
}


#tudo limpooooou
