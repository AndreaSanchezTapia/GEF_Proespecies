# P3 busca ocorrencias
library(readr)
library(dplyr)
library(Rocc)
pasta_splink <- "output/p2/occs/splink/"

#sinonimos
df_sinonimos <- list.files("output/p2/sinonimos/", full.names = T)
nomes1 <- basename(df_sinonimos)
nomes <- stringr::str_remove(nomes1, ".csv")
names(df_sinonimos) <- nomes
syn_df <- tibble(nome_aceito_correto = nomes,
                 sinonimo_file = df_sinonimos)

#para cada especie que entra pelo criterio ameaca
p2 <- read_csv("output/p2/03_resumo_anotado.csv")
p2 <- read_csv("output/p2/p3_territorio20.csv")
entra <- p2 %>%
  #filter(cat_ameaca_geral == "entra") %>%
  select(nome_aceito_correto)

syn_df <- left_join(entra, syn_df)
syn_df %>% View()
#para cada especie

sp_link_now <- file_data_frame("output/p2/occs/splink/")
sp_link_entra <- sp_link_now %>% filter(names %in% syn_df$nome_aceito_correto)
falta_splink <- setdiff(syn_df$nome_aceito_correto, sp_link_entra$names)
syn_df <- filter(syn_df, nome_aceito_correto %in% falta_splink)
library(furrr)
plan(multisession, workers = 15)
df <- furrr::future_map(syn_df$sinonimo_file, ~read_csv(.x), .progress = T)
plan(sequential)

names(df)
oc <- df[[1]]
#baja ocurrencias de species link
download_splink <- function(oc, syn = T) {
  sp <- unique(oc$especie)
  if (syn) {
    search <- oc$nomes[stringr::str_detect(string = oc$nomes, "var\\.", negate = T)]
  } else search <- sp
  if (!file.exists(fs::path(pasta_splink, sp, ext = "csv"))) {
    message(paste(sp))

    out <- tryCatch (
      {
        sl <- rspeciesLink(dir = paste0(pasta_splink),
                     filename = sp,
                     species = search,
                     Scope = "plants",
                     Synonyms =  "flora2020")
        print(sp)
      },
      error = function(e) {
        message(paste(sp, "problem"))
        message(e)
      },
      warning = function(w) {
        message(paste(sp, "caused a warning"))
        message(w)
      })
    return(out)
  }
}
df
length(df)

plan(sequential)

gf <- furrr::future_map(df, ~download_splink(.x, syn = F), .progress = T)
for(i in 4:15) {
  download_splink(df[[i]], syn = T)
}

