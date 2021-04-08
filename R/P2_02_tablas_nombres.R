library(dplyr)
library(purrr)


#crear las tablas de nombre y sinonimos
#o seguinte passo vai ser 1. carregar
spp_files <- list.files("output/p2/check_flora/", full.names = T)
spp <- stringr::str_remove(basename(spp_files), "\\.rda$")
#carrega e renomeia
load_and_rename <- function(object, name) {
  load(object)
  assign(name, a)
}
## test
load_and_rename(object = spp_files[1], name = spp[1])

lista_completa <- purrr::map2(spp_files, spp, ~load_and_rename(.x, .y))
lista_completa[1]
names(lista_completa) <- spp


#extrai a tabela de sinonimos, pode ser NULL
syn_todas <- purrr::map(lista_completa,
                        ~.x$synonyms)
#quando tem?
tax_syn_yes_no <- purrr::map(tax_todas,
                             ~unique(.x$synonyms))
#quantas?
table(unlist(tax_syn_yes_no))

#checa o  string quando a tabela nao eh nula
check_non_null <- function(x) {
  if (!is.null(x)) {
    syn <- unlist(Rocc::check_string(x$scientificName)$species)
    return(syn)
  }
}
#testando
a <- check_non_null(NULL)
a <- check_non_null(syn_todas[[7]])
a <- check_non_null(syn_todas[[1]])
#aplica:
syn_list <- purrr::map(syn_todas,
                      ~check_non_null(.x))
# temos uma lista de nomes com os sinonimos quando a especie tem sinonimos

#formata em tibble
sin_df <- purrr::imap(syn_list,
                     ~dplyr::tibble(especie = .y, nomes =c(.y, .x))
                     )
pasta <- "output/p2/sinonimos"
dir.create(pasta)
purrr::imap(sin_df, ~readr::write_csv(.x, file = fs::path(pasta, .y, ext = "csv")))

write.csv(data.frame(nome = sp, sinonimos = syn), file = paste0("output/p2/", unique(sp), ".csv"))

#sucesso
