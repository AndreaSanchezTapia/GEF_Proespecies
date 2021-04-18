library(stringr)
ya_t20 <- list.files("output/p2/t20", full.names = T)
table(str_detect(ya_t20, "clean"))
ya <- basename(ya_t20) %>% str_remove(".csv")
sum(ya %in% occs$sp)
sum(!ya %in% occs$sp)
#unlink(ya_t20[!ya %in% occs$sp])



clean_all <- function(file_path) {
  sp <- basename(file_path) %>% str_remove(".csv")
  ff <- fs::path(fs::path_dir(file_path), paste0(sp, "_clean.csv"))
  if (!file.exists(ff)) {
file <- vroom::vroom(file_path, guess_max = 100000)
clean <- file %>% mutate(basisOfRecord = tolower(basisOfRecord)) %>%
  mutate(basisOfRecord = str_remove(basisOfRecord,"_")) %>%
  filter(basisOfRecord %in% c("preservedspecimen", "preserverdspecimen"))
  write_csv(clean, file = ff)
print(unique(clean$basisOfRecord))
}
}
occs %>% View()
write_csv(occs, "output/p2/06_occ_files.csv")
occs <- read_csv("output/p2/06_occ_files.csv")
ya <- basename(ya_t20) %>% str_remove("_clean") %>% str_remove(".csv") %>% unique(.)
nrow(occs)
length(ya)
setdiff(occs$sp, ya)
setdiff(ya, occs$sp)
falta <-
library(furrr)
plan(multisession, workers = 15)
ya_t20 %>% furrr::future_map(~clean_all(.x), .progress = T)
occs$gbif_file %>% furrr::future_map(~clean_all(.x), .progress = T)
na.omit(occs$splink_file) %>% furrr::future_map(~clean_all(.x), .progress = T)
na.omit(occs$splink_file)
plan(sequential)

