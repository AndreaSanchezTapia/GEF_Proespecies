library(stringr)
#no need anymore
# clean_all <- function(file_path) {
#   sp <- basename(file_path) %>% str_remove(".csv")
#   ff <- fs::path(fs::path_dir(file_path), paste0(sp, "_clean.csv"))
#   if (!file.exists(ff)) {
# file <- vroom::vroom(file_path, guess_max = 100000)
# clean <- file %>% mutate(basisOfRecord = tolower(basisOfRecord)) %>%
#   mutate(basisOfRecord = str_remove(basisOfRecord,"_")) %>%
#   filter(basisOfRecord %in% c("preservedspecimen", "preserverdspecimen"))
#   write_csv(clean, file = ff)
# print(unique(clean$basisOfRecord))
# }
# }

# library(furrr)
# plan(multisession, workers = 15)
# ya_t20 %>% furrr::future_map(~clean_all(.x), .progress = T)
# occs$gbif_file %>% furrr::future_map(~clean_all(.x), .progress = T)
# na.omit(occs$splink_file) %>% furrr::future_map(~clean_all(.x), .progress = T)
# plan(sequential)


