all <- list.files("output/p2/sel_sp", full.names = T)
plan(sequential)
all_occs <- read_csv(all[1], guess_max = 10000)  %>%
  mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
  mutate(across(is_double, .fns = function(x) as.character(x))) %>%
  mutate(across(is_logical, .fns = function(x) as.character(x)))

for (i in 2:length(all)) {
  new <- read_csv(all[i], guess_max = 10000)  %>%
    mutate(across(is_integer, .fns = function(x) as.character(x))) %>%
    mutate(across(is_double, .fns = function(x) as.character(x))) %>%
    mutate(across(is_logical, .fns = function(x) as.character(x)))
  all_occs <- bind_rows(all_occs, new)
}
all_occs <- all_occs %>% distinct()
i
