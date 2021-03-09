library(googledrive)
gdrive_folder <- drive_find("CONSULTORIA")
all_files <- drive_ls(gdrive_folder)
dados <- drive_ls(all_files[all_files$name == "DADOS",])
dados_get <- drive_get(path = all_files[all_files$name == "DADOS",]$id)
SP_oficial <- dados %>% drive_get()

