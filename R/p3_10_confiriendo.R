nrow(rtf_filt_fica)
nrow(FICA_MESMO) +
nrow(FICA_MESMO2) +
nrow(CHECAR_MPOS)
bind_geral <- bind_rows(FICA_MESMO, FICA_MESMO2) %>%
  bind_rows(CHECAR_MPOS)
names(bind_geral) %in% names(rtf_filt_fica)
names(rtf_filt_fica) %in% names(bind_geral)

no_dupl$id %in% rtf_filt_fica$id

no_dupl$etiqueta_duplicaDO %in% rtf_filt_fica$etiqueta_duplicaDO
colunas <- c(id, )

count(rtf_filt_fica, status_dupl)
no_dupl <- rtf_filt_fica %>% filter(status_dupl == "NO_DUPL")
ha_dupl <- rtf_filt_fica %>% count(etiqueta_duplicaDO) %>% filter(n != 1)
count(rtf_filt_fica, status_dupl)
rtf_filt_fica <- rtf_filt_fica %>%
  mutate(status_dupl = case_when(
    !etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "NO_DUPL",
    etiqueta_duplicaDO %in% ha_dupl$etiqueta_duplicaDO ~ "DUPL"
  ))

count(rtf_filt_22, status_dupl)
write_csv(rtf_filt_fica, "output/p3/p3_Planilha_filtrada28MAY.csv")
names(rtf_filt_fica)

count(rtf_filt_fica, status_dupl, which_dupl)
rtf_filt_fica <- rtf_filt_fica %>%
group_by(etiqueta_duplicaDO) %>%  mutate(which_dupl = if_else(status_dupl == "DUPL" &
                                                                (is.na(lat_original) | is.na(long_original)),
                                                              "no_coordinates", "this"))

