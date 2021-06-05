#planilha antiga
jun4 <- readr::read_csv("output/p3/p3_4JUN.csv", guess_max = 13000) %>% arrange(especie, id)

coord_jun <- jun4 %>% select(id, lat_corrigida, long_corrigida)

#refaz municipio consolidado primeiro
# library(dplyr)
# names(jun4)
# jun4 <- jun4 %>%
# mutate(municipio_consolidado = case_when(
#   !is.na(municipio_corrigidos_leo) ~ municipio_corrigidos_leo,
#   is.na(municipio_corrigidos_leo) & !is.na(municipio_pati) ~ municipio_pati,
#   is.na(municipio_corrigidos_leo) & is.na(municipio_pati) ~ municipio_final)) %>%
#   mutate(municipio_consolidado2 = if_else(is.na(municipio_consolidado) & !is.na(municipio), municipio, NA_character_)) %>%
#   mutate(municipio_consolidado3 = if_else(!is.na(municipio_consolidado2) & is.na(municipio_consolidado), municipio_consolidado2, municipio_consolidado))
# jun4 %>% filter(!is.na(municipio_consolidado3))

#resolver municipio modoficado pontualmente

#planilha leo
leo <- readr::read_csv("data/dados_formatados/p3/leo/p3_4JUN_leo.csv", guess_max = 13000) %>%
  arrange(especie, id)
# %>% distinct() %>% arrange(localidade) %>%write_csv("output/p3/p3_centroideUC.csv")
names(leo)
coord_leo <- leo %>%  filter(conflito_municipios == "conflito") %>% select(id, lat_corrigida, long_corrigida)
munjun <- jun4 %>% select(id, municipio_corrigidos_leo) %>% rename(mun_old = municipio_corrigidos_leo)
munleo <- leo %>% select(id, municipio_corrigidos_leo)%>% rename(mun_leo = municipio_corrigidos_leo)
munast <- ast %>% select(id, municipio_corrigidos_leo)%>% rename(mun_ast = municipio_corrigidos_leo)
municipios_novos_geral <- left_join(munjun, munast) %>%
  left_join(munleo) %>% mutate(
  municipio_corrigido_final = case_when(
    !is.na(mun_leo) ~ mun_leo,
    !is.na(mun_ast) ~ mun_ast,
    is.na(mun_leo) & !is.na(mun_old) ~ mun_old
  )
)
leo_UC %>% left_join(municipios_novos_geral)
leo %>% select(id, especie, localidade_x, coordenada_original_boa, )
municipios_novos_geral %>% count(mun_leo, mun_ast, mun_old, municipio_corrigido_final) %>% arrange(desc(n)) %>% View()

#  count(municipio_corrigidos_leo.x, municipio_corrigidos_leo.y, municipio_corrigidos_leo) %>% View()
 filter(municipio_corrigidos_leo.x == "Atibaia", is.na(municipio_corrigidos_leo))



leo <- leo %>%
   select(
  id, especie,
     municipio_corrigidos_leo,
  lat_corrigida,
  long_corrigida,
  situacao_coordenada,
  coordenada_original_boa,
  obs_leo)



 pat <- readr::read_csv("data/dados_formatados/p3/patricia/p3_4JUN - backup - planilha - 04-06-PR.csv", guess_max = 13000) %>% arrange(especie, id)

#ast
ast <- readr::read_csv("data/dados_formatados/p3/ast/p3_4JUN_AST.csv", guess_max = 13000) %>%
#ast <- ast %>%
  # rename(
  #   lat_corr_ast = lat_corrigida,
  #   long_corr_ast = long_corrigida,
  #   situacao_coordenada_ast = situacao_coordenada,
  #   coordenada_original_boa_ast = coordenada_original_boa
  # ) %>%
  arrange(especie,id)
coord_ast <- ast %>% select(id, lat_corrigida, long_corrigida)
coord_corrigida <- coord_jun %>% left_join(coord_leo, by = "id") %>% left_join(coord_ast, by = "id") %>%
  filter(!is.na(lat_corrigida.x) | !is.na(lat_corrigida.y) | !is.na(long_corrigida.x) | !is.na(long_corrigida.y))
coord_corrigida %>% View()
coord_corrigida <- coord_corrigida %>% mutate(lat_corrigida_final =
                            case_when(
                              lat_corrigida.x == lat_corrigida & is.na(lat_corrigida.y) ~ lat_corrigida,
                              lat_corrigida.x == lat_corrigida & !is.na(lat_corrigida.y) ~ lat_corrigida.y,
                              lat_corrigida.x != lat_corrigida & is.na(lat_corrigida.y) ~ lat_corrigida,
                            )) %>% mutate(long_corrigida_final =
                                            case_when(
                                              long_corrigida.x == long_corrigida & is.na(long_corrigida.y) ~ long_corrigida,
                                              long_corrigida.x == long_corrigida & !is.na(long_corrigida.y) ~ long_corrigida.y,
                                              long_corrigida.x != long_corrigida & is.na(long_corrigida.y) ~ long_corrigida,
                                            ))
View(coord_corrigida)
count(coord_corrigida, lat_corrigida.x == lat_corrigida.y)
count(coord_corrigida, lat_corrigida.x == lat_corrigida)
count(coord_corrigida, lat_corrigida.y == lat_corrigida)
coord_corrigida %>% filter(lat_corrigida_final == "-23075")
count(coord_corrigida, lat_corrigida_final) %>% arrange(lat_corrigida_final)
filter(coord_corrigida, lat_corrigida_final == -23075)




id_ast <- ast %>% transmute(id_mod = if_else(
  lat_corrigida != jun4$lat_corrigida |
  long_corrigida != jun4$long_corrigida |
    situacao_coordenada != jun4$situacao_coordenada |
  coordenada_original_boa != jun4$coordenada_original_boa, id, NA_real_))
sum(!is.na(id_ast$id_mod), na.rm = T)

ast_cols <- ast %>%
  select(
    id, especie,
    lat_original, long_original,
    municipio_corrigidos_leo,
    lat_corrigida,
    long_corrigida,
    situacao_coordenada,
    coordenada_original_boa,
    obs_leo)

left_join(ast_cols, leo, by = c("id", "especie")) %>% names() %>%
  mutate(municipio_corrigidos_leo = )


sum(!is.na(id_ast$id_mod))
id_leo <- leo %>% transmute(id_mod = if_else(
  municipio_corrigidos_leo != jun4$municipio_corrigidos_leo |
  lat_corrigida != jun4$lat_corrigida |
  long_corrigida != jun4$long_corrigida |
    situacao_coordenada != jun4$situacao_coordenada |
  coordenada_original_boa != jun4$coordenada_original_boa |
  obs_leo != jun4$obs_leo ,
  id, NA_real_))
sum(!is.na(id_leo$id_mod))
id_pat <- pat %>% transmute(id_mod = if_else(
  municipio_corrigidos_leo != jun4$municipio_corrigidos_leo |
  lat_corrigida != jun4$lat_corrigida |
  long_corrigida != jun4$long_corrigida |
  situacao_coordenada != jun4$situacao_coordenada |
  coordenada_original_boa != jun4$coordenada_original_boa ,
  #obs_leo != jun4$obs_leo ,
  id, NA_real_))
sum(!is.na(id_pat$id_mod))
jun4$lat_corrigida != pat$lat_corrigida
which(pat$long_corrigida != jun4$long_corrigida)
tibble(pat = pat$municipio_corrigidos_leo,
       jun = jun4$municipio_corrigidos_leo) %>% count(pat == jun)
names(pat)

leo_UC <- leo %>% filter(coordenada_original_boa == "Cent_UC") %>% select(id, especie,
                                                                          municipio_final,
                                                                          municipio_corrigidos_leo,
                                                                          municipio_consolidado,
                                                                          localidade_x, , nome_uc, coordenada_original_boa,
                                                                          situacao_coordenada)

coord_corrigida_leo <- coord_corrigida %>% select(id, lat_corrigida_final, long_corrigida_final)
original <- ast %>% select(id, lat_original, long_original)
leo_UC %>% left_join(original) %>% left_join(coord_corrigida_leo) %>% write_csv("output/p3/p3_UC_PARA_LEO.csv")
