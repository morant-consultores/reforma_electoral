library(tidyr)
devtools::load_all("~/Documents/Git/aelectoral2")
bd <- Electoral$new("df_21", entidad = "nacional",
                    llaves = c("seccion", "distritof"))


bd$partido("distritof_21", "df_21")
bd$candidato(al_df_21,"distritof_21", "df_21")
bd

bd$bd_candidato$df_21 %>%
  pivot_longer(-distritof_21) %>% group_by(distritof_21) %>% filter(value == max(value)) %>% ungroup %>%
  separate(distritof_21, c("entidad", "distrito")) %>% count(entidad, name) %>%
  pivot_wider(names_from = name, values_from = n) %>%
  mutate(across(-entidad, ~replace_na(.x,0))) %>%
  write_excel_csv("data/real_edos.csv")

