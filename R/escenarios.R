devtools::load_all("~/Documents/Git/aelectoral2/")

library(tidyverse)

bd <- Electoral$new("df_21", entidad = "nacional", extranjero = T)
bd$bd <- bd$bd %>% mutate(distritof_21 = paste(estado, distritof_21, sep = "_"))
bd$partido("distritof_21", "df_21")
data("al_df_21")
bd$candidato(alianzas = al_df_21, "distritof_21", "df_21")

bd$bd_candidato$df_21 %>%
  pivot_longer(-distritof_21) %>%
  separate(distritof_21, into=c("estado", "distrito")) %>%
  group_by(estado,name) %>%
  summarise(n=sum(is.na(value)),
            N=n_distinct(distrito)) %>%
  filter(grepl(pattern = "pan|prd|pri|morena|pt|pvem",name)) %>%
  pivot_wider(names_from = name,values_from =  n) %>%
  mutate(va_por_mexico=cand_pan_pri_prd_df_21>
           pmax(ele_pan_df_21, ele_pri_df_21, ele_prd_df_21),
         juntos_hacemos_historia=cand_pan_pri_prd_df_21>
           pmax(ele_pan_df_21, ele_pri_df_21, ele_prd_df_21),
  ) %>%
  select(estado, va_por_mexico, juntos_hacemos_historia) #%>% View()

coaliciones <- bd$bd_candidato$df_21 %>%
  pivot_longer(-distritof_21) %>%
  separate(distritof_21, into=c("estado", "distrito")) %>%
  group_by(estado,name) %>%
  summarise(n=sum(!is.na(value)),
            N=n_distinct(distrito)) %>%
  filter(grepl(pattern = "pan|prd|pri|morena|pt|pvem",name)) %>%
  pivot_wider(names_from = name,values_from =  n) %>%
  mutate(va_por_mexico=cand_pan_pri_prd_df_21>
           pmax(ele_pan_df_21, ele_pri_df_21, ele_prd_df_21),
         juntos_hacemos_historia=cand_pvem_pt_morena_df_21>
           pmax(ele_morena_df_21, ele_pt_df_21, ele_pvem_df_21),
  ) %>%
  select(estado, va_por_mexico, juntos_hacemos_historia)

metodo_Hondt <- function(bd){
  curules <- bd$n
  bd <- bd %>% pivot_longer(starts_with("ele_"))
  res <- 1:curules %>%
    map(~bd %>% mutate(value=value/.x)) %>%
    reduce(bind_rows) %>% arrange(desc(value)) %>%
    top_n(n=curules) %>%
    count(name)
  return(res)
}

# Considerando alianzas parciales ########
votos <- bd$bd_partido$df_21 %>%
  separate(distritof_21, into=c("estado", "distrito")) %>%
  group_by(estado) %>%
  summarise(n=n(),across(starts_with("ele_"), ~sum(.x))) %>%
  inner_join(coaliciones) %>%
  mutate(ele_vapormexico=ifelse(va_por_mexico,
                                ele_pan_df_21+ele_pri_df_21+ele_prd_df_21, 0),
         ele_juntoshacemoshistoria=ifelse(juntos_hacemos_historia,
                                          ele_morena_df_21+ele_pt_df_21+ele_pvem_df_21, 0),
         ele_morena_df_21=ifelse(!juntos_hacemos_historia, ele_morena_df_21, 0),
         ele_pt_df_21=ifelse(!juntos_hacemos_historia, ele_pt_df_21, 0),
         ele_pvem_df_21=ifelse(!juntos_hacemos_historia, ele_pvem_df_21, 0),
         ele_pan_df_21=ifelse(!va_por_mexico, ele_pan_df_21, 0),
         ele_pri_df_21=ifelse(!va_por_mexico, ele_pri_df_21, 0),
         ele_prd_df_21=ifelse(!va_por_mexico, ele_prd_df_21, 0)) %>%
  select(-ele_nulos_df_21, -ele_fxm_df_21, -ele_ci_df_21,
         -ele_rsp_df_21, -ele_pes_df_21)

reparticion <- votos %>%
  nest(-estado) %>%
  mutate(resultado=map(data, ~metodo_Hondt(.x)))

conteo <- reparticion %>% select(-data) %>% unnest(resultado)

conteo <- conteo %>%
  mutate(name=stringr::str_remove_all(name, pattern = "(ele_)|(_df_21)"),
         name=case_when(name=="juntoshacemoshistoria"~"juntos hacemos historia",
                        name=="vapormexico"~"va por mexico",
                        T~name
         ))
parciales <- conteo %>%
  group_by(name) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(pct=n/sum(n))

parciales_estado <- conteo %>% pivot_wider(names_from = name, values_from = n) %>%
  mutate(across(-estado, ~replace_na(.x, 0)))

parciales_estado %>% summarise(across(-estado, ~sum(.x)))

######## Alianzas totales ####
votos_ctotales <- bd$bd_partido$df_21 %>%
  separate(distritof_21, into=c("estado", "distrito")) %>%
  group_by(estado) %>%
  summarise(n=n(),across(starts_with("ele_"), ~sum(.x))) %>%
  inner_join(coaliciones) %>%
  mutate(ele_vapormexico=ele_pan_df_21+ele_pri_df_21+ele_prd_df_21,
         ele_juntoshacemoshistoria=ele_morena_df_21+ele_pt_df_21+ele_pvem_df_21) %>%
  select(estado,n,ele_vapormexico, ele_mc_df_21, ele_juntoshacemoshistoria)

reparticion <- votos_ctotales %>%
  nest(-estado) %>%
  mutate(resultado=map(data, ~metodo_Hondt(.x)))

conteo <- reparticion %>% select(-data) %>% unnest(resultado)

conteo <- conteo %>%
  mutate(name=stringr::str_remove_all(name, pattern = "(ele_)|(_df_21)"),
         name=case_when(name=="juntoshacemoshistoria"~"juntos hacemos historia",
                        name=="vapormexico"~"va por mexico",
                        T~name
         ))

totales <- conteo %>%
  group_by(name) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(pct=n/sum(n))

totales_estado <- conteo %>% pivot_wider(names_from = name, values_from = n) %>%
  mutate(across(-estado, ~replace_na(.x, 0)))

totales_estado %>% summarise(across(-estado, ~sum(.x)))
######## Sin alianza ####

votos_sc <- bd$bd_partido$df_21 %>%
  separate(distritof_21, into=c("estado", "distrito")) %>%
  group_by(estado) %>%
  summarise(n=n(),across(starts_with("ele_"), ~sum(.x))) %>%
  select(-ele_nulos_df_21, -ele_fxm_df_21, -ele_ci_df_21,
         -ele_rsp_df_21, -ele_pes_df_21)

reparticion <- votos_sc %>%
  nest(-estado) %>%
  mutate(resultado=map(data, ~metodo_Hondt(.x)))

conteo <- reparticion %>% select(-data) %>% unnest(resultado)

conteo <- conteo %>%
  mutate(name=stringr::str_remove_all(name, pattern = "(ele_)|(_df_21)"),
         name=case_when(name=="juntoshacemoshistoria"~"juntos hacemos historia",
                        name=="vapormexico"~"va por mexico",
                        T~name
         ))

sin <- conteo %>%
  group_by(name) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  mutate(pct=n/sum(n))


sin_estado <- conteo %>% pivot_wider(names_from = name, values_from = n) %>%
  mutate(across(-estado, ~replace_na(.x, 0)))

sin_estado %>% summarise(across(-estado, ~sum(.x)))


sin %>% write_excel_csv("data/sin.csv")
parciales %>% write_excel_csv("data/parciales.csv")
totales %>% write_excel_csv("data/totales.csv")


sin_estado %>% write_excel_csv("data/sin_estado.csv")
parciales_estado %>% write_excel_csv("data/parciales_estado.csv")
totales_estado %>% write_excel_csv("data/totales_estado.csv")
