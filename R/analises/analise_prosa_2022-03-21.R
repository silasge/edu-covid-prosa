library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(broom)
library(stringr)
library(magrittr)


base_prosa <- read_csv("./data/processed/prosa/base_prosa_coelba_inep.csv") %>%
  filter(id_etapa %in% c(5, 9)) %>%
  mutate(
    edicao = glue::glue("{nu_ano}-{nu_avaliacao}"),
    nm_regional = nm_regional %>%
      str_replace("CIDADE BAIXA", "LIBERDADE")
  ) %>%
  select(
    edicao,
    nm_regional,
    id_escola,
    id_etapa,
    nm_disciplina,
    nu_alunos,
    tx_respondentes,
    vl_nota_media,
    atu:icg,
    ied:tdi,
    nm_localizacao,
    kwh_comercial, 
    kwh_industrial, 
    kwh_residencial, 
    kwh_resultado
  )


estimacoes <- base_prosa %>%
  group_by(edicao, id_etapa, nm_disciplina) %>%
  mutate(vl_nota_media_padr = (vl_nota_media - mean(vl_nota_media, na.rm = TRUE)) / sd(vl_nota_media, na.rm = TRUE),
         kwh_resultado_2sd = (kwh_resultado - mean(kwh_resultado, na.rm = TRUE)) / 2*sd(kwh_resultado, na.rm = TRUE)) %>%
  nest() %>%
  mutate(
    total_nivel = map(
      data,
      ~ lm(vl_nota_media ~ kwh_resultado + factor(nm_regional) + atu + had + factor(icg) + ied + ird + tdi, data = .x) %>%
        tidy()),
    total_elast = map(
      data,
      ~ lm(log(vl_nota_media+1) ~ log(kwh_resultado+1) + factor(nm_regional) + atu + had + factor(icg) + ied + ird + tdi, data = .x) %>%
        tidy()),
    total_padr = map(
      data,
      ~ lm(vl_nota_media_padr ~ kwh_resultado + factor(nm_regional) + atu + had + factor(icg) + ied + ird + tdi, data = .x) %>%
        tidy()),
    total_padr_2sd = map(
      data,
      ~ lm(vl_nota_media ~ kwh_resultado_2sd + factor(nm_regional) + atu + had + factor(icg) + ied + ird + tdi, data = .x) %>%
        tidy())
    
  )



estimacoes %>% 
  filter(id_etapa == 5, nm_disciplina == "MATEMÁTICA") %$%
  walk2(.x = edicao,
        .y = total_padr_2sd,
        ~ writexl::write_xlsx(.y, glue::glue("./reports/22-03-21_graficos/reg_padr2sdind_5ano_matematica_{.x}.xlsx")))
