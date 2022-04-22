library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(purrr)
library(jtools)
library(hrbrthemes)

base_prosa <- read_csv("./data/processed/prosa/base_prosa_coelba_inep.csv") %>%
  filter(id_etapa %in% c(5, 9)) %>%
  mutate(
    edicao = glue::glue("{nu_ano}-{nu_avaliacao}"),
    nm_regional = nm_regional %>%
      str_replace("CIDADE BAIXA", "LIBERDADE"),
    porte_escola = porte_escola %>%
      str_replace_all("matrículas de escolarização", "") %>%
      str_trim()
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
    porte_escola,
    kwh_comercial, 
    kwh_industrial, 
    kwh_residencial, 
    kwh_resultado
  )


estimacoes <- base_prosa %>%
  group_by(edicao, id_etapa, nm_disciplina) %>%
  mutate(
    across(c(vl_nota_media, contains("kwh")), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))) %>%
  nest() %>%
  mutate(
    residencial = map(
      data,
      ~ lm(vl_nota_media ~ atu + had + factor(icg) + ied + ird + tdi + factor(porte_escola) + kwh_residencial, data = .x) %>%
        tidy()),
    comercial = map(
      data,
      ~ lm(vl_nota_media ~ atu + had + factor(icg) + ied + ird + tdi + factor(porte_escola) + kwh_comercial, data = .x) %>%
        tidy()),
    total = map(
      data,
      ~ lm(vl_nota_media ~ atu + had + factor(icg) + ied + ird + tdi + factor(porte_escola) + kwh_resultado, data = .x) %>%
        tidy())
  )

obt_estimacoes <- function(estim_df, selecao, ...) {
  obj_name <- deparse(substitute(selecao))
  if (obj_name == "total") {
    coef_nm <- "kwh_resultado"
  } else if (obj_name == "comercial") {
    coef_nm <- "kwh_comercial"
  } else if (obj_name == "residencial") {
    coef_nm <- "kwh_residencial"
  }
  
  
  estimacoes <- estim_df %>% 
    filter(...) %>%
    select(edicao, id_etapa, nm_disciplina, {{ selecao }}) %>%
    arrange(edicao)
  
  disciplina <- unique(estimacoes$nm_disciplina) %>% str_to_title()
  
  etapa <- unique(estimacoes$id_etapa) %>% as.character()
  
  coef_title = coef_nm %>% str_replace("_", " ") %>% str_to_title()
  
  estimacoes <- estimacoes %>%
    pluck(4)
  
  
  plot <- do.call(plot_coefs, 
                  args = c(estimacoes, 
                           list(model.names = c("2018-1", "2019-1", "2019-2", "2021-1"), 
                                coefs = c("tdi", "atu", "had", "ied", coef_nm)))) +
    theme_ipsum_rc(grid = "XY", 
                   axis = "xy",
                   plot_title_margin = 2,
                   subtitle_margin = 5,
                   plot_margin = margin(2, 0, 0, 0)) +
    scale_x_continuous(limits = c(-0.4, 0.4)) +
    labs(
      title = glue::glue("Resultado de Regressão Linear - PROSA"),
      subtitle = glue::glue("{disciplina}, Escolas do {etapa}º Ano e {coef_title}"),
      y = "Variável",
      x = "Estimativa"
    )
}

(estimacoes_residencial <- obt_estimacoes(estimacoes, residencial, id_etapa == 5, nm_disciplina == "LÍNGUA PORTUGUESA"))
ggsave("./reports/22-03-14_graficos/regr_5ano_lingua_portuguesa_residencial.png")
(estimacoes_comercial <- obt_estimacoes(estimacoes, comercial, id_etapa == 5, nm_disciplina == "LÍNGUA PORTUGUESA"))
ggsave("./reports/22-03-14_graficos/regr_5ano_lingua_portuguesa_comercial.png")
(estimacoes_total <- obt_estimacoes(estimacoes, total, id_etapa == 5, nm_disciplina == "LÍNGUA PORTUGUESA"))
ggsave("./reports/22-03-14_graficos/regr_5ano_lingua_portuguesa_total.png")
