library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(purrr)
library(tidyr)

read_base_prosa <- function(...) {
  base_prosa <- readr::read_csv("./data/processed/prosa/base_prosa_coelba_inep.csv") %>%
    select(-c(vl_nota_sd, vl_nota_mediana)) %>%
    mutate(across(c(icg, nm_localizacao, porte_escola), factor),
           nu_avaliacao = stringr::str_c(nu_ano, nu_avaliacao, sep = "-"),
           nm_disciplina = nm_disciplina %>%
             stringi::stri_trans_general(id = "Latin-ASCII") %>%
             stringr::str_to_lower() %>%
             stringr::str_replace_all(" ", "_"),
           nm_regional = nm_regional %>%
             stringr::str_replace("CIDADE BAIXA", "LIBERDADE") %>%
             stringr::str_to_title() %>%
             stringr::str_replace("Ii", "II") %>%
             factor()) %>%
    tidyr::pivot_wider(names_from = nm_disciplina, values_from = vl_nota_media) %>%
    filter(...)
}

prosa <- read_base_prosa(id_etapa == 5)

prosa_anot <- prosa %>%
  select(nu_avaliacao, lingua_portuguesa, matematica, kwh_resultado, kwh_residencial, kwh_comercial) %>%
  group_by(nu_avaliacao) %>%
  nest() %>%
  mutate(
    b_lp_resultado = map_dbl(
      data, 
      ~ lm(lingua_portuguesa ~ kwh_resultado, data = .x) %>%
        coef() %>%
        pluck(2)),
    b_mt_resultado = map_dbl(
      data, 
      ~ lm(matematica ~ kwh_resultado, data = .x) %>%
        coef() %>%
        pluck(2)),
    b_lp_comercial = map_dbl(
      data, 
      ~ lm(lingua_portuguesa ~ kwh_comercial, data = .x) %>%
        coef() %>%
        pluck(2)),
    b_mt_comercial = map_dbl(
      data, 
      ~ lm(matematica ~ kwh_comercial, data = .x) %>%
        coef() %>%
        pluck(2)))

ggplot(data = prosa, aes(x = kwh_residencial, y = lingua_portuguesa)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ nu_avaliacao, scales = "free") +
  theme_ipsum_rc(grid = "XY", 
                 axis = "xy",
                 plot_title_margin = 2,
                 subtitle_margin = 5,
                 plot_margin = margin(2, 0, 0, 0)) +
  scale_x_continuous(n.breaks = 4, labels = scales::scientific) +
  labs(
    title = "Média em Língua Portuguesa x KWh Residencial do Bairro",
    subtitle = "Por Edições do Prosa e Escolas Municipais do 5º Ano",
    caption = "Fonte: Sec. Municipal de Educação de Salvador"
  ) +
  theme(
    plot.caption.position = "plot",
    plot.caption = element_text(hjust = 0)
  )


ggsave("./reports/22-03-07_graficos/disp_5ano_língua_portuguesa_kwh_residencial.png")  


