library(dplyr)
library(ggplot2)
library(hrbrthemes)


# A função abaixo serve para ler a base do prosa e aplicar algumas pré-modificações
ler_base_prosa <- function(...) {
  base_prosa <- readr::read_csv("base_prosa_coelba_inep.csv") %>%
    mutate(nu_avaliacao = stringr::str_c(nu_ano, nu_avaliacao, sep = "-"),
           nm_disciplina = nm_disciplina %>%
             stringr::str_to_title(),
           nm_regional = nm_regional %>%
             # Antes de 2021 só existia a GRE "Liberdade" e não existia o da Cidade Baixa
             # Para compatibilizar com os outros anos, tô substituindo todo "CIDADE BAIXA" para "LIBERDADE"
             stringr::str_replace("CIDADE BAIXA", "LIBERDADE") %>% 
             stringr::str_to_title() %>%
             stringr::str_replace("Ii", "II") %>%
             factor()) %>%
    filter(...)
}

## ---- Plotando o gráfico ----

## Preparação dos dados

prosa <- ler_base_prosa(
  id_etapa == 5, # 5º ano
  nu_avaliacao == "2018-1", # Primeira prova de 2018
  nm_disciplina == "Língua Portuguesa" # Prova de Língua Portuguesa
)

prosa <- prosa %>%
  # Dropar NA na variável das notas, se não o forcats::fct_reorder não funciona
  # corretamente para ordenar as GRE's de forma decrescente pelas notas
  tidyr::drop_na(vl_nota_media) %>%
  mutate(nm_regional = forcats::fct_reorder(nm_regional, vl_nota_media, mean)) %>%
  inner_join(
    prosa %>%
      group_by(nm_regional) %>%
      summarise(media_regional = mean(vl_nota_media, na.rm = TRUE))
  )

## Média total de Salvador, para criar a linha verdical com a média
media_total <- prosa %>%
  summarise(media_total = mean(vl_nota_media, na.rm = TRUE)) %>%
  pull(media_total)


## Um df com as cordenadas das setas usadas nas legendas

arrows <- tibble(
  xinicio = c(243, 165, 171),
  xfim = c(251.8, 160.5, media_total),
  yinicio = c(6.8, 3.4, 9.2),
  yfim = c(7.9, 1.2, 8.5)
)



## O gráfico
ggplot(prosa, aes(x = vl_nota_media, y = nm_regional, color = nm_regional)) +
  geom_segment(
    aes(y = nm_regional, 
        yend = nm_regional, 
        x = media_total, 
        xend = media_regional),
    size = 0.8) + # size é a grossura da linha de segmento q vai da média geral até a média da GRE
  geom_vline(aes(xintercept = media_total), color = "gray70", size = 0.6) +
  geom_jitter(size = 3, alpha = 0.25, position = position_jitter(seed = 123456, height = 0.1)) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  theme_ipsum_rc(
    grid = FALSE, 
    axis = TRUE,
    plot_title_margin = 2,
    subtitle_margin = 5,
    plot_margin = margin(2, 0, 0, 0)
  ) +
  theme(
    legend.position = "none"
  ) +
  labs(
    title = "Nota Média no PROSA 2018-1",
    subtitle = "Em Língua Portuguesa nas Escolas Municipais do 5º Ano",
    x = "Nota Média",
    y = "Gerência Regional de Educação"
  ) +
  annotate(
    geom = "text",
    x = 253,
    y = 6.2,
    label = "EM Senhora Santana\n Nota Média: 252,90",
    size = 3,
    hjust = 1 # 1 é texto justificado à direita, 0 é justificado à esquerda e -1 é centralizado
  ) +
  annotate(
    geom = "text",
    x = 155,
    y = 4,
    label = "EM de Ilha de Maré \nNota Média: 159,74",
    size = 3,
    hjust = 0
  ) +
  annotate(
    geom = "text",
    x = 160,
    y = 9.5,
    label = glue::glue("Média Geral: {round(media_total, 2)}"),
    size = 3,
    hjust = 0
  ) +
  geom_curve(
    data = arrows,
    aes(x = xinicio, xend = xfim, y = yinicio, yend = yfim),
    arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
    color = "gray20", curvature = 0
  )

ggsave("media_prosa_lp_20181.png")
