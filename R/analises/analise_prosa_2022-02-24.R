library(RPostgres)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(sf)



# ---- PRÉ ANÁLISE ----

prosa_indicadores <- readr::read_csv("./data/processed/prosa_indicadores.csv") 

prosa <- prosa_indicadores %>%
  filter(id_etapa %in% c(5, 9)) %>%
  inner_join(prosa_indicadores %>%
               group_by(nu_ano, nu_avaliacao, nm_regional, nm_disciplina, id_etapa) %>%
               summarise(vl_nota_media_gre = mean(vl_nota_media, na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(nm_regional = nm_regional %>%
           str_replace("CIDADE BAIXA", "LIBERDADE"),
         nu_avaliacao = glue::glue("{nu_ano}-{nu_avaliacao}"),
         vl_media_escola_rel_geral = vl_nota_media / mean(vl_nota_media, na.rm = TRUE),
         vl_media_escola_rel_gre = vl_nota_media / mean(vl_nota_media_gre, na.rm = TRUE))



geometria_gre <- read_sf("./data/raw/shapefiles/gre/gre_2019.gpkg")


# ---- MAPAS ----


prosa_por_gre <- prosa %>%
  group_by(nu_avaliacao, nm_regional, id_etapa, nm_disciplina) %>%
  summarise(
    nu_alunos = sum(nu_alunos, na.rm = TRUE),
    media_respondentes = mean(tx_respondentes, na.rm = TRUE),
    media_escolas_rel_ger = mean(vl_media_escola_rel_geral, na.rm = TRUE),
    media_escolas_rel_gre = mean(vl_media_escola_rel_gre, na.rm = TRUE),
    mediana_notas = median(vl_nota_media, na.rm = TRUE),
    sd_notas = sd(vl_nota_media, na.rm = TRUE)) %>%
  inner_join(geometria_gre)

centroids_geom <- geometria_gre %>%
  mutate(centroids = st_centroid(geom)) %>%
  st_drop_geometry()

anotacoes_mapa <- tibble(
  label = centroids_geom$nm_regional %>% str_to_title() %>%  str_replace("Ii", "II"),
  cen_lon = purrr::map_dbl(centroids_geom$centroids, ~ .x %>% purrr::pluck(1, 1)),
  cen_lat = purrr::map_dbl(centroids_geom$centroids, ~ .x %>% purrr::pluck(2, 1))
) %>%
  mutate(
    x1 = case_when(
      label == "Itapua" ~ cen_lon + 0.03,
      label == "Orla" ~ cen_lon + 0.05,
      label == "Centro" ~ cen_lon - 0.08,
      label == "Liberdade" ~ cen_lon - 0.07,
      label == "Sao Caetano" ~ cen_lon - 0.1,
      label == "Suburbio I" ~ cen_lon - 0.05,
      label == "Piraja" ~ cen_lon - 0.08,
      label == "Cabula" ~ cen_lon + 0.08,
      label == "Cajazeiras" ~ cen_lon + 0.01,
      TRUE ~ cen_lon
    ),
    y1 = case_when(
      label == "Itapua" ~ cen_lat - 0.01,
      label == "Orla" ~ cen_lat - 0.02,
      label == "Cabula" ~ cen_lat - 0.03,
      label == "Centro" ~ cen_lat - 0.03,
      label == "Suburbio I" ~ cen_lat + 0.05,
      label == "Liberdade" ~ cen_lat - 0.02,
      label == "Cajazeiras" ~ cen_lat + 0.1,
      label == "Piraja" ~ cen_lat + 0.01,
      TRUE ~ cen_lat
    ),
    x2 = if_else(((x1 == cen_lon) & (y1 == cen_lat)) | (label == "Itapua"), NA_real_, cen_lon),
    y2 = if_else(((x1 == cen_lon) & (y1 == cen_lat)) | (label == "Itapua"), NA_real_, cen_lat),
    x2 = case_when(
      label == "Orla" ~ x2 + 0.01,
      TRUE ~ x2
    )
  )


map_plots <- function(edicao, etapa, disciplina, var) {
  .prosa <- prosa_por_gre %>%
    filter(nu_avaliacao == edicao,
           id_etapa == etapa,
           nm_disciplina == disciplina)
  
  main_title <- case_when(
    var == "media_respondentes" ~ "Média de Respondentes",
    var == "media_escolas_rel_ger" ~ "Média em Relação a Geral",
    var == "media_escolas_rel_gre" ~ "Média em Relação a GRE",
    var == "mediana_notas" ~ "Mediana das Notas",
    var == "sd_notas" ~ "Desvio Padrão das Notas"
  )
  
  var <- rlang::sym(var)
  
  vec_var <- .prosa %>% 
    ungroup() %>%
    select(!!var)
  
  leg_breaks <- round(seq(min(vec_var), max(vec_var), length = 5), 2)
  leg_breaks[1] <- leg_breaks[1] + 0.01
  leg_breaks[5] <- leg_breaks[5] - 0.01
  
  
  plt <- ggplot() +
    geom_sf(data = .prosa, aes(geometry = geom, fill = !!var)) +
    geom_segment(data = anotacoes_mapa, 
                 aes(x = x1, xend = x2, y = y1, yend = y2),
                 size = 0.4,
                 color = "gray20") + 
    geom_label(data = anotacoes_mapa,
               aes(x = x1, y = y1, label = label),
               size = 2.5,
               family = "Roboto Condensed") +
    scale_fill_gradientn(colours = c("#F27C4B", "#E7995E", "#A7AD8A", "#A6D397", "#8FD175"),
                         breaks = c(leg_breaks),
                         labels = scales::number_format(leg_breaks, decimal.mark = ",")) +
    theme_ipsum_rc(grid = FALSE, 
                   axis = FALSE,
                   plot_title_margin = 2,
                   subtitle_margin = 5,
                   plot_margin = margin(2, 0, 0, 0)) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    ) +
    labs(
      title = glue::glue("{main_title} - Edição {edicao}"),
      subtitle = glue::glue("Em {str_to_title(disciplina)}. Por Gerência Regional e Escolas do {etapa}º Ano"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador",
      fill = NULL,
      x = NULL,
      y = NULL
    )
  
  return(plt)
}


map_plots("2021-1", 5, "LÍNGUA PORTUGUESA", "media_escolas_rel_gre")



purrr::pwalk(
  tidyr::expand_grid(
    edicao = c("2018-1", "2019-1", "2019-2", "2021-1"),
    etapa = c(5, 9), 
    disciplina = c("LÍNGUA PORTUGUESA", "MATEMÁTICA"), 
    var = c("media_respondentes", 
            "media_escolas_rel_ger", 
            #"media_escolas_rel_gre", 
            "mediana_notas",
            "sd_notas")),
  function(edicao, etapa, disciplina, var) map_plots(edicao, etapa, disciplina, var) %>% 
    ggsave(glue::glue("./reports/22-02-24_graficos/map_ed_{edicao}_{etapa}ano_{str_to_lower(str_replace(disciplina, ' ', '_'))}_{var}.png"), plot = .)
)