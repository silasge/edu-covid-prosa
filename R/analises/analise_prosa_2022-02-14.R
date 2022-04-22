library(RPostgres)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(stringr)
library(sf)
library(RColorBrewer)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


prosa_indicadores <- DBI::dbGetQuery(conn, "SELECT * FROM media_escolas_indicadores")

readr::write_csv(prosa_indicadores, "./data/processed/prosa_indicadores.csv")

# ---- PRÉ ANÁLISE ----

prosa <- prosa_indicadores %>%
  filter(id_etapa %in% c(5, 9)) %>%
  mutate(nm_regional = nm_regional %>%
                          str_replace("CIDADE BAIXA", "LIBERDADE"),
         nu_avaliacao = glue::glue("{nu_ano}-{nu_avaliacao}"))

geometria_gre <- read_sf("./data/raw/shapefiles/gre/gre_2019.gpkg")


# ---- DISTRIBUIÇÃO ----

sumario_para_anotar <- prosa %>%
  group_by(nu_avaliacao, id_etapa, nm_disciplina) %>%
  summarise(
    minim = min(vl_nota_media, na.rm = TRUE),
    media = mean(vl_nota_media, na.rm = TRUE),
    dp = sd(vl_nota_media, na.rm = TRUE),
    maxi = max(vl_nota_media, na.rm = TRUE)
  ) %>%
  mutate(
    .label = glue::glue("Min: {round(minim, 2)}
                         Média: {round(media, 2)}
                         DP: {round(dp, 2)}
                         Máx: {round(maxi, 2)}") %>%
      str_replace_all("\\.", ","),
  )


dist_plots <- function(etapa, disciplina, limits = c(0, 0.04), n_breaks = 6) {
  ggplot(prosa %>% 
           filter(id_etapa == etapa,
                  nm_disciplina == disciplina), aes(x = vl_nota_media)) +
    geom_histogram(aes(y = ..density.., fill = nu_avaliacao), color = "black") +
    geom_density() +
    facet_wrap(. ~ nu_avaliacao) +
    geom_text(data = sumario_para_anotar %>%
                filter(id_etapa == etapa,
                       nm_disciplina == disciplina), 
              aes(label = .label), 
              x = 0, 
              y = 0.02,
              size = 2.5,
              hjust = 0,
              family = "Roboto Condensed") +
    theme_ipsum_rc(grid = "XY", 
                   axis = "xy",
                   plot_title_margin = 2,
                   subtitle_margin = 5,
                   plot_margin = margin(2, 0, 0, 0)) +
    scale_color_ipsum() +
    scale_fill_ipsum() +
    scale_y_comma(limits = limits, accuracy = 0.01, decimal.mark = ",") +
    scale_x_comma(n.breaks = n_breaks) +
    theme(
      legend.position = "none",
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    ) +
    labs(
      title = glue::glue("Distribuição das Notas Médias - {str_to_title(disciplina)}"),
      subtitle = glue::glue("Por Edições do Prosa e Escolas do {etapa}º Ano"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador",
      x = "Nota Média",
      y = "Densidade"
    ) -> plt
  return(plt)
}


purrr::walk2(
  .x = c(5, 9, 5, 9),
  .y = c("LÍNGUA PORTUGUESA", "LÍNGUA PORTUGUESA", "MATEMÁTICA", "MATEMÁTICA"),
  ~ dist_plots(etapa = .x, disciplina = .y) %>%
    ggsave(glue::glue("./reports/22-02-14_graficos/dist_{.x}ano_{str_to_lower(str_replace(.y, ' ', '_'))}.png"), plot = .)
)


# ---- MAPAS ----

prosa_por_gre <- prosa %>%
  group_by(nu_avaliacao, nm_regional, id_etapa, nm_disciplina) %>%
  summarise(nota_media = mean(vl_nota_media, na.rm = TRUE)) %>%
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


map_plots <- function(edicao, etapa, disciplina) {
  .prosa <- prosa_por_gre %>%
    filter(nu_avaliacao == edicao,
           id_etapa == etapa,
           nm_disciplina == disciplina)
  
  leg_breaks <- round(seq(min(.prosa$nota_media), max(.prosa$nota_media), length = 5), 2)
  leg_breaks[1] <- leg_breaks[1] + 0.01
  leg_breaks[5] <- leg_breaks[5] - 0.01
  
  
  plt <- ggplot() +
    geom_sf(data = .prosa, aes(geometry = geom, fill = nota_media)) +
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
      title = glue::glue("Notas Médias em {str_to_title(disciplina)} - Edição {edicao}"),
      subtitle = glue::glue("Por Gerência Regional e Escolas do {etapa}º Ano"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador",
      fill = "Nota Média",
      x = NULL,
      y = NULL
    )
  
  return(plt)
}


for (ed in c("2018-1", "2019-1", "2019-2", "2021-1")) {
  for (et in c(5, 9)) {
    for (di in c("LÍNGUA PORTUGUESA", "MATEMÁTICA")) {
      plt <- map_plots(ed, et, di)
      ggsave(glue::glue("./reports/22-02-14_graficos/map_ed_{ed}_{et}ano_{str_to_lower(str_replace(di, ' ', '_'))}.png"), plot = plt)
    }
  }
}


# ---- DISPERSÃO ----


anotacoes_disp <- prosa %>%
  group_by(nu_avaliacao, id_etapa, nm_disciplina) %>%
  tidyr::nest()



disp_plots <- function(etapa, disciplina, var_x) {
  .prosa <- prosa %>%
    filter(id_etapa == etapa,
           nm_disciplina == disciplina) %>%
    mutate(nm_regional = nm_regional %>%
             str_to_title() %>%
             str_replace("Ii", "II"))
  
  var <- rlang::sym(var_x)
  
  plt <- ggplot(.prosa, aes(x = !!var, y = vl_nota_media)) +
    geom_point(aes(color = nm_regional)) +
    facet_wrap(~ nu_avaliacao, scales = "free") +
    geom_smooth(method = "lm", se = FALSE) +
    theme_ipsum_rc(grid = "XY", 
                   axis = "xy",
                   plot_title_margin = 2,
                   subtitle_margin = 5,
                   plot_margin = margin(2, 0, 0, 0)) +
    labs(
      title = glue::glue("Nota Média em {str_to_title(disciplina)} x {str_to_upper(var_x)}"),
      subtitle = glue::glue("Por Edições do Prosa, Escolas do {etapa}º Ano e Gerencias Regionais"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador",
      color = "GRE",
      x = str_to_upper(var_x),
      y = "Nota Média"
    ) +
    theme(
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    )
}


for (et in c(5, 9)) {
  for (di in c("LÍNGUA PORTUGUESA", "MATEMÁTICA")) {
    for (var in c("atu", "tdi", "ird", "ied")) {
      plt <- disp_plots(et, di, var)
      ggsave(glue::glue(glue::glue("./reports/22-02-14_graficos/disp_{et}ano_{str_to_lower(str_replace(di, ' ', '_'))}_{var}.png"), plot = plt))
    }
  }
}


# ---- JITTER ----


jitter_plots <- function(etapa, disciplina, var_x) {
  .prosa <- prosa %>%
    filter(id_etapa == etapa,
           nm_disciplina == disciplina) %>%
    mutate(nm_regional = nm_regional %>%
             str_to_title() %>%
             str_replace("Ii", "II"))
  
  var <- rlang::sym(var_x)
  
  medias <- .prosa %>%
    group_by(nu_avaliacao, !!var) %>%
    summarise(nota_media = mean(vl_nota_media, na.rm = TRUE),
              dp_nota_media = sd(vl_nota_media, na.rm = TRUE),
              lower = nota_media - dp_nota_media,
              upper = nota_media + dp_nota_media,
              n = n()) %>%
    filter(n != 1)
  
  plt <- ggplot() +
    geom_jitter(data = .prosa, aes(x = !!var, y = vl_nota_media, color = nm_regional), size = 1.5, alpha = 0.5, width = 0.1) +
    geom_point(data = medias, aes(x = !!var, y = nota_media), size = 2) +
    facet_wrap(. ~ nu_avaliacao, scales = "free") +
    coord_flip() +
    theme_ipsum_rc(grid = "XY", 
                   axis = "xy",
                   plot_title_margin = 2,
                   subtitle_margin = 5,
                   plot_margin = margin(2, 0, 0, 0)) +
    labs(
      title = glue::glue("Nota Média em {str_to_title(disciplina)} x {str_to_upper(var_x)}"),
      subtitle = glue::glue("Por Edições do Prosa, Escolas do {etapa}º Ano e Gerencias Regionais"),
      caption = "Fonte: Sec. Municipal de Educação de Salvador",
      color = "GRE",
      x = str_to_upper(var_x),
      y = "Nota Média"
    ) +
    theme(
      plot.caption.position = "plot",
      plot.caption = element_text(hjust = 0)
    )
}


purrr::walk2(
  .x = c(5, 9, 5, 9),
  .y = c("LÍNGUA PORTUGUESA", "LÍNGUA PORTUGUESA", "MATEMÁTICA", "MATEMÁTICA"),
  ~ jitter_plots(etapa = .x, disciplina = .y, var_x = "icg") %>%
    ggsave(glue::glue("./reports/22-02-14_graficos/jitt_{.x}ano_{str_to_lower(str_replace(.y, ' ', '_'))}.png"), 
           plot = .,
           width = 6.39,
           height = 3.79)
)
