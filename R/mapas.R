library(dplyr)
library(ggplot2)
library(ggspatial)
library(stringr)
library(RColorBrewer)
library(viridis)
library(wesanderson)
#library(plotly)
library(sf)
library(geobr)


source("./R/helpers_maps.R")


bahia_shp <- abrir_shp("ba") %>%
  filter(MUNICIPIO != "SALVADOR")

renda_bairros_ssa <- readxl::read_excel("./edu-covid-data/data/projeto_prosa/raw/bairros/salvador_populacao_e_renda.xlsx") %>%
  select(
    COD_BR = `Código`,
    renda_med_resp = `Rendimento médio dos responsáveis por domicílios particulares permanentes (R$)`
  ) %>%
  mutate(
    COD_BR = stringr::str_remove_all(COD_BR, "\\d+\\-") %>% stringr::str_pad(width = 4, side = "left", pad = "0")
  )

salvador_shp <- abrir_shp() %>%
  select(NM_BAIRROS, COD_BR, SHAPE_AREA, SHAPE_LEN, geometry) %>%
  inner_join(renda_bairros_ssa) %>%
  mutate(
    percentil = percent_rank(renda_med_resp),
    quartil = factor(case_when(
      percentil >= 0.0 & percentil < 0.25 ~ "1º Q",
      percentil >= 0.25 & percentil < 0.5 ~ "2º Q", 
      percentil >= 0.5 & percentil < 0.75 ~ "3º Q",
      percentil >= 0.75 ~ "4º Q"
    ), levels = c("1º Q", "2º Q", "3º Q", "4º Q"))
  )

escolas <- abrir_lista_de_escolas_ssa() %>%
  select(
    ID_ESCOLA = ID_INEP,
    ESCOLA,
    ENDERECO,
    LATITUDE,
    LONGITUDE
  )

sep_coord <- function(coord) {
  str_coord <- as.character(coord)
  coord_antdec <- str_coord %>% str_sub(end = 3)
  coord_depdec <- str_coord %>% str_sub(start = 4)
  new_coord <- as.numeric(glue::glue("{coord_antdec}.{coord_depdec}"))
  return(new_coord)
}


ideb <- readr::read_csv("./edu-covid-data/data/projeto_prosa/intermed/indicadores_inep/ideb_mun_salvador_2013_2015_2017_2019.csv") %>%
  filter(ANO == 2019) %>%
  tidyr::drop_na() %>%
  inner_join(escolas) %>%
  mutate(
    faixa_ideb = factor(case_when(
      IDEB >= 3.8 & IDEB <= 5.2 ~ "3,8 - 5,2",
      IDEB >= 5.3 & IDEB <= 5.6 ~ "5,3 - 5,6",
      IDEB >= 5.7 & IDEB <= 6.0 ~ "5,7 - 6,0",
      IDEB >= 6.1 & IDEB <= 7.2 ~ "6,1 - 7,2"
    ), levels = c("3,8 - 5,2", "5,3 - 5,6", "5,7 - 6,0", "6,1 - 7,2")),
    across(c(LATITUDE, LONGITUDE), ~ sep_coord(.x)),
    LONGITUDE = if_else(str_detect(ENDERECO, "ILHA DE MARE"), LONGITUDE - 0.005, LONGITUDE)
  )

ideb <- st_as_sf(ideb, coords = c("LONGITUDE", "LATITUDE"), 
                          crs = 4326)


ideb_e_renda_merge <- salvador_shp %>%
  st_intersection(ideb) %>%
  st_drop_geometry()

ideb_e_renda_merge %>%
  select_if(is.numeric) %>%
  corrr::correlate() %>%
  corrr::focus(renda_med_resp, IDEB, mirror = TRUE) %>%
  corrr::rearrange() %>%
  corrr::shave()

ideb_renda <- lm(IDEB ~ renda_med_resp, data = ideb_e_renda_merge)

ggplot(data = ideb_renda, aes(x = renda_med_resp, y = IDEB)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggsave("reg.png")

(p <- ggplot() +
    geom_sf(data = bahia_shp) +
    geom_sf(data = salvador_shp, aes(geometry = geometry, fill = quartil), color = "black") +
    scale_fill_brewer(palette="Blues") +
    geom_sf(data = ideb, aes(geometry = geometry, color = faixa_ideb), shape = 18, size = 2.5) +
    scale_color_brewer(palette="PRGn") +
    #scale_color_viridis(option = "mako", discrete = TRUE) +
    #scale_color_manual(values = wes_palette("FantasticFox1", type = "discrete")) + 
    annotation_scale(location = "br") +
    annotation_north_arrow(
      location = "tr", 
      which_north = "true", 
      #height = unit(2, "cm"), 
      #width = unit(2, "cm"), 
      style = north_arrow_fancy_orienteering) +
    theme_void() +
    labs(fill = "Quartis de Renda 2010 \n(Rendimento Médio \nNominal Mensal \ndos Chefes de Família)", color = "Ideb 2019 \n(Escolas Municipais)") +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm"),
      legend.title = element_text(size = 14, face = "bold"),
      legend.text = element_text(size = 14),
      panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5),
      panel.background = element_rect(fill = "aliceblue"),
      legend.position = c(0.15, 0.3),
      legend.background = element_rect(fill = "white", color = "black"),
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.margin = margin(6, 6, 6, 6)
    ) +
    coord_sf(xlim = c(-38.7, -38.22), ylim = c(-13.02, -12.75)))

ggsave("./reports/mapas/ideb_2019_renda_salvador_mapa.png", width = 3200, height = 1800, units = "px")
#ggsave("./reports/mapas/ideb_2019_renda_salvador_mapa.pdf", width = unit(2560, "px"), height = unit(2560, "px"))
