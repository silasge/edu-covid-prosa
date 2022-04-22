library(readr)
library(dplyr)
library(sf)
library(ggplot2)


source("./R/helpers_maps.R")


salvador_shp <- abrir_shp() %>%
  select(
    COD_BR,
    NM_BAIRROS
  ) %>%
  distinct(COD_BR, .keep_all = TRUE) %>%
  st_intersection(
    abrir_lista_de_escolas_ssa() %>%
      select(
        ID_INEP,
        ESCOLA,
        LATITUDE,
        LONGITUDE
      ) %>%
      mutate(
        across(c(LATITUDE, LONGITUDE), ~ sep_coord(.x))
      ) %>%
      tidyr::drop_na() %>%
      st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  )


# TODO: saber de qual bairro é cada escola

escolas <- abrir_lista_de_escolas_ssa() %>%
  select(
    ID_INEP,
    ESCOLA,
    LATITUDE,
    LONGITUDE
  ) %>%
  mutate(
    across(c(LATITUDE, LONGITUDE), ~ sep_coord(.x))
  ) %>%
  tidyr::drop_na() %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)


salvador_shp_escolas <- salvador_shp %>%
  st_join(escolas)




(p <- ggplot() +
    geom_sf(data = salvador_shp, aes(geometry = geometry), color = "black"))
