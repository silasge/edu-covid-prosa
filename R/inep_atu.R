library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


read_and_clean_atu <- function(path) {
  atu <- read_excel(path, skip = 8, na = "--")
  if (str_detect(path, "2018")) {
    atu <- atu %>%
      rename(
        cod_mun = NO_MUNICIPIO,
        nm_mun = CO_MUNICIPIO,
        cod_ent = NO_ENTIDADE,
        nm_ent = CO_ENTIDADE,
        NO_CATEGORIA = TIPOLOCA,
        NO_DEPENDENCIA = DEPENDAD,
        FUN_01_CAT_0 = ATU_F58,
        FUN_02_CAT_0 = ATU_F00,
        FUN_03_CAT_0 = ATU_F01,
        FUN_04_CAT_0 = ATU_F02,
        FUN_05_CAT_0 = ATU_F03,
        FUN_06_CAT_0 = ATU_F05,
        FUN_07_CAT_0 = ATU_F06,
        FUN_08_CAT_0 = ATU_F07,
        FUN_09_CAT_0 = ATU_F08
      ) %>%
      rename(
        CO_MUNICIPIO = cod_mun,
        NO_MUNICIPIO = nm_mun,
        CO_ENTIDADE = cod_ent,
        NO_ENTIDADE = nm_ent
      )
  } else if (str_detect(path, "2017")) {
    atu <- atu %>%
      rename(
        cod_mun = NO_MUNICIPIO,
        nm_mun = CO_MUNICIPIO,
        cod_ent = NO_ENTIDADE,
        nm_ent = CO_ENTIDADE,
        NO_CATEGORIA = TIPOLOCA,
        NO_DEPENDENCIA = DEPENDAD,
        FUN_01_CAT_0 = ATU_F00,
        FUN_02_CAT_0 = ATU_F01,
        FUN_03_CAT_0 = ATU_F02,
        FUN_04_CAT_0 = ATU_F03,
        FUN_05_CAT_0 = ATU_F04,
        FUN_06_CAT_0 = ATU_F05,
        FUN_07_CAT_0 = ATU_F06,
        FUN_08_CAT_0 = ATU_F07,
        FUN_09_CAT_0 = ATU_F08
      ) %>%
      rename(
        CO_MUNICIPIO = cod_mun,
        NO_MUNICIPIO = nm_mun,
        CO_ENTIDADE = cod_ent,
        NO_ENTIDADE = nm_ent
      )
  }
  atu <- atu %>%
    select(
      nu_ano = NU_ANO_CENSO,
      sg_uf = SG_UF,
      id_municipio = CO_MUNICIPIO,
      nm_municipio = NO_MUNICIPIO,
      id_escola = CO_ENTIDADE,
      nm_escola = NO_ENTIDADE,
      nm_localizacao = NO_CATEGORIA,
      nm_dependencia = NO_DEPENDENCIA,
      `1` = FUN_01_CAT_0,
      `2` = FUN_02_CAT_0,
      `3` = FUN_03_CAT_0,
      `4` = FUN_04_CAT_0,
      `5` = FUN_05_CAT_0,
      `6` = FUN_06_CAT_0,
      `7` = FUN_07_CAT_0,
      `8` = FUN_08_CAT_0,
      `9` = FUN_09_CAT_0
    ) %>%
    tidyr::drop_na(id_escola) %>%
    tidyr::pivot_longer(cols = `1`:`9`,
                        names_to = "id_etapa",
                        values_to = "atu") %>%
    mutate(id_etapa = as.numeric(id_etapa))
}


atu <- map_dfr(
  fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/media_turmas", glob = "*.xlsx"),
  ~ read_and_clean_atu(.x)
)


DBI::dbWriteTable(conn = conn, "atu", atu, overwrite = TRUE)

DBI::dbDisconnect(conn)
