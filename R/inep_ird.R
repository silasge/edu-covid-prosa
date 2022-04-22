library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


read_and_clean_ird <- function(path) {
  tdi <- read_excel(path, skip = 10, na = "--") %>%
    rename_with(
      ~ case_when(
        .x %in% c("ano", "NU_ANO_CENSO") ~ "nu_ano",
        .x %in% c("SIGLA", "SG_UF") ~ "sg_uf",
        .x %in% c("PK_COD_MUNICIPIO", "CO_MUNICIPIO") ~ "id_municipio",
        .x == "NO_MUNICIPIO" ~ "nm_municipio",
        .x %in% c("PK_COD_ENTIDADE", "CO_ENTIDADE") ~ "id_escola",
        .x == "NO_ENTIDADE" ~ "nm_escola",
        .x %in% c("TIPOLOCA", "NO_CATEGORIA") ~ "nm_localizacao",
        .x %in% c("Dependad", "NO_DEPENDENCIA") ~ "nm_dependencia",
        .x %in% c("MIRD", "EDU_BAS_CAT_0") ~ "ird",	
        TRUE ~ .x
      )
    ) %>%
    select(
      nu_ano,
      sg_uf,
      id_municipio,
      nm_municipio,
      id_escola,
      nm_escola,
      nm_localizacao,
      nm_dependencia,
      ird
    ) %>%
    tidyr::drop_na(id_escola)
}

ird <- map_dfr(
  fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/regularidade_corpo_docente", glob = "*.xlsx"),
  ~ read_and_clean_ird(.x)
)

DBI::dbWriteTable(conn = conn, "ird", ird, overwrite = TRUE)

DBI::dbDisconnect(conn)
