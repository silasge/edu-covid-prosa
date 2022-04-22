library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


read_and_clean_had <- function(path) {
  tdi <- read_excel(path, skip = 8, na = "--") %>%
    rename_with(
      ~ case_when(
        .x %in% c("Ano", "NU_ANO_CENSO") ~ "nu_ano",
        .x %in% c("SIGLA", "SG_UF") ~ "sg_uf",
        .x %in% c("PK_COD_MUNICIPIO", "CO_MUNICIPIO") ~ "id_municipio",
        .x == "NO_MUNICIPIO" ~ "nm_municipio",
        .x %in% c("PK_COD_ENTIDADE", "CO_ENTIDADE") ~ "id_escola",
        .x == "NO_ENTIDADE" ~ "nm_escola",
        .x %in% c("TIPOLOCA", "NO_CATEGORIA") ~ "nm_localizacao",
        .x %in% c("DEPENDAD", "NO_DEPENDENCIA") ~ "nm_dependencia",
        .x == "HAD_F00" ~ "FUN_01_CAT_0",	
        .x == "HAD_F01" ~ "FUN_02_CAT_0",
        .x == "HAD_F02" ~ "FUN_03_CAT_0",
        .x == "HAD_F03" ~ "FUN_04_CAT_0",
        .x == "HAD_F04" ~ "FUN_05_CAT_0",
        .x == "HAD_F05" ~ "FUN_06_CAT_0",
        .x == "HAD_F06" ~ "FUN_07_CAT_0",	
        .x == "HAD_F07" ~ "FUN_08_CAT_0",
        .x == "HAD_F08" ~ "FUN_09_CAT_0",
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
      starts_with("FUN_0")
    ) %>%
    tidyr::drop_na(id_escola) %>%
    tidyr::pivot_longer(FUN_01_CAT_0:FUN_09_CAT_0, 
                        names_to = "id_etapa",
                        values_to = "had") %>%
    mutate(id_etapa = id_etapa %>%
             str_extract("\\d\\d") %>%
             as.numeric())
}

had <- map_dfr(
  fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/media_horas_aula", glob = "*.xlsx"),
  ~ read_and_clean_had(.x)
)

DBI::dbWriteTable(conn = conn, "had", had, overwrite = TRUE)

DBI::dbDisconnect(conn)
