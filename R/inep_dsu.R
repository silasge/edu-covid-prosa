library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


read_and_clean_dsu <- function(path) {
  dsu <- read_excel(path, skip = 9, na = "--") %>%
    rename_with(
      ~ case_when(
        .x == "FUN_CAT0" ~ "FUN_CAT_0",
        .x == "FUN_AI_CAT0" ~ "FUN_AI_CAT_0",
        .x == "FUN_AF_CAT0" ~ "FUN_AF_CAT_0",
        TRUE ~ .x
      )
    ) %>%
    select(
      nu_ano = NU_ANO_CENSO,
      sg_uf = SG_UF,
      id_municipio = CO_MUNICIPIO,
      nm_municipio = NO_MUNICIPIO,
      id_escola = CO_ENTIDADE,
      nm_escola = NO_ENTIDADE,
      nm_localizacao = NO_CATEGORIA,
      nm_dependencia = NO_DEPENDENCIA,
      `Total` = FUN_CAT_0,
      `Anos Iniciais` = FUN_AI_CAT_0,
      `Anos Finais` = FUN_AF_CAT_0
    ) %>%
    tidyr::drop_na(id_escola) %>%
    tidyr::pivot_longer(cols = `Total`:`Anos Finais`,
                        names_to = "nm_faixa_etapa",
                        values_to = "dsu")
  
}


dsu <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_prosa/raw/inep/docentes_ensino_superior", glob = "*.xlsx"),
  ~ read_and_clean_dsu(.x)
)


DBI::dbWriteTable(conn = conn, "dsu", dsu)

DBI::dbDisconnect(conn)
