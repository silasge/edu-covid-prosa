library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")


read_and_clean_icg <- function(path) {
  icg <- read_excel(path, skip = 10, na = "--")
  if (str_detect(path, "2017|2018")) {
    icg <- icg %>%
      rename(
        NU_ANO_CENSO = Ano,
        SG_UF = SIGLA,
        CO_MUNICIPIO = PK_COD_MUNICIPIO,
        CO_ENTIDADE = PK_COD_ENTIDADE,
        NO_CATEGORIA = TIPOLOCA,
        NO_DEPENDENCIA = Dependad,
        COMPLEX = ICG
      )
  }
  icg <- icg %>%
    select(
      nu_ano = NU_ANO_CENSO,
      sg_uf = SG_UF,
      id_municipio = CO_MUNICIPIO,
      nm_municipio = NO_MUNICIPIO,
      id_escola = CO_ENTIDADE,
      nm_escola = NO_ENTIDADE,
      nm_localizacao = NO_CATEGORIA,
      nm_dependencia = NO_DEPENDENCIA,
      icg = COMPLEX
    ) %>%
    tidyr::drop_na(nu_ano)
}


icg <- map_dfr(
  fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/complexidade_gestao", glob = "*.xlsx"),
  ~ read_and_clean_icg(.x)
)


DBI::dbWriteTable(conn = conn, "icg", icg, overwrite = TRUE)

DBI::dbDisconnect(conn)





