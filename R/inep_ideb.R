library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "12345678")


read_and_clean_ideb <- function(path) {
  df <- read_excel(path, skip = 9, na = "-")  %>%
    select(
      sg_uf = SG_UF,
      id_municipio = CO_MUNICIPIO,
      nm_municipio = NO_MUNICIPIO,
      id_escola = ID_ESCOLA,
      nm_escola = NO_ESCOLA,
      nm_dependencia = REDE,
      VL_OBSERVADO_2005,
      VL_OBSERVADO_2007, 
      VL_OBSERVADO_2009,
      VL_OBSERVADO_2011,
      VL_OBSERVADO_2013,
      VL_OBSERVADO_2015,
      VL_OBSERVADO_2017,
      VL_OBSERVADO_2019
    ) %>%
    tidyr::drop_na(id_escola) %>%
    tidyr::pivot_longer(
      VL_OBSERVADO_2005:VL_OBSERVADO_2019,
      names_to = "nu_ano",
      values_to = "ideb"
    ) %>%
    mutate(
      nu_ano = nu_ano %>%
        str_extract("\\d{4}") %>%
        as.numeric(),
      nm_faixa_etapa = case_when(
        str_detect(path, "anos_finais") ~ "Anos Finais",
        str_detect(path, "anos_iniciais") ~ "Anos Iniciais"
      )
    ) %>%
    relocate(nu_ano, .before = sg_uf) %>%
    relocate(nm_faixa_etapa, .before = ideb)
}


ideb <- purrr::map_df(
  c("./edu-covid-data/data/projeto_prosa/raw/inep/ideb/divulgacao_anos_iniciais_escolas_2019.xlsx", "./edu-covid-data/data/projeto_prosa/raw/inep/ideb/divulgacao_anos_finais_escolas_2019.xlsx"),
  ~ read_and_clean_ideb(.x)
)


DBI::dbWriteTable(conn = conn, "ideb", ideb)


DBI::dbDisconnect(conn)
