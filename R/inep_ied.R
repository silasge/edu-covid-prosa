library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(RPostgres)


conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")

read_and_clean_ied <- function(path) {
  ied <- read_excel(path, skip = 10, na = "--") %>%
    rename_with(
      ~ case_when(
        .x %in% c("Ano", "NU_ANO_CENSO") ~ "nu_ano",
        .x %in% c("SIGLA", "SG_UF") ~ "sg_uf",
        .x %in% c("PK_COD_MUNICIPIO", "CO_MUNICIPIO") ~ "id_municipio",
        .x == "NO_MUNICIPIO" ~ "nm_municipio",
        .x %in% c("PK_COD_ENTIDADE", "CO_ENTIDADE") ~ "id_escola",
        .x == "NO_ENTIDADE" ~ "nm_escola",
        .x %in% c("TIPOLOCA", "NO_CATEGORIA") ~ "nm_localizacao",
        .x %in% c("Dependad", "NO_DEPENDENCIA") ~ "nm_dependencia",
        .x == "IED_FUN1" ~ "FUN_CAT_1",	
        .x == "IED_FUN2" ~ "FUN_CAT_2",
        .x == "IED_FUN3" ~ "FUN_CAT_3",
        .x == "IED_FUN4" ~ "FUN_CAT_4",
        .x == "IED_FUN5" ~ "FUN_CAT_5",
        .x == "IED_FUN6" ~ "FUN_CAT_6",
        .x == "IED_F141" ~ "FUN_AI_CAT_1",	
        .x == "IED_F142" ~ "FUN_AI_CAT_2",
        .x == "IED_F143" ~ "FUN_AI_CAT_3",
        .x == "IED_F144" ~ "FUN_AI_CAT_4",
        .x == "IED_F145" ~ "FUN_AI_CAT_5",
        .x == "IED_F146" ~ "FUN_AI_CAT_6",
        .x == "IED_F581" ~ "FUN_AF_CAT_1",	
        .x == "IED_F582" ~ "FUN_AF_CAT_2",
        .x == "IED_F583" ~ "FUN_AF_CAT_3",
        .x == "IED_F584" ~ "FUN_AF_CAT_4",
        .x == "IED_F585" ~ "FUN_AF_CAT_5",
        .x == "IED_F586" ~ "FUN_AF_CAT_6",
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
      starts_with("FUN_")
    ) %>%
    tidyr::drop_na(id_escola) %>%
    tidyr::pivot_longer(
      FUN_CAT_1:FUN_AF_CAT_6,
      names_to = "nm_faixa_etapa",
      values_to = "ied"
    ) %>%
    mutate(
      nu_cat_ied = nm_faixa_etapa %>%
        str_extract("\\d") %>%
        as.numeric(),
      nm_faixa_etapa = case_when(
        str_detect(nm_faixa_etapa, "AI") ~ "Anos Iniciais",
        str_detect(nm_faixa_etapa, "AF") ~ "Anos Finais",
        TRUE ~ "Total"
      )
    ) %>%
    relocate(nu_cat_ied, .before = ied)
}


ied <- map_dfr(
  fs::dir_ls(path = "./edu-covid-data/data/projeto_prosa/raw/inep/esforco_docente/", glob = "*.xlsx"),
  ~ read_and_clean_ied(.x)
)


DBI::dbWriteTable(conn = conn, "ied", ied, overwrite = TRUE)

DBI::dbDisconnect(conn)

