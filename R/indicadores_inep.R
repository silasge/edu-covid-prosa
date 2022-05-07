library(readxl)
library(tidyr)
library(stringr)
library(dplyr)
library(magrittr)


# ---- ATU - Alunos por Turma ----

.read_atu <- function(path) {
  read_excel(path, skip = 8, na = "--")
}

.clean_atu <- function(atu) {
  ano <- atu %>% select(NU_ANO_CENSO) %>% distinct(NU_ANO_CENSO) %>% purrr::pluck(1, 1)
  if (ano == 2018) {
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
  } else if (ano == 2017) {
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
    pivot_longer(cols = `1`:`9`,
                 names_to = "id_etapa",
                 values_to = "atu") %>%
    mutate(id_etapa = as.numeric(id_etapa),
           nm_faixa_etapa = case_when(id_etapa %in% c(1, 2, 3, 4, 5) ~ "Anos Iniciais",
                                      id_etapa %in% c(6, 7, 8, 9) ~ "Anos Finais"))
  
}

get_atu <- function(path, ...) {
  atu <- .read_atu(path) %>% .clean_atu() %>% filter(...)
}

# ---- DSU - Docentes com Ensino Superior ----

.read_dsu <- function(path) {
  read_excel(path, skip = 9, na = "--")
}

.clean_dsu <- function(dsu) {
  dsu %>%
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
    pivot_longer(cols = `Total`:`Anos Finais`,
                        names_to = "nm_faixa_etapa",
                        values_to = "dsu")
}

get_dsu <- function(path, ...) {
  dsu <- .read_dsu(path) %>% .clean_dsu() %>% filter(...)
}

# ---- HAD - Horas Aula Diárias ----

.read_had <- function(path) {
  read_excel(path, skip = 8, na = "--") 
}

.clean_had <- function(had) {
  had %>%
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
    pivot_longer(FUN_01_CAT_0:FUN_09_CAT_0, 
                        names_to = "id_etapa",
                        values_to = "had") %>%
    mutate(id_etapa = id_etapa %>% str_extract("\\d\\d") %>% as.numeric(),
           nm_faixa_etapa = case_when(id_etapa %in% c(1, 2, 3, 4, 5) ~ "Anos Iniciais",
                                      id_etapa %in% c(6, 7, 8, 9) ~ "Anos Finais"))
}

get_had <- function(path, ...) {
  had <- .read_had(path) %>% .clean_had() %>% filter(...)
}

# ---- ICG - Índice de Complexidade Gestão ----

.read_icg <- function(path) {
  read_excel(path, skip = 10, na = "--")
}

.clean_icg <- function(icg) {
  vars <- c("NU_ANO_CENSO", "Ano")
  ano <- icg %>% select(any_of(vars)) %>% distinct() %>% purrr::pluck(1, 1)
  if (ano %in% c(2017, 2018)) {
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
    drop_na(nu_ano)
}

get_icg <- function(path, ...) {
  icg <- .read_icg(path) %>% .clean_icg() %>% filter(...)
}

# ---- IDEB ----

.read_ideb <- function(path) {
  read_excel(path, skip = 9, na = "-") %>%
    mutate(nm_faixa_etapa = case_when(
             str_detect(path, "anos_finais") ~ "Anos Finais",
             str_detect(path, "anos_iniciais") ~ "Anos Iniciais")
    )
}

.clean_ideb <- function(ideb) {
  ideb %>%
    select(
      sg_uf = SG_UF,
      id_municipio = CO_MUNICIPIO,
      nm_municipio = NO_MUNICIPIO,
      id_escola = ID_ESCOLA,
      nm_escola = NO_ESCOLA,
      nm_dependencia = REDE,
      nm_faixa_etapa,
      VL_OBSERVADO_2005,
      VL_OBSERVADO_2007, 
      VL_OBSERVADO_2009,
      VL_OBSERVADO_2011,
      VL_OBSERVADO_2013,
      VL_OBSERVADO_2015,
      VL_OBSERVADO_2017,
      VL_OBSERVADO_2019
    ) %>%
    pivot_longer(
      VL_OBSERVADO_2005:VL_OBSERVADO_2019,
      names_to = "nu_ano",
      values_to = "ideb"
    ) %>%
    mutate(
      nu_ano = nu_ano %>%
        str_extract("\\d{4}") %>%
        as.numeric()
    ) %>%
    relocate(nu_ano, .before = sg_uf) %>%
    relocate(nm_faixa_etapa, .before = ideb)
}

get_ideb <- function(path, ...) {
  ideb <- .read_ideb(path) %>% .clean_ideb() %>% filter(...)
}


# ---- INSE - Nível Sócio Econômico ----

.read_inse <- function(path) {
  read_excel(path)
}

.clean_inse <- function(inse) {
  inse %>%
    mutate(nu_ano = 2019) %>%
    select(
      nu_ano,
      id_municipio = CO_MUNICIPIO,
      id_escola = CO_ESCOLA,
      inse = INSE_VALOR_ABSOLUTO,
    )
}

get_inse <- function(path, ...) {
  inse <- .read_inse(path) %>% .clean_inse() %>% filter(...)
}


# --- IED - Índice de Esforço Docente ----

.read_ied <- function(path) {
  read_excel(path, skip = 10, na = "--")
}

.clean_ied <- function(ied) {
  ied %>%
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
    pivot_longer(
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

get_ied <- function(path, ...) {
  ied <- .read_ied(path) %>% .clean_ied() %>% filter(...)
}

# ---- IRD - Índice de Regularidade Docente ----

.read_ird <- function(path) {
  read_excel(path, skip = 10, na = "--")
}

.clean_ird <- function(ird) {
  ird %>%
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
      )
}

get_ird <- function(path, ...) {
  ird <- .read_ird(path) %>% .clean_ird() %>% filter(...)
}

# ---- TDI - Taxa de Distorção Idade Série ----

.read_tdi <- function(path) {
  read_excel(path, skip = 8, na = "--")
}

.clean_tdi <- function(tdi) {
  tdi %>%
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
        .x == "TDI_F00" ~ "FUN_01_CAT_0",	
        .x == "TDI_F01" ~ "FUN_02_CAT_0",
        .x == "TDI_F02" ~ "FUN_03_CAT_0",
        .x == "TDI_F03" ~ "FUN_04_CAT_0",
        .x == "TDI_F04" ~ "FUN_05_CAT_0",
        .x == "TDI_F05" ~ "FUN_06_CAT_0",
        .x == "TDI_F06" ~ "FUN_07_CAT_0",	
        .x == "TDI_F07" ~ "FUN_08_CAT_0",
        .x == "TDI_F08" ~ "FUN_09_CAT_0",
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
    pivot_longer(FUN_01_CAT_0:FUN_09_CAT_0, 
                        names_to = "id_etapa",
                        values_to = "tdi") %>%
    mutate(id_etapa = id_etapa %>% str_extract("\\d\\d") %>% as.numeric(),
           nm_faixa_etapa = case_when(id_etapa %in% c(1, 2, 3, 4, 5) ~ "Anos Iniciais",
                                      id_etapa %in% c(6, 7, 8, 9) ~ "Anos Finais"))
}

get_tdi <- function(path, ...) {
  tdi <- .read_tdi(path) %>% .clean_tdi() %>% filter(...)
}

# ---- Base de Indicadores ----

create_base_indicadores <- function(atu, had, icg, ideb, inse, ied, ird, tdi) {
  atu %>%
    #left_join(dsu %>% select(nu_ano, id_escola, nm_faixa_etapa)) %>%
    left_join(had %>% select(nu_ano, id_escola, id_etapa, nm_faixa_etapa, had)) %>%
    left_join(icg %>% select(nu_ano, id_escola, icg)) %>%
    left_join(ideb %>% select(nu_ano, id_escola, nm_faixa_etapa, ideb)) %>%
    left_join(inse %>% select(nu_ano, id_escola, inse)) %>%
    left_join(ied %>% select(nu_ano, id_escola, nm_faixa_etapa, ied)) %>%
    left_join(ird %>% select(nu_ano, id_escola, ird)) %>%
    left_join(tdi %>% select(nu_ano, id_escola, id_etapa, nm_faixa_etapa, tdi))
}
