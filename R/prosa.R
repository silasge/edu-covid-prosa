library(tidyr)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

.read_prosa <- function(path) {
  is_utf_8 <- str_detect(path, "LP") & str_detect(path, "2018")
  is_excel <- (str_detect(path, "MT") & str_detect(path, "2019") & str_detect(path, "2Av")) | (str_detect(path, "2021"))
  is_latin1 <- !(is_utf_8 | is_excel)
  
  if (is_utf_8) prosa <- read_csv2(path, locale = locale(encoding = "UTF-8"))
  else if (is_latin1) prosa <- read_csv2(path, locale = locale(encoding = "latin1"))
  else if (is_excel) prosa <- read_excel(path)
  
  is_2017 <- str_detect(path, "2017")
  is_2018 <- str_detect(path, "2018")
  is_2019_1av <- str_detect(path, "2019") & !str_detect(path, "2Av")
  is_2019_2av <- str_detect(path, "2019") & str_detect(path, "2Av")
  is_2021 <- str_detect(path, "2021")

  list(prosa = prosa %>% 
         mutate(NU_ANO = str_extract(path, "2017|2018|2019|2021") %>% as.numeric(),
                NU_AVALIACAO = if_else(str_detect(path, "2Av"), 2, 1)),
       is_2017 = is_2017, 
       is_2018 = is_2018,
       is_2019_1av = is_2019_1av,
       is_2019_2av = is_2019_2av, 
       is_2021 = is_2021)
}

.clean_prosa <- function(prosa_lst) {
  if (prosa_lst$is_2017) {
    prosa <- prosa_lst$prosa %>% drop_na(CD_ALUNO) %>% 
      mutate(NM_SEXO = NA_character_,
             VL_PROFICIENCIA = VL_PROFICIENCIA / 1000) # TODO: divisão por 1000 só em 2017-1. corrigir para o 2017-2
  } else if (prosa_lst$is_2018 | prosa_lst$is_2019_1av) {
    prosa <- prosa_lst$prosa %>% mutate(NM_SEXO = NA_character_)
  } else if (prosa_lst$is_2019_2av) {
    prosa <- prosa_lst$prosa
  } else if (prosa_lst$is_2021) {
    prosa <- prosa_lst$prosa %>%
      mutate(`ANO ESCOLARIZAÇÃO` = str_extract(`ANO ESCOLARIZAÇÃO`, "^\\d") %>% as.numeric(),
             MATRICULA = MATRICULA %>% as.numeric()) %>%
      rename(`LÍNGUA PORTUGUESA` = `NOTA PROFICIENCIA PORTUGUES`,
             `MATEMÁTICA` = `NOTA PROFICIENCIA MATERIA MATEMATICA`) %>%
      pivot_longer(cols = c(`LÍNGUA PORTUGUESA`, `MATEMÁTICA`),
                   names_to = "NM_DISCIPLINA",
                   values_to = "VL_PROFICIENCIA")
  }
  prosa <- prosa %>%
    rename_with(~ case_when(
      .x %in% c("NM_REGIONAL_ESCOLA", "CRE") ~ "NM_REGIONAL",
      .x %in% c("CD_ESCOLA", "INEP") ~ "ID_ESCOLA",
      .x %in% c("DC_TURNO_TURMA", "DC_TURNO") ~ "NM_TURNO",
      .x %in% c("CD_ALUNO_INSTITUCIONAL", "MATRICULA") ~ "ID_MATRICULA",
      .x %in% c("CD_SEXO") ~ "NM_SEXO",
      .x %in% c("DC_DEFICIENCIA_ALUNO", "DC_DEFICIENCIA") ~ "NM_DEFICIENCIA",
      .x %in% c("CD_ETAPA_APLICACAO", "CD_ETAPA_APLICACAO_TURMA", "ANO ESCOLARIZAÇÃO", "CD_ETAPA_PUBLICACAO") ~ "NU_ETAPA",
      TRUE ~ .x
    )) %>%
    mutate(
      NU_ETAPA = case_when(
        NU_ETAPA == 18 ~ 5,
        NU_ETAPA == 41 ~ 9,
        NU_ETAPA == 16 ~ 3,
        NU_ETAPA == 15 ~ 2,
        TRUE ~ NU_ETAPA
      )
    )

  vars_select <- c("NU_ANO", "NU_AVALIACAO", "NM_REGIONAL", "ID_ESCOLA", "NM_TURMA",
                   "NM_TURNO", "ID_MATRICULA", "NM_SEXO", "NM_DEFICIENCIA", "NM_DISCIPLINA",
                   "NU_ETAPA", "VL_PROFICIENCIA")
  
  prosa %>% select(any_of(vars_select))
}

get_prosa <- function(path) {
  prosa <- .read_prosa(path) %>% .clean_prosa()
}

