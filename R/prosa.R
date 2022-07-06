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
  
  is_lp <- str_detect(path, "LP")
  is_mt <- str_detect(path, "MT")
  print(path)
  list(prosa = prosa %>% 
         mutate(nu_ano = str_extract(path, "2017|2018|2019|2021") %>% as.numeric(),
                nu_avaliacao = if_else(str_detect(path, "2Av"), 2, 1)),
       is_2017 = is_2017, 
       is_2018 = is_2018,
       is_2019_1av = is_2019_1av,
       is_2019_2av = is_2019_2av, 
       is_2021 = is_2021,
       is_lp = is_lp,
       is_mt = is_mt)
}

.clean_prosa <- function(prosa_lst) {
  if (prosa_lst$is_2017) {
    prosa <- prosa_lst$prosa %>% drop_na(CD_ALUNO) %>% 
      mutate(nm_sexo = NA_character_,
             VL_PROFICIENCIA = VL_PROFICIENCIA / 1000, # TODO: divisão por 1000 só em 2017-1. corrigir para o 2017-2
             nm_disciplina = case_when(prosa_lst$is_lp ~ "prosa_lp",
                                       prosa_lst$is_mt ~ "prosa_mt")) 
  } else if (prosa_lst$is_2018 | prosa_lst$is_2019_1av) {
    prosa <- prosa_lst$prosa %>% 
      mutate(
        nm_sexo = NA_character_,
        nm_disciplina = case_when(prosa_lst$is_lp ~ "prosa_lp",
                                  prosa_lst$is_mt ~ "prosa_mt")) 
      
  } else if (prosa_lst$is_2019_2av) {
    prosa <- prosa_lst$prosa %>%
      mutate(nm_disciplina = case_when(prosa_lst$is_lp ~ "prosa_lp",
                                       prosa_lst$is_mt ~ "prosa_mt"))
  } else if (prosa_lst$is_2021) {
    prosa <- prosa_lst$prosa %>%
      mutate(`ANO ESCOLARIZAÇÃO` = str_extract(`ANO ESCOLARIZAÇÃO`, "^\\d") %>% as.numeric(),
             MATRICULA = MATRICULA %>% as.numeric()) %>%
      rename(prosa_lp = `NOTA PROFICIENCIA PORTUGUES`,
             prosa_mt = `NOTA PROFICIENCIA MATERIA MATEMATICA`) %>%
      pivot_longer(cols = c(`prosa_lp`, `prosa_mt`),
                   names_to = "nm_disciplina",
                   values_to = "vl_proficiencia")
  }
  prosa <- prosa %>%
    rename_with(~ case_when(
      .x %in% c("NM_REGIONAL_ESCOLA", "CRE") ~ "nm_regional",
      .x %in% c("CD_ESCOLA", "INEP") ~ "id_escola",
      .x %in% c("DC_TURNO_TURMA", "DC_TURNO") ~ "nm_turno",
      .x %in% c("CD_ALUNO_INSTITUCIONAL", "MATRICULA") ~ "id_matricula",
      .x %in% c("CD_SEXO") ~ "nm_sexo",
      .x %in% c("DC_DEFICIENCIA_ALUNO", "DC_DEFICIENCIA") ~ "nm_deficiencia",
      .x %in% c("CD_ETAPA_APLICACAO", "CD_ETAPA_APLICACAO_TURMA", "ANO ESCOLARIZAÇÃO", "CD_ETAPA_PUBLICACAO") ~ "nu_etapa",
      .x == "VL_PROFICIENCIA" ~ "vl_proficiencia",
      TRUE ~ .x
    )) %>%
    mutate(
      nu_etapa = case_when(
        nu_etapa == 18 ~ 5,
        nu_etapa == 41 ~ 9,
        nu_etapa == 16 ~ 3,
        nu_etapa == 15 ~ 2,
        TRUE ~ nu_etapa
      )
    )

  vars_select <- c("nu_ano", "nu_avaliacao", "nm_regional", "id_escola", "nm_turma",
                   "nm_turno", "id_matricula", "nm_sexo", "nm_deficiencia", "nm_disciplina",
                   "nu_etapa", "vl_proficiencia")
  
  prosa %>% select(any_of(vars_select))
}

get_prosa <- function(path) {
  prosa <- .read_prosa(path) %>% .clean_prosa()
}

# TODO
#Error : Can't combine `..1$prosa_lp` <list> and `..9$prosa_lp` <double>.
#
#
#
#
#
