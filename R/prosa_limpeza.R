library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(RPostgres)

# PROSA 2014: observações
# - Mudar a codificação
# - Não tem o ID INEP das escolas
# - O que são os "X" em algumas colunas? Seria o equivalente a um NA?
# - Não tem as notas?
#prosa_14 <- read_csv2("./edu-covid-data/data/projeto_prosa/raw/prosa/Microdados_Prosa_2014.csv", locale = locale(encoding = "latin1"))


prosa_reader <- function(path) {
  is_utf_8 <- str_detect(path, "LP") & str_detect(path, "2018")
  is_nothing <- str_detect(path, "MT") & str_detect(path, "2019") & str_detect(path, "2Av")
  is_latin1 <- !(is_utf_8) & !(is_nothing)
  
  if (is_utf_8) {
    prosa <- read_csv2(path, locale = locale(encoding = "UTF-8"))
  }
  else if (is_latin1) {
    prosa <- read_csv2(path, locale = locale(encoding = "latin1"))
  }
  else if (is_nothing) {
    prosa <- read_excel(path)
  }
}


clean_prosa <- function(paths) {
  dfs <- list()
  for (i in seq_along(paths)) {
    prosa <- prosa_reader(paths[i])
    
    
    
    if (str_detect(paths[i], "2019") & str_detect(paths[i], "2Av")) {
      prosa <- prosa %>%
        rename(NM_REGIONAL_ESCOLA = NM_REGIONAL,
               DC_LOCALIZACAO_ESCOLA = DC_LOCALIZACAO,
               DC_TURNO_TURMA = DC_TURNO)
    } else if (str_detect(paths[i], "2017")) {
      prosa <- prosa %>%
        rename(CD_ETAPA = CD_ETAPA_APLICACAO_TURMA) %>%
        tidyr::drop_na(CD_ALUNO) %>%
        mutate(CD_SEXO = NA_character_)
        
    } else {
      prosa <- prosa %>%
        mutate(CD_SEXO = NA_character_)
    }
    prosa <- prosa %>%
      mutate(
        NU_ANO = str_extract(paths[i], "2017|2018|2019|2021") %>% as.numeric(),
        NU_AVALIACAO = if_else(str_detect(paths[i], "2Av"), 2, 1)
      ) %>%
      select(
        NU_ANO,
        NU_AVALIACAO,
        NM_REGIONAL = NM_REGIONAL_ESCOLA,
        ID_ESCOLA = CD_ESCOLA,
        NM_ESCOLA,
        NM_LOCALIZACAO = DC_LOCALIZACAO_ESCOLA,
        NM_TURNO = DC_TURNO_TURMA,
        ID_ALUNO = CD_ALUNO_INSTITUCIONAL,
        NM_SEXO = CD_SEXO,
        ID_ETAPA = CD_ETAPA,
        NM_DISCIPLINA,
        VL_PROFICIENCIA, 
        VL_PERC_ACERTOS
      )
    dfs[[i]] <- prosa
  }
  dfs %>% bind_rows()
}

prosa <- clean_prosa(fs::dir_ls(c("./edu-covid-data/data/projeto_prosa/raw/prosa/2017", "./edu-covid-data/data/projeto_prosa/raw/prosa/2018", "./edu-covid-data/data/projeto_prosa/raw/prosa/2019"))) %>%
  bind_rows(
    read_excel("./edu-covid-data/data/projeto_prosa/raw/prosa/2021/prosa_2021.xlsx") %>%
      mutate(NU_ANO = 2021,
             NU_AVALIACAO = 1,
             `ANO ESCOLARIZAÇÃO` = str_extract(`ANO ESCOLARIZAÇÃO`, "^\\d") %>%
               as.numeric(),
             MATRICULA = MATRICULA %>% as.numeric()) %>%
      select(
        NU_ANO,
        NU_AVALIACAO,
        NM_REGIONAL = CRE,
        ID_ESCOLA = INEP,
        NM_ESCOLA = ESCOLA,
        ID_ALUNO = MATRICULA,
        ID_ETAPA = `ANO ESCOLARIZAÇÃO`,
        `LÍNGUA PORTUGUESA` = `NOTA PROFICIENCIA PORTUGUES`,
        `MATEMÁTICA` = `NOTA PROFICIENCIA MATERIA MATEMATICA`
      ) %>%
      tidyr::pivot_longer(cols = c(`LÍNGUA PORTUGUESA`, `MATEMÁTICA`),
                          names_to = "NM_DISCIPLINA",
                          values_to = "VL_PROFICIENCIA") %>%
      filter(ID_ETAPA %in% seq(1, 9))
  ) %>%
  arrange(ID_ESCOLA) %>%
  rename_with(~ str_to_lower(.x))

#readr::write_csv(prosa, "./edu-covid-data/data/projeto_prosa/intermed/prosa/prosa_17181921.csv", na = "")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres",
                       password = "123456789")

DBI::dbWriteTable(conn,"prosa", prosa, overwrite = TRUE)
DBI::dbDisconnect(conn)
