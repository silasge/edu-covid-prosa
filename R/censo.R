library(readr)
library(dplyr)


read_escola_censo <- function(path) {
  .delim <- if_else(stringr::str_detect(path, "2021"), ";", "|")
  read_delim(path, delim = .delim) %>%
    rename_with(
      ~ case_when(
        .x %in% c("IN_FUND_AF") ~ "IN_COMUM_FUND_AF",
        TRUE ~ .x
      )
    ) %>%
    filter(CO_MUNICIPIO == 2927408,
           IN_COMUM_FUND_AF == 1,
           TP_DEPENDENCIA == 3) %>%
    select(nu_ano = NU_ANO_CENSO, 
           id_escola = CO_ENTIDADE,
           #IN_MATERIAL_PED_NENHUM,
           #IN_AGUA_POTAVEL,
           #IN_AGUA_REDE_PUBLICA,
           #IN_ESGOTO_REDE_PUBLICA,
           #IN_LIXO_SERVICO_COLETA,
           #IN_BANHEIRO,
           IN_BANHEIRO_PNE,
           IN_BIBLIOTECA,
           #IN_COZINHA,
           #IN_LABORATORIO_CIENCIAS,
           #IN_LABORATORIO_INFORMATICA,
           IN_QUADRA_ESPORTES,
           #IN_SALA_DIRETORIA,
           IN_SALA_ATENDIMENTO_ESPECIAL,
           #IN_COMPUTADOR,
           #IN_EQUIP_COPIADORA,
           IN_EQUIP_MULTIMIDIA,
           #IN_DESKTOP_ALUNO,
           #IN_COMP_PORTATIL_ALUNO,
           #IN_INTERNET
    )
}


get_indice_de_infra_censo <- function(escola_censo_transf) {
  escola_censo_tri <- escola_censo_transf %>%
    select(contains("IN_"))
  
  fit <- ltm::ltm(escola_censo_tri ~ z1)
  zscores <- ltm::factor.scores(fit, response.patterns = escola_censo_tri %>% distinct()) %>%
    purrr::pluck(1) %>%
    rename(score_infra = z1)
  
  escola_censo_transf %>%
    left_join(zscores) %>%
    select(-c(Obs, Exp, se.z1, contains("IN_")))
}