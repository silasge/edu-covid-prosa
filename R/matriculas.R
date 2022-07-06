library(readr)
library(dplyr)
library(readxl)
library(glue)
library(lubridate)
library(stringr)
library(janitor)
library(geosphere)
library(purrr)


.read_matriculados <- function(path) {
  read_excel(path) %>%
    clean_names() %>%
    mutate(across(where(is.character), ~ if_else(.x == "", NA_character_, .x)),
           ano_esc = str_extract(ano_esc, "\\d") %>% as.numeric()) %>%
    rename(
      nu_ano = ano,
      nm_regional = gre,
      nm_escola = escola,
      nu_cep_escola = escola_cep,
      nm_bairro_escola = escola_bairro,
      nu_etapa = ano_esc,
      nm_turma = turma,
      nm_turno = turno,
      id_matricula = matricula,
      nm_aluno = aluno,
      dt_nascimento = dt_nasc,
      nu_idade = idade,
      nm_sexo = sexo,
      nm_cor = cor,
      nm_deficiencia = pd,
      nm_situacao_final_matricula = situacao_final,
      nu_cep_aluno = aluno_cep,
      nm_municipio_nasc = municipio_nasc,
      nm_mae = mae,
      nm_pai = pai,
      nm_responsavel = responsavel,
      nm_responsavel_escol = resp_escolaridade,
      nm_responsavel_parent = resp_parentesco
    )
}

.calc_situacao_matriculados <- function(matriculados) {
  matriculados %>%
    mutate(
      data_base_mes3 = glue("31/03/{nu_ano}") %>% dmy(),
      data_base_mes12 = glue("31/12/{nu_ano}") %>% dmy(),
      idade_adequada = nu_etapa + 5,
      ano_nascimento = year(dt_nascimento),
      mes_nascimento = month(dt_nascimento),
      ano_limite = nu_ano - (idade_adequada + 1),
      idade_aluno_mes3 = (interval(dt_nascimento, data_base_mes3) / years()) %>% floor(),
      situacao = case_when(
        (idade_aluno_mes3 <= idade_adequada) ~ "Regular", 
        (idade_aluno_mes3 == (idade_adequada + 1)) & (ano_nascimento == ano_limite) & (mes_nascimento >= 4) ~ "Regular",
        (idade_aluno_mes3 == (idade_adequada + 1)) & (ano_nascimento == ano_limite) & (mes_nascimento < 4) ~ "Atraso",
        (idade_aluno_mes3 >= (idade_adequada + 1)) & (ano_nascimento < ano_limite) ~ "Distorção"
      )
    )
}

.obter_vars_inep <- function(matriculados, afd, had, icg, ideb, ied, inse, ird) {
  matriculados %>% 
    left_join(had %>% distinct(nm_escola, id_escola)) %>% # obter id_inep pra cada escola
    left_join(afd %>% mutate(across(contains("adeq_docente_"), ~ .x / 100))) %>%
    left_join(had %>% mutate()) %>%
    left_join(icg) %>%
    left_join(ideb) %>%
    left_join(ied %>% mutate(across(contains("esforco_docente_"), ~ .x / 100))) %>%
    left_join(inse) %>%
    left_join(ird) #%>%
    #mutate(horas_aula = case_when(horas_aula == 4 ~ 1, horas_aula == 10 ~ 0),
    #       prof_lic_mesma_area = adeq_docente_af_grp1,
    #       prof_mesma_area = adeq_docente_af_grp1 + adeq_docente_af_grp2,
    #       prof_lic_bach = adeq_docente_af_grp1 + adeq_docente_af_grp2 + adeq_docente_af_grp3,
    #       esf_docente_nvls_1_a_3 = esforco_docente_af_nvl1 + esforco_docente_af_nvl2 + esforco_docente_af_nvl3,
    #       esf_docente_nvls_1_a_4 = esforco_docente_af_nvl1 + esforco_docente_af_nvl2 + esforco_docente_af_nvl3 + esforco_docente_af_nvl4,
    #       esf_docente_nvls_3_a_6 = esforco_docente_af_nvl3 + esforco_docente_af_nvl4 + esforco_docente_af_nvl5 + esforco_docente_af_nvl6)
}

.obter_censo_escola <- function(matriculados, censo_escola) {
  matriculados %>%
    left_join(censo_escola)
}

.criar_var_distancia <- function(matriculados, enderecos) {
  matriculados %>%
    select(-nm_bairro_escola) %>%
    mutate(across(c(nu_cep_escola, nu_cep_aluno), ~ as.numeric(.x))) %>%
    left_join(
      enderecos %>% 
        select(
          nu_cep_escola = ID_CEP,
          nm_bairro_escola = NM_BAIRRO,
          lati_escola = NU_LATI_RUA,
          long_escola = NU_LONG_RUA
        )
    ) %>%
    left_join(
      enderecos %>%
        select(
          nu_cep_aluno = ID_CEP,
          nm_bairro_aluno = NM_BAIRRO,
          lati_aluno = NU_LATI_RUA,
          long_aluno = NU_LONG_RUA
        )
    ) %>%
    mutate(
      geo_rua_aluno = map2(long_aluno, lati_aluno, ~ c(.x, .y)),
      geo_rua_escola = map2(long_escola, lati_escola, ~ c(.x, .y)),
      distancia = map2_dbl(.x = geo_rua_aluno, .y = geo_rua_escola, ~ distHaversine(.x, .y))
    )
}

.criar_dummies <- function(matriculados) {
  matriculados %>%
    mutate(
      masculino = case_when(nm_sexo == "MASCULINO" ~ 1,
                            nm_sexo == "FEMININO" ~ 0),
      negro = case_when(nm_cor %in% c("PRETO", "PARDO") ~ 1, 
                        nm_cor %in% c("AMARELO", "INDIGENA", "NÃO DECLARADO", "BRANCO") ~ 0),
      necessidade_especial = case_when(nm_deficiencia == "SIM" ~ 1, 
                                       nm_deficiencia == "NÃO" ~ 0),
      vespertino = case_when(nm_turno == "VESPERTINO" ~ 1,
                             nm_turno %in% c("MATUTINO", "INTEGRAL") ~ 0),
      integral = case_when(nm_turno == "INTEGRAL" ~ 1,
                           nm_turno %in% c("MATUTINO", "VESPERTINO") ~ 0),
      ate_fundamental_resp = case_when(str_detect(nm_responsavel_escol, "FUNDAMENTAL") ~ 1, 
                                       str_detect(nm_responsavel_escol, "MEDIO|SUPERIOR") ~ 0)
    )
}


.calc_tam_turma <- function(matriculados) {
  tam_turma <- matriculados %>%
    group_by(nu_ano, nm_escola, nu_etapa, nm_turma) %>%
    summarise(tam_turma = n(),
              porc_turma_em_dist = mean(situacao == "Distorção", na.rm = TRUE))
  
  matriculados %>% left_join(tam_turma)
} 

matriculas <- .read_matriculados("./edu-covid-data/data/projeto_prosa/raw/matriculas/matriculados_1,6-9ano.xlsx") %>%
  .calc_situacao_matriculados() %>%
  .obter_vars_inep(afd, had, icg, ideb, ied, inse, ird) %>%
  .obter_censo_escola(censo) %>%
  .criar_var_distancia(enderecos) %>%
  .calc_tam_turma()

matriculados <- matriculas %>%
  left_join(prosa %>% 
              filter(nu_ano != 2017) %>%
              mutate(nm_disciplina = case_when(
                nm_disciplina == "LÍNGUA PORTUGUESA" ~ "prosa_lp",
                nm_disciplina == "MATEMÁTICA"
              )) %>%
              select(nu_ano, id_matricula, id_escola, vl_proficiencia))

get_matriculas <- function(path_matriculas, afd, had, icg, ideb, ied, inse, ird, censo_escola, enderecos, anos_esc) {
  .read_matriculados(path_matriculas) %>%
    .calc_situacao_matriculados() %>%
    .obter_vars_inep(afd, had, icg, ideb, ied, inse, ird) %>%
    .obter_censo_escola(censo_escola) %>%
    .criar_var_distancia(enderecos) %>%
    .criar_dummies() %>%
    .calc_tam_turma() %>%
    filter(ano_esc %in% anos_esc) %>%
    select(
      ano, 
      gre,
      escola,
      id_inep,
      ano_esc,
      idade,
      diff_idade_adequada,
      situacao,
      situacao_final_matricula = situacao_final,
      tam_turma,
      porc_turma_em_dist,
      contains("prof_"),
      contains("adeq_docente_"),
      horas_aula,
      complexidade_gestao,
      ideb,
      contains("esf_docente_"),
      contains("esforco_docente_"),
      inse,
      regularidade_docente,
      score_infra,
      distancia,
      masculino,
      negro,
      necessidade_especial,
      vespertino,
      integral,
      ate_fundamental_resp,
      lati_escola,
      long_escola
    )
}


get_grouped_matriculas <- function(banco_matriculas){
  grp_ano_esc <- banco_matriculas %>%
    group_by(ano, gre, id_inep, escola, ano_esc) %>%
    summarise(
      n_alunos = n(),
      n_alunos_em_dist = sum(situacao == "Distorção", na.rm = TRUE),
      taxa_dist = mean(situacao == "Distorção", na.rm = TRUE),
      across(c(idade:diff_idade_adequada, 
               tam_turma:porc_turma_em_dist,
               contains("prof_"),
               contains("adeq_docente_"),
               horas_aula:ideb,
               contains("esf_docente_"),
               contains("esforco_docente_"),
               inse:long_escola), ~ mean(.x, na.rm = TRUE))
    )
  
  grp_anos_finais <- banco_matriculas %>%
    group_by(ano, gre, id_inep, escola) %>%
    summarise(
      n_alunos = n(),
      n_alunos_em_dist = sum(situacao == "Distorção", na.rm = TRUE),
      taxa_dist = mean(situacao == "Distorção", na.rm = TRUE),
      across(c(idade:diff_idade_adequada, 
               tam_turma:porc_turma_em_dist,
               contains("prof_"),
               contains("adeq_docente_"),
               horas_aula:ideb,
               contains("esf_docente_"),
               contains("esforco_docente_"),
               inse:long_escola), ~ mean(.x, na.rm = TRUE))
    ) %>%
    mutate(ano_esc = "Anos Finais")
  
  matriculas_grp <- bind_rows(grp_ano_esc, grp_anos_finais)
  
  matric_tx_meninas <- banco_matriculas %>%
    filter(masculino == 0) %>%
    group_by(id_inep, escola, ano_esc) %>%
    summarise(
      tx_dist_meninas = mean(situacao == "Distorção", na.rm = TRUE)
    ) %>%
    bind_rows(banco_matriculas %>%
                filter(masculino == 0) %>%
                group_by(id_inep, escola) %>%
                summarise(
                  tx_dist_meninas = mean(situacao == "Distorção", na.rm = TRUE)) %>%
                mutate(ano_esc = "Anos Finais"))
  
  matric_tx_pd <- banco_matriculas %>%
    filter(necessidade_especial == 1) %>%
    group_by(id_inep, escola, ano_esc) %>%
    summarise(
      tx_dist_pd = mean(situacao == "Distorção", na.rm = TRUE)
    ) %>%
    bind_rows(
      banco_matriculas %>%
        filter(necessidade_especial == 1) %>%
        group_by(id_inep, escola) %>%
        summarise(
          tx_dist_pd = mean(situacao == "Distorção", na.rm = TRUE)
        ) %>%
        mutate(ano_esc = "Anos Finais")
    )
  
  matric_td_15anos <- banco_matriculas %>%
    filter(situacao == "Distorção") %>%
    group_by(id_inep, escola, ano_esc) %>%
    summarise(
      tx_dist_idade = mean(idade >= 15, na.rm = TRUE)
    ) %>%
    bind_rows(
      banco_matriculas %>%
        filter(situacao == "Distorção") %>%
        group_by(id_inep, escola) %>%
        summarise(
          tx_dist_idade = mean(idade >= 15, na.rm = TRUE)
        ) %>%
        mutate(ano_esc = "Anos Finais")
    )
  
  matriculas_grp %>%
    left_join(matric_tx_meninas) %>%
    left_join(matric_td_15anos) %>%
    left_join(matric_tx_pd)
}


