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
           ano_esc = str_extract(ano_esc, "\\d") %>% as.numeric(),
           matricula = matricula %>%
             as.character() %>%
             str_pad(width = 7, side = "left", pad = "0")) %>%
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

get_matriculas <- function(path_matriculas, enderecos) {
  .read_matriculados(path_matriculas) %>%
    .calc_situacao_matriculados() %>%
    .criar_var_distancia(enderecos) %>%
    .criar_dummies() %>%
    .calc_tam_turma() %>%
    select(
      nu_ano, 
      nm_regional,
      nm_escola,
      id_matricula,
      nu_etapa,
      nu_idade,
      situacao,
      tam_turma,
      porc_turma_em_dist,
      distancia,
      masculino,
      negro,
      necessidade_especial,
      vespertino,
      integral,
      ate_fundamental_resp,
      distancia
    )
}


teste <- matriculas %>%
  left_join(base_indicadores %>% distinct(id_escola, nm_escola)) %>%
  left_join(base_indicadores) %>%
  left_join(censo)

prosa_piv <- prosa %>%
  pivot_wider(names_from = nm_disciplina, values_from = vl_proficiencia)

teste2 <- teste %>%
  left_join(prosa %>%
              )
