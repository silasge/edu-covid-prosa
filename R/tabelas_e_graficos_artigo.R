library(readr)
library(dplyr)
library(stringr)


base_prosa <- read_csv("./edu-covid-data/data/projeto_prosa/processed/prosa/base_prosa_coelba_inep.csv") %>%
  filter(id_etapa %in% c(5, 9)) %>%
  mutate(
    edicao = glue::glue("{nu_ano}-{nu_avaliacao}"),
    nm_regional = nm_regional %>%
      str_replace("CIDADE BAIXA", "LIBERDADE"),
    porte_escola = porte_escola %>%
      str_replace_all("matrículas de escolarização", "") %>%
      str_trim()
  ) %>%
  select(
    edicao,
    nm_regional,
    id_escola,
    id_etapa,
    nm_disciplina,
    nu_alunos,
    vl_nota_media,
    atu:icg,
    ied:tdi,
    nm_localizacao,
    porte_escola,
    kwh_comercial, 
    kwh_industrial, 
    kwh_residencial, 
    kwh_resultado
  )


tabela1.1 <- base_prosa %>%
  filter(nm_disciplina == "MATEMÁTICA") %>%
  group_by(edicao, id_etapa, nm_disciplina) %>%
  summarise(n_de_alunos = sum(nu_alunos, na.rm = TRUE),
            n_de_escolas = n_distinct(id_escola))

