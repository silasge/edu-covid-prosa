library(targets)

# Set target options:
tar_option_set(
  packages = c("readxl", "tidyr", "stringr", "dplyr", "magrittr"), # packages that your targets need to run
  format = "rds"
)


options(clustermq.scheduler = "multicore")

source("./R/indicadores_inep.R")
source("./R/coelba.R")
source("./R/prosa.R")
source("./R/censo.R")
source("./R/enderecos_cep.R")
source("./R/matriculas.R")
source("./R/utils.R")

list(
  tar_target(
    afd_paths, 
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/adequacao_formacao_decente", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    atu_paths, 
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/media_turmas", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    dsu_paths,
    fs::dir_ls(path = "./edu-covid-data/data/projeto_prosa/raw/inep/docentes_ensino_superior", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    had_paths,
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/media_horas_aula", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    icg_paths,
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/complexidade_gestao", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    ideb_paths,
    c("./edu-covid-data/data/projeto_prosa/raw/inep/ideb/divulgacao_anos_iniciais_escolas_2019.xlsx", "./edu-covid-data/data/projeto_prosa/raw/inep/ideb/divulgacao_anos_finais_escolas_2019.xlsx"), 
    format = "file"
  ),
  tar_target(
    inse_path, "./edu-covid-data/data/projeto_prosa/raw/inep/inse/INSE_2019_ESCOLAS.xlsx" , format = "file"),
  tar_target(
    ied_paths,
    fs::dir_ls(path = "./edu-covid-data/data/projeto_prosa/raw/inep/esforco_docente/", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    ird_paths,
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/regularidade_corpo_docente", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(
    tdi_paths,
    fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/inep/taxa_distorcao_idade_serie", glob = "*.xlsx"), 
    format = "file"
  ),
  tar_target(afd, purrr::map_dfr(afd_paths, ~ get_afd(.x, id_municipio == 2927408))),
  tar_target(atu, purrr::map_dfr(atu_paths, ~ get_atu(.x, id_municipio == 2927408))),
  tar_target(had, purrr::map_dfr(had_paths, ~ get_had(.x, id_municipio == 2927408))),
  tar_target(icg, purrr::map_dfr(icg_paths, ~ get_icg(.x, id_municipio == 2927408))),
  tar_target(ideb, purrr::map_dfr(ideb_paths, ~ get_ideb(.x, id_municipio == 2927408))),
  tar_target(inse, get_inse(inse_path, id_municipio == 2927408)),
  tar_target(ied, purrr::map_dfr(ied_paths, ~ get_ied(.x, id_municipio == 2927408))),
  tar_target(ird, purrr::map_dfr(ird_paths, ~ get_ird(.x, id_municipio == 2927408))),
  tar_target(tdi, purrr::map_dfr(tdi_paths, ~ get_tdi(.x, id_municipio == 2927408))),
  tar_target(base_indicadores, base_inep_indicadores(atu, afd, had, icg, ideb, inse, ied, ird, tdi)),
  #tar_target(coelba_path, "./edu-covid-data/data/projeto_prosa/raw/coelba/Salvador_Cativo_2018 vs 2022.xlsx", format = "file"),
  #tar_target(pop_path, "./edu-covid-data/data/projeto_prosa/raw/bairros/populacao_bairros_salvador.xlsx", format = "file"),
  #tar_target(coelba, get_coelba(coelba_path, pop_path)),
  tar_target(
    prosa_files,
    fs::dir_ls(
      c("./edu-covid-data/data/projeto_prosa/raw/prosa/2018",
        "./edu-covid-data/data/projeto_prosa/raw/prosa/2019",
        "./edu-covid-data/data/projeto_prosa/raw/prosa/2021")),
    format = "file"
  ),
  tar_target(prosa, purrr::map_dfr(prosa_files, ~ get_prosa(.x))),
  tar_target(path_censo, fs::dir_ls("./edu-covid-data/data/projeto_prosa/raw/censo", glob = "*.zip", recurse=TRUE), format = "file"),
  tar_target(censo, map_dfr(path_censo, ~ read_escola_censo(.x)) %>% get_indice_de_infra_censo()),
  tar_target(path_ceps, "./edu-covid-data/data/projeto_prosa/raw/enderecos/lista_de_ceps_salvador.csv", format = "file"),
  tar_target(enderecos, get_enderecos(path_ceps)),
  tar_target(path_matriculas, "./edu-covid-data/data/projeto_prosa/raw/matriculas/matriculados_1,6-9ano.xlsx", format = "file"),
  tar_target(matriculas, get_matriculas(path_matriculas, enderecos))
  
  
  #tar_target(export_prosa_to_sql, export_to_sqlite("prosa", prosa))
)
