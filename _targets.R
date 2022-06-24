# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("readxl", "tidyr", "stringr", "dplyr", "magrittr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
source("./R/indicadores_inep.R")
source("./R/coelba.R")
source("./R/prosa.R")
source("./R/utils.R")
# source("other_functions.R") # Source other scripts as needed. # nolint

# Replace the target list below with your own:
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
  #tar_target(base_indicadores, create_base_indicadores(afd, atu, had, icg, ideb, inse, ied, ird, tdi)),
  #tar_target(export_indicadores_to_sql, export_to_sqlite("indicadores_inep", base_indicadores)),
  tar_target(coelba_path, "./edu-covid-data/data/projeto_prosa/raw/coelba/Salvador_Cativo_2018 vs 2022.xlsx", format = "file"),
  tar_target(pop_path, "./edu-covid-data/data/projeto_prosa/raw/bairros/populacao_bairros_salvador.xlsx", format = "file"),
  tar_target(coelba, get_coelba(coelba_path, pop_path)),
  tar_target(export_coelba_to_sql, export_to_sqlite("coelba", coelba)),
  tar_target(
    prosa_files,
    fs::dir_ls(
      c("./edu-covid-data/data/projeto_prosa/raw/prosa/2017",
        "./edu-covid-data/data/projeto_prosa/raw/prosa/2018",
        "./edu-covid-data/data/projeto_prosa/raw/prosa/2019",
        "./edu-covid-data/data/projeto_prosa/raw/prosa/2021")),
    format = "file"
  ),
  tar_target(prosa, purrr::map_dfr(prosa_files, ~ get_prosa(.x))),
  tar_target(export_prosa_to_sql, export_to_sqlite("prosa", prosa))
)
