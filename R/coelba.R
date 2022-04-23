library(readxl)
library(tidyr)
library(dplyr)
library(stringr)

.read_pop_bairros_ssa_2010 <- function(path) {
  read_excel(path) %>%
    select(-c(`n encontrados`, bairro), nu_pop = pop)
}

.read_coelba <- function(path) {
  read_excel(path) %>%
    filter(Bairro != "NÃO CADASTRADO") %>%
    rename(
      nu_ano = Ano,
      nu_mes = `Mês`,
      nm_bairro = Bairro,
      nm_classe = Classe,
      vl_kwh = KWH
    )
}

.clean_coelba <- function(coelba) {
  coelba %>%
    mutate(
      nm_bairro = case_when(
        nm_bairro %in% c("TANCREDO NEVES", "TANCREDO NEVES I") ~ "BEIRU/TANCREDO NEVES",
        nm_bairro %in% c("FAZENDA COUTOS I", "FAZENDA COUTOS III") ~ "FAZENDA COUTOS",
        nm_bairro %in% c("LOBATO - II", "LOBATO I") ~ "LOBATO",
        nm_bairro %in% c("PIRAJA - I") ~ "PIRAJA",
        nm_bairro %in% c("RIO VERMELHO - I", "RIO VERMELHO - II") ~ "RIO VERMELHO",
        nm_bairro %in% c("SAO CAETANO - I") ~ "SAO CAETANO",
        nm_bairro %in% c("SAO MARCOS - I") ~ "SAO MARCOS",
        nm_bairro %in% c("SUSSUARANA - I") ~ "SUSSUARANA - I",
        nm_bairro %in% c("VALERIA - I", "VALERIA - II") ~ "VALERIA",
        nm_bairro %in% c("VILA RUI BARBOSA", "JARDIM CRUZEIRO") ~ "VILA RUY BARBOSA/JADIM CRUZEIRO",
        TRUE ~ nm_bairro), 
      nm_classe = nm_classe %>%
        stringi::stri_trans_general(id = "Latin-ASCII") %>% 
        str_to_lower(nm_classe) %>%
        str_replace(" ", "_"),
    ) 
}

.group_add_pop_coelba_by_year <- function(coelba, pop_bairros) {
  coelba %>%
    group_by(nu_ano, nm_bairro, nm_classe) %>%
    summarise(
      vl_kwh_medio = mean(vl_kwh, na.rm = TRUE),
      vl_kwh_total = sum(vl_kwh, na.rm = TRUE)
    ) %>%
    left_join(pop_bairros) %>%
    mutate(
      vl_kwh_medio_pc = vl_kwh_medio / nu_pop,
      vl_kwh_total_pc = vl_kwh_total / nu_pop
    )
}

.pivot_coelba_by_classe <- function(grouped_coelba) {
  grouped_coelba %>%
    pivot_wider(names_from = nm_classe, names_prefix = "kwh_", values_from = contains("vl_kwh"))
}


get_coelba <- function(path_coelba, path_pop_bairros) {
  pop_bairros <- .read_pop_bairros_ssa_2010(path_pop_bairros)
  
  .read_coelba(path_coelba) %>%
    .clean_coelba() %>%
    .group_add_pop_coelba_by_year(pop_bairros) %>%
    .pivot_coelba_by_classe()
}
