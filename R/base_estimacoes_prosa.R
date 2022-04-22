library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(magrittr)

source("./R/helpers_maps.R")

conn <- DBI::dbConnect(RPostgres::Postgres(),
                       dbname = "educacao",
                       user = "postgres")

# Base coelba
# 2021 só tem os primeiros três meses

coelba <- readxl::read_excel("./edu-covid-data/data/projeto_prosa/processed/coelba/consumo_energia.xlsx") %>%
  separate(col = Data, into = c("nu_mes", "nu_ano"), sep = "\\.") %>%
  mutate(
    nm_bairro = case_when(
      Bairro %in% c("TANCREDO NEVES", "TANCREDO NEVES I") ~ "BEIRU/TANCREDO NEVES",
      Bairro %in% c("FAZENDA COUTOS I", "FAZENDA COUTOS III") ~ "FAZENDA COUTOS",
      Bairro %in% c("LOBATO - II", "LOBATO I") ~ "LOBATO",
      Bairro %in% c("PIRAJA - I") ~ "PIRAJA",
      Bairro %in% c("RIO VERMELHO - I", "RIO VERMELHO - II") ~ "RIO VERMELHO",
      Bairro %in% c("SAO CAETANO - I") ~ "SAO CAETANO",
      Bairro %in% c("SAO MARCOS - I") ~ "SAO MARCOS",
      Bairro %in% c("SUSSUARANA - I") ~ "SUSSUARANA - I",
      Bairro %in% c("VALERIA - I", "VALERIA - II") ~ "VALERIA",
      Bairro %in% c("VILA RUI BARBOSA", "JARDIM CRUZEIRO") ~ "VILA RUY BARBOSA/JADIM CRUZEIRO",
      TRUE ~ Bairro), 
    Classe = Classe %>%
      stringi::stri_trans_general(id = "Latin-ASCII") %>%
      str_to_lower(Classe) %>%
      str_replace(" ", "_"),
    across(nu_mes:nu_ano, ~ as.numeric(.x))) %>%
  group_by(nu_mes, nu_ano, nm_bairro, Classe) %>%
  summarise(KWH = sum(KWH, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = Classe, names_prefix = "kwh_", values_from = KWH) %>%
  group_by(nu_ano, nm_bairro) %>%
  summarise(across(kwh_comercial:kwh_consumo_proprio, ~ mean(.x, na.rm = TRUE))) %>%
  filter(str_starts(nm_bairro, "br/100000006351", negate = TRUE)) %>%
  inner_join(
    readxl::read_excel("./edu-covid-data/data/projeto_prosa/raw/bairros/populacao_bairros_salvador.xlsx") %>%
      select(-`n encontrados`)
  ) %>%
  mutate(
    across(contains("kwh"), ~ .x / pop)
  )


sep_coord <- function(coord) {
  str_coord <- as.character(coord)
  coord_antdec <- str_coord %>% str_sub(end = 3)
  coord_depdec <- str_coord %>% str_sub(start = 4)
  new_coord <- as.numeric(glue::glue("{coord_antdec}.{coord_depdec}"))
}


lista_de_escolas <- readxl::read_excel("./edu-covid-data/data/projeto_prosa/raw/inep/lista_de_escolas_ssa_municipais3.xlsx") %>%
  select(
    nm_escola = Escola,
    id_escola = `Código INEP`,
    nm_bairro,
    nm_localizacao = Localização,
    porte_escola = `Porte da Escola`,
    latitude = Latitude,
    longitude = Longitude
  ) %>%
  mutate(
    across(latitude:longitude,
           ~ sep_coord(.x))
  )

media_indicadores <- DBI::dbGetQuery(conn, "select * from media_escolas_indicadores")

base_completa <- media_indicadores %>%
  select(-c(nm_localizacao, nm_escola)) %>%
  inner_join(lista_de_escolas) %>%
  inner_join(coelba) # Cassange e Ilha de Bom jesus dos passos não tem na coelba. Sugestões na planilha de lista de escolas


readr::write_csv(base_completa, "./edu-covid-data/data/projeto_prosa/processed/prosa/base_prosa_coelba_inep.csv")
