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
        nm_bairro %in% c("AGUA DE MENINOS", "AGUA DE MENINOS - I") ~ "AGUA DE MENINOS",
        nm_bairro %in% c("JARDIM ARMACAO") ~ "ARMACAO",
        nm_bairro %in% c("SETE PORTAS", "SETE PORTAS - I") ~ "BARBALHO",
        nm_bairro %in% c("CALABAR") ~ "BARRA",
        nm_bairro %in% c("BARREIRO") ~ "BARREIRAS",
        nm_bairro %in% c("VISTA ALEGRE") ~ "BOA VIAGEM",
        nm_bairro %in% c("BOCA DO RIO", "BOCA DO RIO - I") ~ "BOCA DO RIO",
        nm_bairro %in% c("CAIXA D AGUA") ~ "CAIXA D'AGUA",
        nm_bairro %in% c("BAIXA DO FISCAL") ~ "CALCADA",
        nm_bairro %in% c("VALE DOS LAGOS") ~ "CASTELO BRANCO",
        nm_bairro %in% c("CENTRO-SALVADOR", "PRACA DA SE") ~ "CENTRO",
        nm_bairro %in% c("CAB") ~ "CENTRO ADMINISTRATIVO DA BAHIA",
        nm_bairro %in% c("ALTO DE COUTOS") ~ "COUTOS",
        nm_bairro %in% c("FAZENDA COUTOS I", "FAZENDA COUTOS III") ~ "FAZENDA COUTOS",
        nm_bairro %in% c("BOM JUA") ~ "FAZENDA GRANDE DO RETIRO",
        nm_bairro %in% c("ALTO DO SOBRADINHO") ~ "FEDERACAO",
        nm_bairro %in% c("CEPEL", "CONJUNTO CEPEL") ~ "CEASA",
        nm_bairro %in% c("CHAME CHAME") ~ "CHAME-CHAME",
        nm_bairro %in% c("RURAL-BOM JESUS DOS PASSOS") ~ "ILHA DE BOM JESUS DOS PASSOS",
        nm_bairro %in% c("LORETO", "PRAIA GRANDE - ILHA DE MARE") ~ "ILHA DE MARE",
        nm_bairro %in% c("COSTA DE FORA", "ILHA DOS FRADES", "PARAMANA", "PONTA DE NOSSA SENHORA DE GUADALUPE", "") ~ "ILHA DOS FRADES",
        nm_bairro %in% c("FAROL ITAPUA") ~ "ITAPUA",
        nm_bairro %in% c("PLAKAFORD") ~ "JARDIM PLACAFORD",
        nm_bairro %in% c("LAPINHA", "LARGO DO TANQUE", "SOLEDADE", "SIEIRO") ~ "LIBERDADE",
        nm_bairro %in% c("MONT SERRAT") ~ "MONTE SERRAT",
        nm_bairro %in% c("NORDESTE DE AMARALINA", "VALE DAS PEDRINHAS") ~ "NORDESTE",
        nm_bairro %in% c("PARIPE BARRAGEM") ~ "PARIPE",
        nm_bairro %in% c("PARALELA - I") ~ "PARALELA",
        nm_bairro %in% c("ITAPAGIPE", "JARDIM PITUBA") ~ "PITUBA",
        nm_bairro %in% c("CORSARIO") ~ "PITUACU",
        nm_bairro %in% c("JARDIM VILA VERDE") ~ "SAO CRISTOVAO",
        nm_bairro %in% c("SAO GONCALO DO RETIRO") ~ "SAO GONCALO",
        nm_bairro %in% c("COLINAS DE PITUACU", "SAO RAFAEL") ~ "SAO MARCOS",
        nm_bairro %in% c("PRAIA DO FLAMENGO") ~ "STELLA MARIS",
        nm_bairro %in% c("TANCREDO NEVES", "TANCREDO NEVES I", "ESTRADA DAS BARREIRAS") ~ "TANCREDO NEVES",
        nm_bairro %in% c("LOBATO - II", "LOBATO I") ~ "LOBATO",
        nm_bairro %in% c("PIRAJA - I", "SAO BARTOLOMEU") ~ "PIRAJA",
        nm_bairro %in% c("RIO VERMELHO - I", "RIO VERMELHO - II", "VILA MATOS") ~ "RIO VERMELHO",
        nm_bairro %in% c("SAO CAETANO - I") ~ "SAO CAETANO",
        nm_bairro %in% c("SAO MARCOS - I") ~ "SAO MARCOS",
        nm_bairro %in% c("SUSSUARANA - I") ~ "SUSSUARANA",
        nm_bairro %in% c("VALERIA - I", "VALERIA - II") ~ "VALERIA",
        nm_bairro %in% c("VILA RUI BARBOSA", "JARDIM CRUZEIRO") ~ "VILA RUY BARBOSA"
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
