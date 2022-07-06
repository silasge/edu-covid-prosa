library(magrittr)

#' Função usada para abrir os shapefiles de Salvador ou da Bahia
#' 
#' @param local string especificando o shapefile desejado. `ssa` se for Salvador, `ba` se for Bahia
#' 
#' @return um tibble com a geometria do shapefile desejado
#' 
#' @examples abrir_shp("ssa"); abrir_shp("ba")
#' 
#' @author Silas Genário \email{silasge.lopes@@gmail.com}
abrir_shp <- function(local = "ssa") {
  base_path <- glue::glue("./edu-covid-data/data/projeto_prosa/raw/shapefiles/{local}/")
  if (local == "ssa") {
    shp <- sf::read_sf(paste0(base_path, "limite_bairro_altbts.shp")) %>%
      sf::st_transform(crs = 4326) %>%
      #dplyr::rename(BAIRRO = NM_BAIRROS) %>%
      dplyr::mutate(NM_BAIRROS = stringr::str_to_upper(stringi::stri_trans_general(NM_BAIRROS, id = "Latin-ASCII")))
    
  }
  else if (local == "ba") {
    shp <- sf::read_sf(paste0(base_path, "29MUE250GC_SIR.shp"), options = "ENCODING=WINDOWS-1252") %>%
      dplyr::rename(MUNICIPIO = NM_MUNICIP) %>%
      dplyr::mutate(MUNICIPIO = stringi::stri_trans_general(MUNICIPIO, id = "Latin-ASCII"))
  }
}

trans_cep <- function(str) {
  first_part <- stringr::str_sub(str, 1, 5)
  last_part <- stringr::str_sub(str, 6, 8)
  cep <- stringr::str_c(first_part, "-", last_part)
}

concat_bairros <- function(str) {
  stringr::str_c("\\b", str, "\\b")
}

corr_bairro_ext <- function(brex) {
  if (length(brex) > 1) {
    return(brex[2])
  } else if (length(brex) == 0) {
    return(NA_character_)
  } else {
    return(brex)
  }
}

abrir_lista_de_ruas <- function(mun = NULL) {
  lista_ruas <- readr::read_csv("./edu-covid-data/data/projeto_prosa/raw/bairros/ceps.csv") %>%
    dplyr::mutate(CEP = trans_cep(as.character(CEP)))
  if (is.null(mun)) {
    return(lista_ruas)
  } else {
    lista_ruas <- lista_ruas %>%
      dplyr::filter(MUNICIPIO == mun)
    return(lista_ruas)
  }
}




#" Função que abre a lista de todas as escolas de Salvador ou da Bahia
abrir_lista_de_escolas <- function(local, dep_adm) {
  shp <- abrir_shp(local)
  bairros_shp <- shp$BAIRRO
  bairros_shp <- purrr::map_chr(bairros_shp, ~ concat_bairros(.x)) %>%
    stringr::str_c(collapse = "|")
  
  escolas <- readr::read_csv2(glue::glue("./edu-covid-data/data/projeto_prosa/raw/inep/lista_de_escolas_{local}.csv")) %>%
    dplyr::rename_all(~ stringr::str_to_upper(stringi::stri_trans_general(.x, id = "Latin-ASCII"))) %>%
    dplyr::select(
      ESCOLA,
      ID_INEP = `CODIGO INEP`,
      ID_UF = UF,
      MUNICIPIO,
      ID_LOCALIZACAO = LOCALIZACAO,
      ENDERECO,
      DEP_ADM = `DEPENDENCIA ADMINISTRATIVA`,
      PORTE_ESCOLA = `PORTE DA ESCOLA`,
      ETAPAS = `ETAPAS E MODALIDADE DE ENSINO OFERECIDAS`,
      LATITUDE,
      LONGITUDE
    ) %>%
    dplyr::filter(DEP_ADM == dep_adm) %>%
    dplyr::mutate(
      MUNICIPIO = stringr::str_to_upper(stringi::stri_trans_general(MUNICIPIO, id = "Latin-ASCII")),
      BAIRRO_EXT = purrr::map_chr(stringr::str_extract_all(ENDERECO, pattern = bairros_shp),
                                  corr_bairro_ext),
      CEP = stringr::str_extract(ENDERECO, pattern = "\\d{5}-\\d{3}")
    )
}

abrir_lista_de_escolas_ssa <- function(dep_adm = "Municipal") {
  lista_de_ruas <- abrir_lista_de_ruas("SALVADOR")
  bairros_shp <- tibble::tibble("BAIRRO" = abrir_shp("ssa")$BAIRRO)
  
  listas_ruas <- abrir_lista_de_ruas("SALVADOR") %>%
    dplyr::select(CEP, BAIRRO)
  
  lista_de_escolas <- abrir_lista_de_escolas("ssa", dep_adm = dep_adm) %>%
    dplyr::left_join(lista_de_ruas) %>%
    dplyr::rename(BAIRRO_CEP = BAIRRO) %>%
    dplyr::mutate(
      CONCORDA = (BAIRRO_EXT == BAIRRO_CEP),
      BAIRRO_EXT = dplyr::case_when(
        BAIRRO_EXT == "BOM JUA" & BAIRRO_CEP == "FAZENDA GRANDE DO RETIRO" ~ "FAZENDA GRANDE DO RETIRO",
        BAIRRO_EXT == "PRAIA GRANDE" & BAIRRO_CEP == "RIO SENA" ~ "RIO SENA",
        BAIRRO_EXT == "LAPINHA" & BAIRRO_CEP == "LIBERDADE" ~ "LIBERDADE",
        BAIRRO_EXT == "NOVA BRASILIA" & BAIRRO_CEP %in% c("ITAPUA", "NOVA BRASILIA DE ITAPUA") ~ "ITAPUA",
        BAIRRO_EXT == "BARREIRAS" & BAIRRO_CEP == "TANCREDO NEVES" ~ "BEIRU/TANCREDO NEVES",
        TRUE ~ BAIRRO_EXT)
    ) %>%
    dplyr::mutate(
      BAIRRO_EXT = dplyr::case_when(
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "VILA RUI BARBOSA" ~ "VILA RUY BARBOSA/JADIM CRUZEIRO",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "DOIS DE JULHO" ~ "CENTRO",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "TANCREDO NEVES" ~ "BEIRU/TANCREDO NEVES",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "ALTO DO PERU" ~ "ALTO DO PERU", # Olhar obs 1
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "CAIXA D'AGUA" ~ "CAIXA D'AGUA",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "ARMACAO" ~ "JARDIM ARMACAO",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "AMARALINA" ~ "NORDESTE DE AMARALINA",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "SANTA TERESA" ~ "BROTAS",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "DANIEL LISBOA" ~ "DANIEL LISBOA", # Olhar obs 2
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "NORDESTE" ~ "NORDESTE DE AMARALINA",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "JARDIM PLACAFORD" ~ "PIATA", # Olhar obs 3
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "PELOURINHO" ~ "CENTRO HISTORICO",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "PARQUE BELA VISTA" ~ "PARQUE BELA VISTA", # Obs 4
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "COUTOS" ~ "FAZENDA COUTOS",
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "CEASA" ~ "CEASA", # Obs 5
        is.na(BAIRRO_EXT) & BAIRRO_CEP == "ILHA AMARELA" ~ "ILHA AMARELA", # Obs 6
        is.na(BAIRRO_EXT) & stringr::str_detect(ESCOLA, "CHAME CHAME") ~ "CHAME-CHAME", # Obs 7
        TRUE ~ BAIRRO_EXT
      )
    )
}


sep_coord <- function(coord) {
  str_coord <- as.character(coord)
  coord_antdec <- str_coord %>% str_sub(end = 3)
  coord_depdec <- str_coord %>% str_sub(start = 4)
  new_coord <- as.numeric(glue::glue("{coord_antdec}.{coord_depdec}"))
  return(new_coord)
}
