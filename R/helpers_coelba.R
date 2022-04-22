library(magrittr)

#' Função usada para agregar os dados de consumo de energia em salvador
#' 
#' @param path caminho para o arquivo consumo_energia_salvador_bairro.xlsx
#' 
#' @return o arquivo coelba.xlsx já processado
#' 
#' @author Silas Genário \email{silasge.lopes@@gmail.com}

read_consumo_energia <- function(path = "./edu-covid-data/data/projeto_prosa/raw/coelba/consumo_energia_salvador_bairro.xlsx") {
  sheets <- readxl::excel_sheets(path)
  
  list_of_sheets <- purrr::map_dfr(
    sheets,
    ~ readxl::read_excel(path, sheet = .x, skip = 6) %>%
      dplyr::select( # Não foram selecionadas a quantidade de instalações, o código do bairro e o nome/código da cidade
        Bairro = ...4,
        Classe,
        KWH
      ) %>%
      dplyr::mutate(
        Data = .x
      ) %>%
      dplyr::relocate(Data, .before = Bairro) %>%
      dplyr::filter(!is.na(Bairro))
  ) %>%
    writexl::write_xlsx("./edu-covid-data/data/projeto_prosa/processed/coelba.xlsx")
}