library(readr)
library(dplyr)
library(stringr)

# ---- Função Principal ----

#' Função para obter os endereços a partir dos CEPS
#' 
#' @description 
#' A função lê o arquivo de endereços e deixa num formato pronto
#' 
#' @param path string com o caminho para o arquivo de endereços
get_enderecos <- function(path) {
  read_csv(path) %>%
    mutate(NM_BAIRRO = NM_BAIRRO %>% 
             str_to_upper() %>% 
             stringi::stri_trans_general(id="Latin-ASCII")) %>%
    select(ID_CEP, NM_BAIRRO, NU_LATI_RUA, NU_LONG_RUA)
}