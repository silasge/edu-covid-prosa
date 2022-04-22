library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(tidyr)


privadas <- read_excel("./data/raw/escolas_privadas_retorno.xlsx", na = "N/A") %>%
  mutate(BAIRRO = BAIRRO %>%
           str_to_lower() %>%
           stringi::stri_trans_general(id = "Latin-ASCII") %>%
           str_replace("canaria", "canario"),
         `RETORNO ESCALONADO?` = case_when(
           str_detect(`RETORNO ESCALONADO?`, "NÃO") ~ "NÃO",
           str_detect(`RETORNO ESCALONADO?`, "SIM|SERÁ") ~ "SIM"
         ))


tab_bairros_tipo_retorno <- privadas %>%
  group_by(BAIRRO, `TIPO RETORNO`) %>%
  summarise(freq = n()) %>%
  mutate(freq_rel = round((freq/sum(freq) * 100), 2)) %>%
  replace_na(list(BAIRRO = "N/A", `TIPO RETORNO` = "N/A")) %>%
  writexl::write_xlsx("tab_bairros_tipo_retorno.xlsx")


tab_bairros_retorno_escalonado <- privadas %>%
  group_by(BAIRRO, `RETORNO ESCALONADO?`) %>%
  summarise(freq = n()) %>%
  mutate(freq_rel = round((freq/sum(freq) * 100), 2)) %>%
  replace_na(list(BAIRRO = "N/A", `RETORNO ESCALONADO?` = "N/A")) %>%
  writexl::write_xlsx("tab_bairros_retorno_escalonado.xlsx")
 