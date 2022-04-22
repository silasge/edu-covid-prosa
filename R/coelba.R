library(readxl)
library(tidyr)
library(dplyr)
library(stringr)


coelba <- read_excel("./edu-covid-data/data/projeto_prosa/raw/coelba/Salvador_Cativo_2018 vs 2022.xlsx") %>%
  filter(Bairro != "NÃO CADASTRADO") %>%
  mutate(
    Classe = Classe %>% str_replace_all(" ", "_") %>% str_to_lower(),
    Bairro_Padronizado = case_when(
      Bairro == "ACUPE DE BROTAS" ~ "ACUPE",
      str_detect(Bairro, "AEROPORTO") ~ "AEROPORTO",
      Bairro == "ESTRADA DAS BARREIRAS" ~ "BARREIRAS",
      str_detect(Bairro, "TANCREDO NEVES") ~ "TANCREDO NEVES",
      Bairro %in% c("BOA VIAGEM", "ITAPAGIPE") ~ "BOA VIAGEM",
      Bairro %in% c("BROTAS", "CAMPINAS DE BROTAS", "DANIEL LISBOA", "PARQUE BELA VISTA", "SANTA TERESA") ~ "BROTAS",
      Bairro %in% c("CABULA", "BARROS REIS") ~ "CABULA",
      Bairro %in% c("CAJAZEIRAS", "CAJAZEIRAS II") ~ "CAJAZEIRAS",
      Bairro %in% c("CAJAZEIRAS IV", "CAJAZEIRAS III") ~ "CAJAZEIRAS IV",
      Bairro %in% c("CANELA", "CAMPO GRANDE") ~ "CANELA",
      Bairro %in% c("CENTRO-SALVADOR", "POLITEAMA", "BARROQUINHA") ~ "CENTRO",
      Bairro == "CAB" ~ "CENTRO ADMINISTRATIVO DA BAHIA",
      Bairro == "PELOURINHO" ~ "CENTRO HISTORICO",
      Bairro %in% c("COMERCIO", "AGUA DE MENINOS", "AGUA DE MENINOS - I", "BAIXA DOS SAPATEIROS", "SETE PORTAS", "SETE PORTAS - I") ~ "COMERCIO",
      Bairro %in% c("COUTOS", "ALTO DE COUTOS") ~ "COUTOS",
      Bairro %in% c("FAZENDA COUTOS", "FAZENDA COUTOS I", "FAZENDA COUTOS III") ~ "FAZENDA COUTOS",
      Bairro %in% c("FAZENDA GRANDE DO RETIRO", "SAN MARTIM") ~ "FAZENDA GRANDE DO RETIRO",
      Bairro == "BOM JESUS DOS PASSOS" ~ "ILHA DE BOM JESUS DOS PASSOS",
      Bairro %in% c("ILHA DOS FRADES", "LORETO", "COSTA DE FORA", "PARAMANA", "PONTA DE NOSSA SENHORA DE GUADALUPE") ~ "ILHA DOS FRADES/ILHA DE SANTO ANTONIO",
      Bairro %in% c("ITAPUA", "NOVA BRASILIA DE ITAPUA", "PLAKAFORD") ~ "ITAPUA",
      Bairro %in% c("LIBERDADE", "ALTO DO PERU", "LARGO DO TANQUE") ~ "LIBERDADE",
      Bairro %in% c("LOBATO", "LOBATO - II", "LOBATO I", "BAIXA DO FISCAL", "BELA VISTA DO LOBATO") ~ "LOBATO",
      Bairro == "MONT SERRAT" ~ "MONTE SERRAT",
      Bairro %in% c("MUSSURUNGA", "COLINAS MUSSURUNGA") ~ "MUSSURUNGA",
      Bairro %in% c("ONDINA", "JARDIM APIPEMA") ~ "ONDINA",
      Bairro %in% c("PATAMARES", "ALPHAVILLE I", "JAGUARIBE", "PARALELA", "PARALELA - I") ~ "PATAMARES",
      Bairro %in% c("PERNAMBUES", "JARDIM BRASILIA") ~ "PERNAMBUES",
      Bairro %in% c("PIRAJA", "PIRAJA - I", "SAO BARTOLOMEU") ~ "PIRAJA",
      Bairro %in% c("PITUACU", "COLINAS DE PITUACU", "CORSARIO") ~ "PITUACU",
      Bairro %in% c("PITUBA", "JARDIM PITUBA") ~ "PITUBA",
      Bairro %in% c("PLATAFORMA", "ESCADA") ~ "PLATAFORMA",
      Bairro %in% c("PRAIA GRANDE", "PRAIA GRANDE - ILHA DE MARE") ~ "PRAIA GRANDE",
      Bairro %in% c("RETIRO", "RETIRO - I", "RETIRO - II") ~ "RETIRO",
      Bairro %in% c("RIO VERMELHO", "RIO VERMELHO - I", "RIO VERMELHO - II", "CEASA", "VILA MATOS") ~ "RIO VERMELHO",
      Bairro %in% c("SAO CAETANO", "SAO CAETANO - I") ~ "SAO CAETANO",
      Bairro %in% c("SAO CRISTOVAO", "CEPEL", "JARDIM VILA VERDE") ~ "SAO CRISTOVAO",
      Bairro == "SAO GONCALO DO RETIRO" ~ "SAO GONCALO",
      Bairro %in% c("SAO MARCOS", "SAO MARCOS - I") ~ "SAO MARCOS",
      Bairro == "SAO TOME DE PARIPE" ~ "SAO TOME",
      Bairro %in% c("STELLA MARIS", "PRAIA DO FLAMENGO") ~ "STELLA MARIS",
      Bairro %in% c("SUSSUARANA", "SUSSUARANA - I") ~ "SUSSUARANA",
      Bairro %in% c("TROBOGY", "VILA 2 DE JULHO") ~ "TROBOGY",
      Bairro %in% c("VALERIA", "VALERIA - I", "VALERIA - II", "NOVA BRASILIA DE VALERIA") ~ "VALERIA",
      Bairro %in% c("VILA RUI BARBOSA", "JARDIM CRUZEIRO") ~ "VILA RUI BARBOSA/JARDIM CRUZEIRO",
      TRUE ~ Bairro
    )
  ) %>%
  pivot_wider(names_from = Classe, values_from = KWH, names_prefix = "kwh_")


writexl::write_xlsx(coelba, "./edu-covid-data/data/projeto_prosa/intermed/coelba/salvador_consumo_kwh_2018-2022.xlsx")
