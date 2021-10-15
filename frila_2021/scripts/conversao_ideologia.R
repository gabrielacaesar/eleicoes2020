definir_ideologia <- function(tse_file){
  esquerda <- c("PPL", "UP", "PSTU", "PCO", "PCB", "PSOL", "PCdoB", "PT")
  centro <- c("MDB", "PSDB", "PDT", "PSB", "REDE", "CIDADANIA", "PV", "PTB", "AVANTE", "SOLIDARIEDADE", "PMN")
  direita <- c("PHS", "PRP", "PMB", "PSD", "PODE", "PRTB", "PROS", "PL", "REPUBLICANOS", "PTC", "DC", "PSL", "NOVO", "PP", "PSC", "PATRIOTA", "DEM")
  tse_file %>%
    mutate(SG_IDEOLOGIA = case_when(SG_PARTIDO %in% esquerda ~ "Esquerda",
                                 SG_PARTIDO %in% centro ~ "Centro",
                                 SG_PARTIDO %in% direita ~ "Direita",
                                 SG_PARTIDO == "-" ~ "-",
                                 TRUE ~ "Sem classificação"))
}

