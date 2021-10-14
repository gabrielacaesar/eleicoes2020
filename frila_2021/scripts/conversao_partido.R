corrigir_partidos <- function(tse_file){
  tse_file %>%
  mutate(SG_PARTIDO = case_when(SG_PARTIDO == "PC do B" ~ "PCdoB",
                                SG_PARTIDO == "PEN" ~ "PATRIOTA",
                                SG_PARTIDO == "PMDB" ~ "MDB",
                                SG_PARTIDO == "PPS" ~ "CIDADANIA",
                                SG_PARTIDO == "SD" ~ "SOLIDARIEDADE",
                                SG_PARTIDO == "PRB" ~ "REPUBLICANOS",
                                SG_PARTIDO == "PR" ~ "PL",
                                SG_PARTIDO == "PSDC" ~ "DC",
                                SG_PARTIDO == "PTN" ~ "PODE",
                                SG_PARTIDO == "PT do B" ~ "AVANTE",
                                TRUE ~ SG_PARTIDO))
}
