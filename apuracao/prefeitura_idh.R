library(data.table)
library(tidyverse)

# download de dados 
# Atlas do Desenvolvimento Humano no Brasil. Pnud Brasil, Ipea e FJP, 2020.
# http://www.atlasbrasil.org.br/ranking

resultado_2020 <- fread("C:/Users/acaesar/Downloads/resultado_candidatos-COE-18nov2020.csv",
                        encoding = "UTF-8")

idh <- fread("C:/Users/acaesar/Downloads/data-atlas-brasil.csv")

idh_tidy <- idh %>%
  janitor::clean_names() %>%
  separate(territorialidade, c("cidade", "uf"), sep = "\\(") %>%
  mutate(cidade_n = abjutils::rm_accent(toupper(str_trim(cidade)))) %>%
  mutate(uf = str_remove_all(uf, "\\)")) %>%
  relocate(cidade_n, after = cidade) %>%
  select("cidade_n", "cidade", "uf", "posicao_idhm", "idhm") %>%
  mutate(idhm = str_replace_all(idhm, "\\,", ".")) %>%
  mutate(faixa = case_when(idhm >= 0.8 & idhm <= 1.000 ~ "Muito alto",
                           idhm >= 0.700  & idhm <= 0.799 ~ "Alto",
                           idhm >= 0.600 & idhm <= 0.699 ~ "Médio",
                           idhm >= 0.500 & idhm <= 0.599 ~ "Baixo",
                           idhm >= 0 & idhm <= 0.499 ~ "Muito baixo")) %>%
  filter(cidade_n != "BRASILIA" &
           cidade_n != "FERNANDO DE NORONHA" &
           cidade_n != "MACAPA")

resultado_2020_n <- resultado_2020 %>%
  filter(eleito == "TRUE") %>%
  distinct(codigo_municipio_ibge, .keep_all = TRUE) %>%
  mutate(cidade_n = abjutils::rm_accent(toupper(str_trim(nome_municipio)))) %>%
  select(nome_municipio, cidade_n, uf, cargo, nome_candidato, sigla_partido)
  
joined_data <- idh_tidy %>%
  left_join(resultado_2020_n, by = c("cidade_n" = "cidade_n",
                                     "uf" = "uf"))

