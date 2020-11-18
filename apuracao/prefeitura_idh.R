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
                           idhm >= 0 & idhm <= 0.499 ~ "Muito baixo"))
  
resultado_2020_n <- resultado_2020 %>%
  filter(eleito == "TRUE") %>%
  filter(cargo == "Prefeito") %>%
  distinct(codigo_municipio_tse, .keep_all = TRUE)



