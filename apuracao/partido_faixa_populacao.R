# leitura de pacotes
library(data.table)
library(tidyverse)

# leitura de arquivo
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_19nov2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv")

cand_2020_coe <- fread("C:/Users/acaesar/Downloads/resultado_candidatos-COE-19nov2020_n.csv",
                       encoding = "UTF-8")

### PREFEITO
prefeito_2020 <- cand_2020 %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO")


partido_faixa <- cand_2020_coe %>%
  filter(eleito == "TRUE") %>%
  select(uf, nome_municipio, nome_candidato, cargo, sigla_partido, sigla_partido_vice, eleito, populacao) %>%
  mutate(faixa = case_when(populacao > 500000 ~ "mais_500mil",
         populacao >= 150001  & populacao <= 500000 ~ "entre_150_500mil",
         populacao >= 50001 & populacao <= 150000 ~ "entre_50_150mil",
         populacao >= 20001 & populacao <= 50000 ~ "entre_20_50mil",
         populacao <= 20000 ~ "ate_20mil")) %>%
  group_by(sigla_partido, faixa) %>%
  summarise(int = n()) %>%
  pivot_wider(names_from = faixa, values_from = int) %>%
  mutate(sigla_partido = str_replace_all(sigla_partido, "AVANTE", "Avante"),
         sigla_partido = str_replace_all(sigla_partido, "CIDADANIA", "Cidadania"),
         sigla_partido = str_replace_all(sigla_partido, "NOVO", "Novo"),
         sigla_partido = str_replace_all(sigla_partido, "PATRIOTA", "Patriota"),
         sigla_partido = str_replace_all(sigla_partido, "PODE", "Podemos"),
         sigla_partido = str_replace_all(sigla_partido, "PC do B", "PCdoB"),
         sigla_partido = str_replace_all(sigla_partido, "REDE", "Rede"),
         sigla_partido = str_replace_all(sigla_partido, "REPUBLICANOS", "Republicanos"),
         sigla_partido = str_replace_all(sigla_partido, "SOLIDARIEDADE", "SD")) %>%
  replace(is.na(.), 0) %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = T))

