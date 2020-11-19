# leitura de pacotes
library(data.table)
library(tidyverse)

############
### 2020 ###
############

# leitura de arquivo
cand_2020_coe <- fread("C:/Users/acaesar/Downloads/resultado_candidatos-COE-19nov2020_n.csv",
                       encoding = "UTF-8")

### PREFEITO
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

############
### 2016 ###
############

setwd("C:/Users/acaesar/Downloads/populacao/")
class_columns <- c(cd_uf = "character", cd_ue = "character", pop_est = "character")

hab_2017 <- fread("habitantes_2017_municipios.csv", encoding = "Latin-1", colClasses = class_columns, stringsAsFactors = FALSE)

# leitura de arquivo / CONVERSOR
# url: https://github.com/betafcc/Municipios-Brasileiros-TSE/blob/master/municipios_brasileiros_tse.csv

cod_file <- fread("municipios_brasileiros_tse.csv", encoding = "UTF-8", sep = ",", colClasses = c(codigo_ibge = "character"))

# leitura de arquivos / RESULTADO 
setwd("C:/Users/acaesar/Downloads/resultado_eleicoes/")

resultado_2016 <- fread("votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.csv",
                        select = c("DS_ELEICAO", "NR_TURNO", "SG_UF", "NM_UE", "CD_MUNICIPIO", "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                                   "DS_SITUACAO_CANDIDATURA", "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS"))

resultado_2016_n <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_totais = sum(QT_VOTOS_NOMINAIS)) %>%
  mutate(NM_UE_n = abjutils::rm_accent(toupper(NM_UE)))

resultado_hab_2016 <- hab_2017 %>%
  filter(nm_ue != "Brasília" &
           nm_ue != "Fernando de Noronha") %>%
  mutate(nm_ue_n = abjutils::rm_accent(toupper(nm_ue)),
         cd_ue = str_pad(cd_ue, 5, pad = 0)) %>%
  unite(cd_ibge, c(cd_uf, cd_ue), sep = "") %>%
  left_join(cod_file, by = c("cd_ibge" = "codigo_ibge")) %>%
  left_join(resultado_2016_n, by = c("codigo_tse" = "CD_MUNICIPIO")) %>%
  arrange(desc(NM_UE_n)) 

#write.csv(resultado_hab_2016, "resultado_hab_2016.csv")

hab_partido_2016 <- resultado_hab_2016 %>%
  mutate(SG_PARTIDO = case_when(codigo_tse == "8931" ~ "PSD",
                                codigo_tse != "8931" ~ SG_PARTIDO)) %>%
  mutate(votos_totais = case_when(codigo_tse == "8931" ~ as.character("2317"),
                                  codigo_tse != "8931" ~ as.character(votos_totais))) %>%
  filter(!is.na(SG_PARTIDO)) %>%
  mutate(populacao = as.integer(pop_est)) %>%
  mutate(faixa = case_when(populacao > 500000 ~ "mais_500mil",
                           populacao >= 150001  & populacao <= 500000 ~ "entre_150_500mil",
                           populacao >= 50001 & populacao <= 150000 ~ "entre_50_150mil",
                           populacao >= 20001 & populacao <= 50000 ~ "entre_20_50mil",
                           populacao <= 20000 ~ "ate_20mil")) %>%
  group_by(SG_PARTIDO, faixa) %>%
  summarise(int = n()) %>%
  pivot_wider(names_from = faixa, values_from = int) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PEN", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PMDB", "MDB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPS", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRB", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PR$", "PL"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PSDC", "DC"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PTN", "PODE"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REDE", "Rede"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PT do B", "Avante")) %>%
  replace(is.na(.), 0) %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = T))



