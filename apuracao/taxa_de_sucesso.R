# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos
keep_columns <- c("NR_TURNO", "DS_ELEICAO", "SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "DS_SITUACAO_CANDIDATURA", "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", "DS_SITUACAO_CANDIDATO_PLEITO")

cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv", select = keep_columns)

cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_3nov2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", select = keep_columns)

saldo_pref_UF <- fread("C:/Users/acaesar/Downloads/saldo-prefeitura-UF-TOTAL.csv")

# 2016 / candidatos

cand_2016_n <- cand_2016 %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_ELEICAO == "Eleições Municipais 2016") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO" |
           DS_DETALHE_SITUACAO_CAND == "DEFERIDO COM RECURSO" |
           DS_DETALHE_SITUACAO_CAND == "PENDENTE DE JULGAMENTO" |
           DS_DETALHE_SITUACAO_CAND == "AGUARDANDO JULGAMENTO") %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE) %>%
  group_by(SG_PARTIDO) %>%
  summarise(candidatos = n()) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PEN", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PMDB", "MDB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPS", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRB", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PR$", "PL"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PSDC", "DC"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PTN", "PODE"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REDE", "Rede"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PT do B", "Avante"))

eleitos_2016 <- saldo_pref_UF %>%
  janitor::clean_names() %>%
  filter(ano == "2016") %>%
  group_by(partido) %>%
  summarise(eleitos = sum(prefeituras))

tx_sucesso_2016 <- cand_2016_n %>%
  left_join(eleitos_2016, by = c("SG_PARTIDO" = "partido")) %>%
  replace(is.na(.), 0) %>%
  mutate(tx_sucesso = round((eleitos / candidatos), 2) * 100)

write.csv(tx_sucesso_2016, "tx_sucesso_2016.csv")

# 2020 / candidatos

cand_2020_n <- cand_2020 %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_ELEICAO == "Eleições Municipais 2020") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO" |
           DS_DETALHE_SITUACAO_CAND == "DEFERIDO COM RECURSO" |
           DS_DETALHE_SITUACAO_CAND == "PENDENTE DE JULGAMENTO" |
           DS_DETALHE_SITUACAO_CAND == "AGUARDANDO JULGAMENTO") %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE) %>%
  group_by(SG_PARTIDO, SG_UF) %>%
  summarise(candidatos = n()) %>%
  mutate(ano = 2020) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "AVANTE", "Avante"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "CIDADANIA", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "NOVO", "Novo"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PATRIOTA", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REDE", "Rede"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REPUBLICANOS", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "SOLIDARIEDADE", "SD"))

tx_sucesso_2020 <- cand_2020_n %>%
  left_join(saldo_pref_UF, by = c("ano" = "Ano",
                                  "SG_PARTIDO" = "Partido",
                                  "SG_UF" = "UF")) %>%
  replace(is.na(.), 0) %>%
  rename(eleitos = "Prefeituras") %>%
  mutate(tx_sucesso = round((eleitos / candidatos), 2) * 100)


write.csv(tx_sucesso_2020, "tx_sucesso_2020.csv")
