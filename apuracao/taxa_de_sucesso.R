# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos
keep_columns <- c("NR_TURNO", "DS_ELEICAO", "SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "DS_SITUACAO_CANDIDATURA", "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", "DS_SITUACAO_CANDIDATO_PLEITO")

cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv", select = keep_columns)

cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_3nov2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", select = keep_columns)

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
  group_by(SG_PARTIDO, SG_UF) %>%
  summarise(int = n())

write.csv(cand_2016_n, "cand_2016_n.csv")

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
  summarise(int = n())
  
write.csv(cand_2020_n, "cand_2020_n.csv")

  
  
