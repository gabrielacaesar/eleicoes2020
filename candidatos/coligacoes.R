# reading libraries
library(tidyverse)
library(data.table)

# colunas para eliminar
drop_columns <- c("DT_GERACAO", "HH_GERACAO", "CD_TIPO_ELEICAO", 
                  "NM_TIPO_ELEICAO", "CD_ELEICAO", 
                  "DT_ELEICAO", "TP_ABRANGENCIA", "CD_CARGO", 
                  "CD_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", 
                  "NR_PARTIDO", "NM_PARTIDO", "NM_SOCIAL_CANDIDATO", "CD_NACIONALIDADE", 
                  "CD_MUNICIPIO_NASCIMENTO", "CD_GRAU_INSTRUCAO", 
                  "CD_ESTADO_CIVIL", "CD_COR_RACA", "CD_OCUPACAO", 
                  "CD_SIT_TOT_TURNO", "CD_SITUACAO_CANDIDATO_PLEITO", 
                  "CD_SITUACAO_CANDIDATO_URNA", "DS_NACIONALIDADE", 
                  "SG_UF_NASCIMENTO", "NM_MUNICIPIO_NASCIMENTO", 
                  "DT_NASCIMENTO", "NR_IDADE_DATA_POSSE", "DS_GRAU_INSTRUCAO",
                  "DS_ESTADO_CIVIL", "DS_OCUPACAO", "DS_SIT_TOT_TURNO",
                  "NR_PROTOCOLO_CANDIDATURA", "DS_SITUACAO_CANDIDATO_PLEITO",
                  "DS_SITUACAO_CANDIDATO_URNA", "ST_CANDIDATO_INSERIDO_URNA",
                  "VR_DESPESA_MAX_CAMPANHA", "NR_PROCESSO", "TP_AGREMIACAO")

# importing CSV
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_28set2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns)

cand_2016_BR <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns)

freq_coligacoes <- cand_2016_BR %>%
  filter(DS_CARGO == "PREFEITO") %>%
  group_by(DS_COMPOSICAO_COLIGACAO) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

most_freq_coligacoes <- freq_coligacoes %>%
  separate(DS_COMPOSICAO_COLIGACAO, c("1", "2", "3", "4", "5", "6", "7", 
                                      "8", "9", "10", "11", "12", "13", "14", 
                                      "15", "16", "17", "18", "19", "20"), sep = "/")



PT_coligacoes <- cand_2020_BR %>%
  filter(str_detect(DS_COMPOSICAO_COLIGACAO, "PT ")) %>%
  group_by(DS_COMPOSICAO_COLIGACAO) %>%
  summarise(int = n())
