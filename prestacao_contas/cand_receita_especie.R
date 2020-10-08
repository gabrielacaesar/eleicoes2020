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

# importing CSV - candidatos
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_8out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns)

# importing CSV - prestação de contas / ÓRGÃOS PARTIDÁRIOS
receitas_orgaos_partidarios <- fread("C:/Users/acaesar/Downloads/dados_8out2020/prestacao_de_contas_eleitorais_orgaos_partidarios_2020/receitas_orgaos_partidarios_2020_BRASIL.csv",
                                     encoding = "Latin-1")

# importing CSV - prestação de contas / CANDIDATOS
receitas_candidatos <- fread("C:/Users/acaesar/Downloads/dados_8out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                             encoding = "Latin-1")

receitas_candidatos_n <- receitas_candidatos %>%
  mutate(VR_RECEITA = str_replace_all(VR_RECEITA, "\\,", ".")) %>%
  group_by(SQ_CANDIDATO, NR_CPF_CNPJ_DOADOR, DS_ESPECIE_RECEITA) %>%
  summarise(int = sum(as.double(VR_RECEITA))) %>%
  left_join(cand_2020_BR, by = "SQ_CANDIDATO") %>%
  filter(DS_ESPECIE_RECEITA == "Depósito em espécie" |
         DS_ESPECIE_RECEITA == "Em espécie") %>%
  filter(int > 1604.10) 

