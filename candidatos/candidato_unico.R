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

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# definindo nomes para cabeçalho de DF de 2012, 2008, 2006, 2004 e 2000
names_columns <- c("DT_GERACAO", "HH_GERACAO", "ANO_ELEICAO", 
                   "NR_TURNO", "DS_ELEICAO", "SG_UF", 
                   "SG_UE", "NM_UE", "CD_CARGO", 
                   "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                   "NR_CANDIDATO", "NR_CPF_CANDIDATO", "NM_URNA_CANDIDATO", 
                   "CD_SITUACAO_CANDIDATURA", "DS_SITUACAO_CANDIDATURA", 
                   "NR_PARTIDO", "SG_PARTIDO", "NM_PARTIDO", 
                   "CD_LEGENDA", "SGL_LEGENDA", "DS_COMPOSICAO_COLIGACAO", 
                   "NM_COLIGACAO", "CD_OCUPACAO", "DS_OCUPACAO", 
                   "DT_NASCIMENTO", "NR_TITULO_ELEITORAL_CANDIDATO", 
                   "NR_IDADE_DATA_ELEICAO", "CD_GENERO", "DS_GENERO", 
                   "CD_GRAU_INSTRUCAO", "DS_GRAU_INSTRUCAO", 
                   "CD_ESTADO_CIVIL", "DS_ESTADO_CIVIL", 
                   "CD_NACIONALIDADE", "DS_NACIONALIDADE", 
                   "SG_UF_NASCIMENTO", "CD_MUNICIPIO_NASCIMENTO", 
                   "NM_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", 
                   "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO", "NM_EMAIL")


# leitura de todas as UFs - 2012
path_2012 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2012/"
cand_2012 <- map_df(paste0(path_2012, list.files(path_2012, pattern = "*txt")), fread, 
                    encoding = "Latin-1", col.names = names_columns, drop = drop_columns, colClasses = class_columns)

# importing CSV
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_28set2020/consulta_cand_2020_17h/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns)

cand_2016_BR <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns)

# eleitorado 2020
eleitorado_2020 <- fread("C:/Users/acaesar/Downloads/eleitorado/perfil_eleitorado_2020.csv", 
                         encoding = "Latin-1")

eleitorado_2020_n <- eleitorado_2020 %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  rename("SG_UE" = "CD_MUNICIPIO")

um_candidato_2020 <- cand_2020_BR %>%
  distinct(SQ_CANDIDATO, .keep_all = T) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  group_by(SG_UE, NM_UE, SG_UF) %>%
  summarize(int = n()) %>%
  left_join(eleitorado_2020_n, by = "SG_UE") %>%
  filter(int == 1) %>%
  select(-c(`SG_UF.y`, int, NM_MUNICIPIO))

um_candidato_2020_n <- um_candidato_2020 %>%
  left_join(cand_2020_BR, by = "SG_UE") %>%
  filter(DS_CARGO == "PREFEITO") %>%
  select(`SG_UF.x`, `NM_UE.x`, NM_URNA_CANDIDATO, SG_PARTIDO, total)
  
 write.csv(um_candidato_2020_n, "um_candidato_2020_n.csv")


um_candidato_2016 <- cand_2016_BR %>%
  distinct(SQ_CANDIDATO, .keep_all = T) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  group_by(SG_UE, NM_UE, SG_UF) %>%
  summarize(int = n()) %>%
  filter(int == 1) 

um_candidato_2012 <- cand_2012 %>%
  distinct(SQ_CANDIDATO, .keep_all = T) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  group_by(SG_UE, NM_UE, SG_UF) %>%
  summarize(int = n()) %>%
  filter(int == 1) 
