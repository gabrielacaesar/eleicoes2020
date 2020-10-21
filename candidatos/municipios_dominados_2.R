# leitura de pacotes
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

# leitura de BRASIL - 2020
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_20out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

# leitura de BRASIL - 2016
cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

resultado_2016 <- fread("C:/Users/acaesar/Downloads/dados_20out2020/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.csv",
                        encoding = "Latin-1", colClasses = class_columns)

# 76 municipios
municipios_76 <- fread("C:/Users/acaesar/Downloads/dados_20out2020/76municipios.csv",
                       encoding = "UTF-8")

# candidatos 2020
cand_2020_n <- cand_2020 %>%
  select(SG_UE, DS_CARGO, SQ_CANDIDATO, NM_CANDIDATO, SG_PARTIDO, NR_CPF_CANDIDATO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  distinct(SG_UE, NM_CANDIDATO, .keep_all = TRUE)

colnames(cand_2020_n) <- paste(colnames(cand_2020_n), "2020", sep = "_")

# resultado 2016
cand_2016 <- cand_2016 %>%
  select(SQ_CANDIDATO, NR_CPF_CANDIDATO)

cand_2016_n <- resultado_2016 %>%
  select(NM_TIPO_ELEICAO, SG_UE, NM_UE, SG_UF, DS_CARGO, SG_PARTIDO, NM_CANDIDATO, SQ_CANDIDATO, 
         DS_DETALHE_SITUACAO_CAND, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  filter(NM_TIPO_ELEICAO == "Eleição Ordinária") %>%
  distinct(NM_UE, SG_UF, NM_CANDIDATO, .keep_all = TRUE) %>%
  left_join(cand_2016, by = "SQ_CANDIDATO") %>%
  select(SG_UE, SG_PARTIDO, NM_CANDIDATO, NR_CPF_CANDIDATO) %>%
  mutate(SG_UE = SG_UE)

colnames(cand_2016_n) <- paste(colnames(cand_2016_n), "2016", sep = "_")

# análise
## reeleição
t <- municipios_76 %>%
  select(-c(`ELEITO-2016`)) %>%
  left_join(cand_2016_n, by = c("SG_UE" = "SG_UE_2016")) %>%
  left_join(cand_2020_n, by = c("NR_CPF_CANDIDATO_2016" = "NR_CPF_CANDIDATO_2020")) %>%
  filter(!is.na(SG_UE_2020)) %>%
  mutate(SG_PARTIDO_2020 = str_replace_all(SG_PARTIDO_2020, "MDB", "PMDB")) %>%
  mutate(check_partido = SG_PARTIDO_2016 == SG_PARTIDO_2020)

## candidato único
g <- cand_2020 %>%
  filter(DS_CARGO == "PREFEITO") %>%
  group_by(SG_UE, NM_UE, SG_UF) %>%
  summarize(int = n()) %>%
  distinct(SG_UE, NM_UE, SG_UF, .keep_all = TRUE) %>%
  left_join(municipios_76, by = "SG_UE") %>%
  filter(!is.na(`SG_UF.y`))
  
## candidato mesmo partido
cand_2020_n2 <- cand_2020 %>%
  select(SG_UE, SG_PARTIDO, NM_CANDIDATO, DS_CARGO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "MDB", "PMDB"))

colnames(cand_2020_n2) <- paste(colnames(cand_2020_n2), "2020", sep = "_")

f <- municipios_76 %>%
  select(-c(`ELEITO-2016`)) %>%
  left_join(cand_2016_n, by = c("SG_UE" = "SG_UE_2016")) %>%
  left_join(cand_2020_n2, by = c("SG_UE" = "SG_UE_2020")) %>%
  mutate(check_partido = PARTIDO == SG_PARTIDO_2020) %>%
  #select(SG_PARTIDO_2016, SG_PARTIDO_2020, check_partido) %>%
  filter(check_partido == TRUE)



  