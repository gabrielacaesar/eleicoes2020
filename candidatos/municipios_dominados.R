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
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_12out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

# leitura de BRASIL - 2016
cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

# leitura de todas as UFs - 2012
path_2012 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2012/"
cand_2012 <- map_df(paste0(path_2012, list.files(path_2012, pattern = "*txt")), fread, 
                    encoding = "Latin-1", col.names = names_columns, drop = drop_columns, colClasses = class_columns)

# leitura de todas as UFs - 2008
path_2008 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2008/"
cand_2008 <- map_df(paste0(path_2008, list.files(path_2008, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                                                                CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                                                                CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                                                                CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                                                                CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                                                                CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))

# leitura de todas as UFs - 2004
path_2004 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2004/"
cand_2004 <- map_df(paste0(path_2004, list.files(path_2004, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                                                                CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                                                                CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                                                                CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                                                                CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                                                                CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))


# leitura de todas as UFs - 2000
path_2000 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2000/"
cand_2000 <- map_df(paste0(path_2000, list.files(path_2000, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                                                                CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                                                                CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                                                                CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                                                                CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                                                                CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))

# eleitorado 2020
eleitorado_2020 <- fread("C:/Users/acaesar/Downloads/eleitorado/perfil_eleitorado_2020.csv", 
                         encoding = "Latin-1")


##### 2000

cand_2000_n <- cand_2000 %>%
  select(SG_UE, NM_UE, SG_UF, DS_CARGO, SG_PARTIDO, NM_CANDIDATO, NR_CPF_CANDIDATO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SITUACAO_CANDIDATURA == "DEFERIDO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, SG_UF, SG_PARTIDO, NR_CPF_CANDIDATO)

colnames(cand_2000_n) <- paste(colnames(cand_2000_n), "2000", sep = "_") 

##### 2004

cand_2004_n <- cand_2004 %>%
  select(SG_UE, NM_UE, SG_UF, DS_CARGO, SG_PARTIDO, NM_CANDIDATO, NR_CPF_CANDIDATO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SITUACAO_CANDIDATURA == "DEFERIDO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, SG_UF, SG_PARTIDO, NR_CPF_CANDIDATO)

colnames(cand_2004_n) <- paste(colnames(cand_2004_n), "2004", sep = "_") 

##### 2008

cand_2008_n <- cand_2008 %>%
  select(SG_UE, NM_UE, SG_UF, DS_CARGO, SG_PARTIDO, NM_CANDIDATO, NR_CPF_CANDIDATO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SITUACAO_CANDIDATURA == "DEFERIDO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, SG_UF, SG_PARTIDO, NR_CPF_CANDIDATO)

colnames(cand_2008_n) <- paste(colnames(cand_2008_n), "2008", sep = "_") 

##### 2012

cand_2012_n <- cand_2012 %>%
  select(SG_UE, NM_UE, SG_UF, DS_CARGO, SG_PARTIDO, NM_CANDIDATO, NR_CPF_CANDIDATO, DS_SITUACAO_CANDIDATURA, DS_SIT_TOT_TURNO) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SITUACAO_CANDIDATURA == "DEFERIDO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(SG_UE, NM_UE, SG_UF, SG_PARTIDO, NR_CPF_CANDIDATO) %>%
  mutate(SG_UE = as.character(SG_UE))

colnames(cand_2012_n) <- paste(colnames(cand_2012_n), "2012", sep = "_")

##### 

cand_2000_2012 <- eleitorado_2020 %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  rename("SG_UE" = "CD_MUNICIPIO") %>%
  mutate(SG_UE = as.character(SG_UE)) %>%
  left_join(cand_2000_n, by = c("SG_UE" = "SG_UE_2000")) %>%
  left_join(cand_2004_n, by = c("SG_UE" = "SG_UE_2004")) %>%
  left_join(cand_2008_n, by = c("SG_UE" = "SG_UE_2008")) %>%
  left_join(cand_2012_n, by = c("SG_UE" = "SG_UE_2012")) %>%
  select(NM_MUNICIPIO, total, SG_PARTIDO_2000, SG_PARTIDO_2004, SG_PARTIDO_2008, SG_PARTIDO_2012) %>%
  mutate(check_partido = SG_PARTIDO_2000 == SG_PARTIDO_2004 
         & SG_PARTIDO_2004 == SG_PARTIDO_2008 
         & SG_PARTIDO_2008 == SG_PARTIDO_2012) %>%
  filter(check_partido == TRUE)












