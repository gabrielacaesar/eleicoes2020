library(tidyverse)
library(data.table)

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

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
                  "DS_ESTADO_CIVIL", "DS_OCUPACAO",
                  "NR_PROTOCOLO_CANDIDATURA", "DS_SITUACAO_CANDIDATO_PLEITO",
                  "DS_SITUACAO_CANDIDATO_URNA", "ST_CANDIDATO_INSERIDO_URNA",
                  "VR_DESPESA_MAX_CAMPANHA", "NR_PROCESSO", "TP_AGREMIACAO")

# importing CSV - CAND
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_5out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1",
                   drop = drop_columns,
                   colClasses = class_columns)

cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1",
                   drop = drop_columns,
                   colClasses = class_columns)

# importing CSV - ELEIT 2020
eleitorado_2020 <- fread("C:/Users/acaesar/Downloads/dados_5out2020/perfil_eleitorado_2020/perfil_eleitorado_2020.csv",
                   encoding = "Latin-1")

# importing CSV - RESULT 2016
resultado_2016 <- fread("C:/Users/acaesar/Downloads/dados_5out2020/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.csv",
                        encoding = "Latin-1", colClasses = c(SQ_CANDIDATO = "character"))
# RESULT - 
resultado_2016_n <- resultado_2016 %>%
  group_by(NR_TURNO, SQ_CANDIDATO, NM_CANDIDATO, NM_URNA_CANDIDATO) %>%
  summarize(votos_2016 = sum(QT_VOTOS_NOMINAIS)) %>%
  arrange(desc(votos_2016)) %>%
  filter(NR_TURNO == 1) %>%
  ungroup() %>%
  select(SQ_CANDIDATO, votos_2016)

# CAND - append suffix w year
colnames(cand_2020) <- paste(colnames(cand_2020), "2020", sep = "_") 
colnames(cand_2016) <- paste(colnames(cand_2016), "2016", sep = "_") 

# CAND - 2016 + 2020
# candidatos sem voto em 2016 eram candidatos a vice-prefeito
cand_2020_2016 <- cand_2020 %>%
  distinct(NM_CANDIDATO_2020, NR_CPF_CANDIDATO_2020, .keep_all = TRUE) %>%
  left_join(cand_2016, by = c("NR_CPF_CANDIDATO_2020" = "NR_CPF_CANDIDATO_2016")) %>%
  filter(!is.na(NM_CANDIDATO_2016)) %>%
  left_join(resultado_2016_n, by = c("SQ_CANDIDATO_2016" = "SQ_CANDIDATO")) %>%
  #mutate(check_titulo = NR_TITULO_ELEITORAL_CANDIDATO_2020 == NR_TITULO_ELEITORAL_CANDIDATO_2016) %>%
  #filter(check_titulo == FALSE)
  mutate(check_city = SG_UE_2020 == SG_UE_2016,
         check_state = SG_UF_2020 == SG_UF_2016) %>%
  filter(check_city == FALSE) %>%
  select(NR_TURNO_2020, SG_UF_2016, SG_UF_2020 , NM_UE_2016, NM_UE_2020, SG_UE_2016, SG_UE_2020,
         NM_CANDIDATO_2016, NM_CANDIDATO_2020, NM_URNA_CANDIDATO_2016, NM_URNA_CANDIDATO_2020,
         NR_CPF_CANDIDATO_2020, SG_PARTIDO_2016, SG_PARTIDO_2020, DS_CARGO_2016, 
         DS_CARGO_2020, DS_GENERO_2020, DS_GENERO_2016, DS_SIT_TOT_TURNO_2016, 
         NM_EMAIL_2020, NM_EMAIL_2016, check_city, check_state, votos_2016) %>%
  distinct(NM_CANDIDATO_2020, NR_CPF_CANDIDATO_2020, .keep_all = TRUE) %>%
  arrange(desc(votos_2016)) %>%
  filter(str_detect(DS_SIT_TOT_TURNO_2016, "ELEITO"))


