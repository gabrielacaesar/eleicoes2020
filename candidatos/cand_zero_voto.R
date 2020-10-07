# lendo pacotes
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
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_6out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
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

# ELEITORADO 2020
eleitorado_2020_n <- eleitorado_2020 %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  rename("SG_UE" = "CD_MUNICIPIO")

# RESULT - 2016
resultado_2016_n <- resultado_2016 %>%
  group_by(NR_TURNO, SQ_CANDIDATO, NM_CANDIDATO, NM_URNA_CANDIDATO) %>%
  summarize(votos = sum(QT_VOTOS_NOMINAIS)) %>%
  arrange(desc(votos)) %>%
  filter(NR_TURNO == 1) %>%
  ungroup() %>%
  filter(votos == 0) %>%
  select(SQ_CANDIDATO, votos) %>%
  left_join(cand_2016, by = "SQ_CANDIDATO") %>%
  filter(DS_CARGO == "VEREADOR") %>%
  filter(DS_GENERO == "FEMININO") %>%
  filter(DS_DETALHE_SITUACAO_CAND == "DEFERIDO") %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PTN", "PODE")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PMDB", "MDB")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PT do B", "AVANTE")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PEN", "PATRIOTA")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PSDC", "DC")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPS", "CIDADANIA")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PR$", "PL")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRB", "REPUBLICANOS")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRP", "PRP - incorporado Patriota")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPL", "PPL - incorporado PCdoB")) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PHS", "PHS - incorporado PODE"))
  
# CAND - append suffix w year
colnames(cand_2020) <- paste(colnames(cand_2020), "2020", sep = "_") 
colnames(resultado_2016_n) <- paste(colnames(resultado_2016_n), "2016", sep = "_") 

# merging
cand_2016_2020 <- resultado_2016_n %>%
  left_join(cand_2020, by = c("NR_CPF_CANDIDATO_2016" = "NR_CPF_CANDIDATO_2020")) %>%
  filter(!is.na(NM_CANDIDATO_2020)) %>%
  mutate(SG_PARTIDO_2020 = str_replace_all(SG_PARTIDO_2020, "SOLIDARIEDADE", "SD")) %>%
  select(-c(ANO_ELEICAO_2016, NR_TURNO_2016, DS_ELEICAO_2016,
            NR_CANDIDATO_2016, NM_COLIGACAO_2016, DS_COMPOSICAO_COLIGACAO_2016,
            CD_GENERO_2016, DS_COR_RACA_2016, ST_REELEICAO_2016, ST_DECLARAR_BENS_2016,
            ANO_ELEICAO_2020, NR_TURNO_2020, DS_ELEICAO_2020, CD_GENERO_2020,
            DS_COR_RACA_2020, NR_CANDIDATO_2020, SQ_COLIGACAO_2020, NM_COLIGACAO_2020,
            DS_COMPOSICAO_COLIGACAO_2020)) %>%
  mutate(check_partido = SG_PARTIDO_2016 == SG_PARTIDO_2020,
         check_titulo = NR_TITULO_ELEITORAL_CANDIDATO_2016 == NR_TITULO_ELEITORAL_CANDIDATO_2020) %>%
  left_join(eleitorado_2020_n, by = c("SG_UE_2020" = "SG_UE")) %>%
  select(SQ_CANDIDATO_2016, votos_2016, SG_UF_2016, SG_UF_2020,
         SG_UE_2016, SG_UE_2020, NM_UE_2016, NM_UE_2020,
         DS_CARGO_2016, DS_CARGO_2020, NM_CANDIDATO_2016,
         NM_CANDIDATO_2020, NM_URNA_CANDIDATO_2016, 
         NM_URNA_CANDIDATO_2020, SG_PARTIDO_2016,
         SG_PARTIDO_2020, check_partido, DS_GENERO_2016, DS_GENERO_2020,
         NR_TITULO_ELEITORAL_CANDIDATO_2016, NR_TITULO_ELEITORAL_CANDIDATO_2020,
         check_titulo, DS_SITUACAO_CANDIDATURA_2020, NM_EMAIL_2016, NM_EMAIL_2020)
  
write.csv(cand_2016_2020, "cand_2016_2020.csv")

# mudou partido
partido_2016_2020 <- cand_2016_2020 %>%
  group_by(check_partido) %>%
  summarise(int = n()) %>%
  pivot_wider(values_from = int, names_from = check_partido) %>%
  janitor::clean_names() %>%
  mutate(total = false + true,
         false_perc = (false / total) * 100,
         true_perc = (true / total) * 100)
  
  
# mudou cidade
city_2016_2020 <- cand_2016_2020 %>%
  mutate(check_city = SG_UE_2016 == SG_UE_2020) %>%
  group_by(check_city) %>%
  summarise(int = n()) %>%
  pivot_wider(values_from = int, names_from = check_city) %>%
  janitor::clean_names() %>%
  mutate(total = false + true,
         false_perc = (false / total) * 100,
         true_perc = (true / total) * 100)

# cargo disputa
cargo_2016_2020 <- cand_2016_2020 %>%
  group_by(DS_CARGO_2020) %>%
  summarise(int = n()) %>%
  pivot_wider(values_from = int, names_from = DS_CARGO_2020) %>%
  janitor::clean_names() %>%
  mutate(total = prefeito + vice_prefeito + vereador,
         ver_perc = (vereador / total) * 100)

# partido disputa
sigla_2016_2020 <- cand_2016_2020 %>%
  group_by(SG_PARTIDO_2020) %>%
  summarise(int = n())

# UF disputa
UF_2016_2020 <- cand_2016_2020 %>%
  group_by(SG_UF_2020) %>%
  summarise(int = n()) 
