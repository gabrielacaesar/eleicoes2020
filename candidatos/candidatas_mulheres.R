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

cand_mulher_VEREADOR <- cand_2020_BR %>%
  filter(DS_CARGO == "VEREADOR") %>%
  distinct(SQ_CANDIDATO, .keep_all = TRUE) %>%
  group_by(SG_PARTIDO, DS_GENERO) %>%
  summarise(int = n()) %>%
  pivot_wider(values_from = int, names_from = DS_GENERO) %>%
  janitor::clean_names() %>%
  mutate(total = feminino + masculino,
         fem_perc = round((feminino / total) * 100),
         masc_perc = round((masculino / total) * 100)) %>%
  arrange(desc(masc_perc))

write.csv(cand_mulher_VEREADOR, "cand_mulher_VEREADOR.csv")
  
cand_mulher_PREFEITO <- cand_2020_BR %>%
  filter(DS_CARGO == "PREFEITO") %>%
  distinct(SQ_CANDIDATO, .keep_all = TRUE) %>%
  group_by(DS_GENERO) %>%
  summarise(int = n()) %>%
  pivot_wider(values_from = int, names_from = DS_GENERO) %>%
  janitor::clean_names() %>%
  replace(is.na(.), 0) %>%
  mutate(total = feminino + masculino,
         fem_perc = round((feminino / total) * 100),
         masc_perc = round((masculino / total) * 100)) %>%
  arrange(desc(masc_perc))

write.csv(cand_mulher_PREFEITO, "cand_mulher_PREFEITO.csv")
