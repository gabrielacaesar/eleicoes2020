library(tidyverse)
library(data.table)

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_PROTOCOLO_CANDIDATURA = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# definindo colunas que queremos ler
selected_colums <- c("ANO_ELEICAO", "NM_TIPO_ELEICAO", "SG_UF", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                     "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_CPF_CANDIDATO", "NM_EMAIL", "SG_PARTIDO", 
                     "NR_TITULO_ELEITORAL_CANDIDATO", "DS_GENERO", "CD_COR_RACA", "DS_COR_RACA", "ST_REELEICAO")

# leitura de BRASIL - 2020
cand_2020 <- fread("C:/Users/acaesar/Downloads/24set2020/consulta_cand_2020_24set2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1", colClasses = class_columns, select = selected_colums)

# leitura de BRASIL - 2018
cand_2018 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2018/consulta_cand_2018_BRASIL.csv",
                   encoding = "Latin-1", colClasses = class_columns, select = selected_colums)

# leitura de BRASIL - 2016
cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", colClasses = class_columns, select = selected_colums)


cand_2018_2020 <- cand_2020 %>%
  left_join(cand_2018, by = "NR_CPF_CANDIDATO") %>%
  filter(`CD_COR_RACA.y` != "NA") %>%
  mutate(check_color = `DS_COR_RACA.x` == `DS_COR_RACA.y`) %>%
  filter(check_color == FALSE) %>%
  select(-c("NM_URNA_CANDIDATO.x", "NM_URNA_CANDIDATO.y", 
            "ST_REELEICAO.x", "NM_EMAIL.x",
            "CD_COR_RACA.x", "CD_COR_RACA.y")) %>%
  filter(`DS_COR_RACA.x` != "SEM INFORMAÇÃO") %>%
  unite("change_color", c("DS_COR_RACA.x", "DS_COR_RACA.y"), sep = " <- ", remove = FALSE) %>%
  group_by(change_color) %>%
  summarise(contagem = n()) %>%
  arrange(desc(contagem))

cand_2016_2020 <- cand_2020 %>%
  left_join(cand_2016, by = "NR_CPF_CANDIDATO") %>%
  filter(`CD_COR_RACA.y` != "NA") %>%
  mutate(check_color = `DS_COR_RACA.x` == `DS_COR_RACA.y`) %>%
  filter(check_color == FALSE) %>%
  select(-c("NM_URNA_CANDIDATO.x", "NM_URNA_CANDIDATO.y", 
            "ST_REELEICAO.y", "NM_EMAIL.x",
            "CD_COR_RACA.x", "CD_COR_RACA.y")) %>%
  filter(`DS_COR_RACA.x` != "SEM INFORMAÇÃO") %>%
  unite("change_color", c("DS_COR_RACA.x", "DS_COR_RACA.y"), sep = " <- ", remove = FALSE) 
  # group_by(change_color) %>%
  # summarise(contagem = n()) %>%
  # arrange(desc(contagem))

BRANCA_PARDA_2016_2020 <- cand_2016_2020 %>%
  filter(change_color == "PARDA <- BRANCA")

write.csv(BRANCA_PARDA_2016_2020, "BRANCA_PARDA_2016_2020.csv")
