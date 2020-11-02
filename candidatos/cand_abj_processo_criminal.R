#HIROTA::
#Ver as respostas da Renata
#Checar caso de estupro de vulneravel ou outro crime mais grave, como homicídio e formação de quadrilha

# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos / ABJ
abj_prefeito <- read_rds("~/Downloads/candidatos_processos_2nov2020/prefeitos_criminal_passivo.rds")
abj_vice_prefeito <- read_rds("~/Downloads/candidatos_processos_2nov2020/vice_prefeitos_criminal_passivo.rds")
abj_vereador <- read_rds("~/Downloads/candidatos_processos_2nov2020/vereadores_criminal_passivo.rds")

# leitura de arquivos / CAND
cand_2020 <- fread("~/Downloads/dados_2nov2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_DETALHE_SITUACAO_CAND",
                              "NM_EMAIL"))

# ajuste CAND
cand_2020_n <- cand_2020 %>%
  filter(DS_DETALHE_SITUACAO_CAND != "RENÚNCIA" &
           DS_DETALHE_SITUACAO_CAND != "FALECIDO" &
           DS_DETALHE_SITUACAO_CAND != "CANCELADO")

# análise / PREFEITO
abj_prefeito_n <- abj_prefeito %>%
  left_join(cand_2020_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_prefeito_c <- abj_prefeito %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VICE-PREFEITO
abj_vice_prefeito_n <- abj_vice_prefeito %>%
  left_join(cand_2020_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_vice_prefeito_c <- abj_vice_prefeito %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VEREADOR
abj_vereador_n <- abj_vereador %>%
  left_join(cand_2020_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_vereador_c <- abj_vereador %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))
