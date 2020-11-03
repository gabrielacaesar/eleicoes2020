# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos / ABJ
abj_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020/prefeitos_criminal_passivo.rds")
abj_vice_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020/vice_prefeitos_criminal_passivo.rds")
abj_vereador <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020/vereadores_criminal_passivo.rds")

# leitura de arquivos / CAND
cand_2020_SP <- fread("C:/Users/acaesar/Downloads/dados_3nov2020/consulta_cand_2020/consulta_cand_2020_SP.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_DETALHE_SITUACAO_CAND",
                              "NM_EMAIL"))

# ajuste CAND
cand_2020_SP_n <- cand_2020_SP %>%
  filter(DS_DETALHE_SITUACAO_CAND != "RENÚNCIA" &
           DS_DETALHE_SITUACAO_CAND != "FALECIDO" &
           DS_DETALHE_SITUACAO_CAND != "CANCELADO")

# análise / PREFEITO
abj_prefeito_tidy <- abj_prefeito %>%
  filter(!str_detect(status, "Suspenso") &
         !str_detect(status, "Extinto")) %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) 

abj_prefeito_n <- abj_prefeito_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int)) %>%
  filter(!is.na(NM_CANDIDATO))

abj_prefeito_c <- abj_prefeito_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VICE-PREFEITO
abj_vice_prefeito_tidy <- abj_vice_prefeito %>%
  filter(!str_detect(status, "Suspenso") &
           !str_detect(status, "Extinto")) %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO"))

abj_vice_prefeito_n <- abj_vice_prefeito_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int)) %>%
  filter(!is.na(NM_CANDIDATO))

abj_vice_prefeito_c <- abj_vice_prefeito_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VEREADOR
abj_vereador_tidy <- abj_vereador %>%
  filter(!str_detect(status, "Suspenso") &
           !str_detect(status, "Extinto")) %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO"))

abj_vereador_n <- abj_vereador_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int)) %>%
  filter(!is.na(NM_CANDIDATO))

abj_vereador_c <- abj_vereador_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_vereador_c2 <- abj_vereador_tidy %>%
  filter(str_detect(assunto, "Crimes de Tráfico Ilícito e Uso Indevido de Drogas") |
         str_detect(assunto, "Associação para a Produção e Tráfico e Condutas Afins") |
         str_detect(assunto, "Decorrente de Violência Doméstica") |
         str_detect(assunto, "Crimes de Tortura") |
         str_detect(assunto, "Homicídio Simples") |
         str_detect(assunto, "Homicídio Qualificado") |
         str_detect(assunto, "Tráfico de Drogas e Condutas Afins"))

