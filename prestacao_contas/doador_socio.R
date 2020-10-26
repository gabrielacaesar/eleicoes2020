# leitura de pacotes
library(data.table)
library(tidyverse)

# leitura de bases / SÓCIOS
socio <- fread("C:/Users/acaesar/Downloads/socios/socio.csv.gz")

# ajuste em bases / SÓCIOS
socio_n <- socio %>%
  mutate(cnpj_cpf_do_socio = str_remove_all(cnpj_cpf_do_socio, "\\*"),
         cnpj_cpf_do_socio = str_remove_all(cnpj_cpf_do_socio, "\\."),
         cnpj_cpf_do_socio = str_remove_all(cnpj_cpf_do_socio, "\\-"))

# leitura de base / CAND
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_26out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_OCUPACAO"))

# ajuste em bases / CAND
cand_2020_n <- cand_2020 %>%
  mutate(cpf_novo = str_sub(NR_CPF_CANDIDATO, 4)) %>%
  mutate(cpf_novo = str_sub(cpf_novo, end = -3))

# leitura de base / PRESTAÇÃO CONTAS / Receita
receitas_candidatos <- fread("C:/Users/acaesar/Downloads/dados_26out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                             encoding = "Latin-1",
                             select = c("DS_FONTE_RECEITA", "VR_RECEITA", "NR_CPF_CNPJ_DOADOR", "NM_DOADOR", "NM_DOADOR_RFB"),
                             colClasses = c(NR_CPF_CNPJ_DOADOR = "character"))

receitas_orgaos <- fread("C:/Users/acaesar/Downloads/dados_26out2020/prestacao_de_contas_eleitorais_orgaos_partidarios_2020/receitas_orgaos_partidarios_2020_BRASIL.csv",
                             encoding = "Latin-1",
                             select = c("DS_FONTE_RECEITA", "VR_RECEITA", "NR_CPF_CNPJ_DOADOR", "NM_DOADOR", "NM_DOADOR_RFB"),
                             colClasses = c(NR_CPF_CNPJ_DOADOR = "character"))

# ajuste em bases / PRESTAÇÃO CONTAS / Receita
receitas_candidatos_n <- receitas_candidatos %>%
  rbind(receitas_orgaos) %>%
  mutate(VR_RECEITA = str_replace_all(VR_RECEITA, "\\,", ".")) %>%
  mutate(NM_DOADOR_RFB = case_when(str_detect(NM_DOADOR_RFB, "#NULO#") ~ NM_DOADOR,
                                   !str_detect(NM_DOADOR_RFB, "#NULO#") ~ NM_DOADOR_RFB)) %>%
  mutate(cpf_novo = str_sub(NR_CPF_CNPJ_DOADOR, 4)) %>%
  mutate(cpf_novo = str_sub(cpf_novo, end = -3)) %>%
  filter(cpf_novo != "")

# ANÁLISE
cand_socio <- cand_2020_n %>%
  left_join(socio_n, by = c("NM_CANDIDATO" = "nome_socio",
                               "cpf_novo" = "cnpj_cpf_do_socio")) %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO,  .keep_all = TRUE) %>%
  filter(!is.na(percentual_capital_social))

doador_socio <- receitas_candidatos_n %>%
  left_join(socio_n, by = c("NM_DOADOR_RFB" = "nome_socio",
                            "cpf_novo" = "cnpj_cpf_do_socio")) %>%
  distinct(NM_DOADOR_RFB, NR_CPF_CNPJ_DOADOR,  .keep_all = TRUE) %>%
  filter(!is.na(percentual_capital_social))
