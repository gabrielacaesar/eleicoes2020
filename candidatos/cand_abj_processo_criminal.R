# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos / ABJ
abj_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020-3/prefeitos_criminal_reus.rds")
abj_vice_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020-3/vice_prefeitos_criminal_reus.rds")
abj_vereador <- read_rds("C:/Users/acaesar/Downloads/candidatos_processos_3nov2020-3/vereadores_criminal_reus.rds")

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
  filter(classe != "Carta Precatória Criminal" &
         classe != "Outros Feitos não Especificados" &
         classe != "Carta de Ordem Criminal" &
         classe != "Execução da Pena") %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  filter(!is.na(NM_CANDIDATO)) %>%
  relocate(NM_CANDIDATO, NM_UE, SG_PARTIDO, DS_DETALHE_SITUACAO_CAND, NM_EMAIL) %>%
  select(-c(movimentacoes, partes, historico, audiencias, delegacia,
            local_fisico, valor_da_acao, cdp, digital, outros_numeros, cdas, 
            apensado_ao, dados_da_precatoria, entranhado_ao, execucao_de_sentenca, 
            processo_principal, recebido_em, nº_de_ordem_antigo))

write.csv(abj_prefeito_tidy, "abj_prefeito_tidy.csv")

abj_prefeito_n <- abj_prefeito_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

write.csv(abj_prefeito_n, "abj_prefeito_n.csv")

abj_prefeito_c <- abj_prefeito_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

write.csv(abj_prefeito_c, "abj_prefeito_c.csv")

# análise / VICE-PREFEITO
abj_vice_prefeito_tidy <- abj_vice_prefeito %>%
  filter(!str_detect(status, "Suspenso") &
           !str_detect(status, "Extinto")) %>%
  filter(classe != "Carta Precatória Criminal" &
           classe != "Outros Feitos não Especificados" &
           classe != "Carta de Ordem Criminal" &
           classe != "Execução da Pena") %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  filter(!is.na(NM_CANDIDATO)) %>%
  relocate(NM_CANDIDATO, NM_UE, SG_PARTIDO, DS_DETALHE_SITUACAO_CAND, NM_EMAIL) %>%
  select(-c(movimentacoes, partes, historico, audiencias, delegacia,
            local_fisico, valor_da_acao, cdp, digital, outros_numeros, cdas, 
            apensado_ao, dados_da_precatoria, entranhado_ao))

write.csv(abj_vice_prefeito_tidy, "abj_vice_prefeito_tidy.csv")

abj_vice_prefeito_n <- abj_vice_prefeito_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int)) 

write.csv(abj_vice_prefeito_n, "abj_vice_prefeito_n.csv")

abj_vice_prefeito_c <- abj_vice_prefeito_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

write.csv(abj_vice_prefeito_c, "abj_vice_prefeito_c.csv")

# análise / VEREADOR
abj_vereador_tidy <- abj_vereador %>%
  filter(!str_detect(status, "Suspenso") &
           !str_detect(status, "Extinto")) %>%
  filter(classe != "Carta Precatória Criminal" &
           classe != "Outros Feitos não Especificados" &
           classe != "Carta de Ordem Criminal" &
           classe != "Execução da Pena") %>%
  left_join(cand_2020_SP_n, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  filter(!is.na(NM_CANDIDATO)) %>%
  relocate(NM_CANDIDATO, NM_UE, SG_PARTIDO, DS_DETALHE_SITUACAO_CAND, NM_EMAIL) %>%
  select(-c(movimentacoes, partes, historico, audiencias, delegacia,
            local_fisico, valor_da_acao, cdp, digital, outros_numeros, cdas, 
            apensado_ao, dados_da_precatoria, entranhado_ao,
            execucao_de_sentenca, processo_principal, recebido_em, acao_incidental, 
            unificado_ao, incidente))

write.csv(abj_vereador_tidy, "abj_vereador_tidy.csv")

abj_vereador_n <- abj_vereador_tidy %>%
  group_by(NM_CANDIDATO, cpf_candidato, SG_PARTIDO, NM_UE, DS_DETALHE_SITUACAO_CAND) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

write.csv(abj_vereador_n, "abj_vereador_n.csv")

abj_vereador_c <- abj_vereador_tidy %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

write.csv(abj_vereador_c, "abj_vereador_c.csv")

abj_vereador_c2 <- abj_vereador_tidy %>%
  filter(str_detect(assunto, "Crimes de Tráfico Ilícito e Uso Indevido de Drogas") |
           str_detect(assunto, "Associação para a Produção e Tráfico e Condutas Afins") |
           str_detect(assunto, "Decorrente de Violência Doméstica") |
           str_detect(assunto, "Crimes de Tortura") |
           str_detect(assunto, "Homicídio Simples") |
           str_detect(assunto, "Homicídio Qualificado") |
           str_detect(assunto, "Tráfico de Drogas e Condutas Afins"))
           
write.csv(abj_vereador_c2, "abj_vereador_c2.csv")

## all
abj_c <- abj_vereador_c %>%
  rbind(abj_vice_prefeito_c, abj_prefeito_c) %>%
  group_by(assunto) %>%
  summarise(int = sum(int)) %>%
  arrange(desc(int))
