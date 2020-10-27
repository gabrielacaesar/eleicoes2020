#HIROTA::
#A base inclui condenados ou nao? ou soh tramitando?
#Por que NA? o que faz? eu entro e olho? segredo de justiça?
#Checar caso de estupro de vulneravel ou outro crime mais grave, como homicídio e formação de quadrilha

# leitura de pacotes
library(tidyverse)

# leitura de arquivos / ABJ
abj_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_HIROTA/prefeitos_criminal.rds")
abj_vice_prefeito <- read_rds("C:/Users/acaesar/Downloads/candidatos_HIROTA/vice_prefeitos_criminal.rds")
abj_vereador <- read_rds("C:/Users/acaesar/Downloads/candidatos_HIROTA/vereadores_criminal.rds")

# leitura de arquivos / CAND
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_27out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO"))

# análise / PREFEITO
abj_prefeito_n <- abj_prefeito %>%
  left_join(cand_2020, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_prefeito_c <- abj_prefeito %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VICE-PREFEITO
abj_vice_prefeito_n <- abj_vice_prefeito %>%
  left_join(cand_2020, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_vice_prefeito_c <- abj_vice_prefeito %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

# análise / VEREADOR
abj_vereador_n <- abj_vereador %>%
  left_join(cand_2020, by = c("cpf_candidato" = "NR_CPF_CANDIDATO")) %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE) %>%
  summarise(int = n()) %>%
  arrange(desc(int))

abj_vereador_c <- abj_vereador %>%
  select(assunto, classe) %>%
  group_by(assunto) %>%
  summarise(int = n()) %>%
  arrange(desc(int))
