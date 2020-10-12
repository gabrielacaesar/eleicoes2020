# reading libraries
library(tidyverse)
library(data.table)

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# importing CSV
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_12out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      colClasses = class_columns)

bens_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_12out2020/bem_candidato_2020/bem_candidato_2020_BRASIL.csv",
                      encoding = "Latin-1",
                      colClasses = c(SQ_CANDIDATO = "character"))

# grouping properties and calculating total
grouped_bens <- bens_2020_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

# joining dataframes and replacing NA for 0
cand_patrimonio <- cand_2020_BR %>%
  left_join(grouped_bens, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) 

# select capitais / VEREADOR
quem_escolho_vereador <- cand_patrimonio %>%
  select(DS_CARGO, DS_DETALHE_SITUACAO_CAND, SQ_CANDIDATO, SG_UE, SG_UF, NM_UE, NM_CANDIDATO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO, SG_PARTIDO, DS_OCUPACAO, 
         NR_IDADE_DATA_POSSE, DS_GENERO, DS_COR_RACA, DS_GRAU_INSTRUCAO, total_patrimonio, NR_CANDIDATO, NM_EMAIL) %>%
  filter(DS_CARGO == "VEREADOR") %>%
  distinct(SQ_CANDIDATO, .keep_all = TRUE) %>%
  filter(SG_UE == "1392" | # Rio Branco - AC
           SG_UE == "27855" | # Maceió - AL
           SG_UE == "6050" | # Macapá - AP
           SG_UE == "2550" | # Manaus - AM
           SG_UE == "38490" | # Salvador - BA
           SG_UE == "13897" | # Fortaleza - CE
           SG_UE == "57053" | # Vitória - ES
           SG_UE == "93734" | # Goiânia - GO
           SG_UE == "9210" | # São Luís - MA
           SG_UE == "90670" | # Cuiabá - MT
           SG_UE == "90514" | # Campo Grande - MS
           SG_UE == "41238" | # Belo Horizonte - MG 
           SG_UE == "4278" | # Belém - PA
           SG_UE == "20516" | # João Pessoa - PB
           SG_UE == "75353" | # Curitiba - PR
           SG_UE == "25313" | # Recife - PE
           SG_UE == "12190" | # Teresina - PI
           SG_UE == "60011" | # Rio de Janeiro - RJ
           SG_UE == "17612" | # Natal - RN
           SG_UE == "88013" | # Porto Alegre - RS
           SG_UE == "35" | # Porto Velho - RO
           SG_UE == "3018" | # Boa Vista - RR
           SG_UE == "81051" | # Florianópolis - SC
           SG_UE == "71072" | # São Paulo - SP
           SG_UE == "31054" | # Aracaju - SE
           SG_UE == "73440") 

write.csv(quem_escolho_vereador, "quem_escolho_vereador.csv")

# BRASIL - PREFEITO + select
quem_escolho_PREFEITO_BR <- cand_patrimonio %>%
  select(DS_CARGO, SQ_CANDIDATO, SG_UE, SG_UF, NM_UE, NM_CANDIDATO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO, SG_PARTIDO, DS_OCUPACAO, 
         NR_IDADE_DATA_POSSE, DS_GENERO, DS_COR_RACA, DS_GRAU_INSTRUCAO, total_patrimonio, NR_CANDIDATO, NM_EMAIL) %>%
  filter(DS_CARGO == "PREFEITO") %>%
  distinct(SQ_CANDIDATO, .keep_all = TRUE)

write.csv(quem_escolho_PREFEITO_BR, "quem_escolho_PREFEITO_BR.csv")
