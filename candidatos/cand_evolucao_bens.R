# reading libraries
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
                  "DS_ESTADO_CIVIL", "DS_OCUPACAO", "DS_SIT_TOT_TURNO",
                  "NR_PROTOCOLO_CANDIDATURA", "DS_SITUACAO_CANDIDATO_PLEITO",
                  "DS_SITUACAO_CANDIDATO_URNA", "ST_CANDIDATO_INSERIDO_URNA",
                  "VR_DESPESA_MAX_CAMPANHA", "NR_PROCESSO", "TP_AGREMIACAO")

# importing CSV - CAND
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns,
                      colClasses = class_columns)

cand_2018_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/consulta_cand_2018/consulta_cand_2018_BRASIL.csv",
                      encoding = "Latin-1", 
                      drop = drop_columns, 
                      colClasses = class_columns)

cand_2016_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", 
                   drop = drop_columns, 
                   colClasses = class_columns)

# importing CSV - BENS
bens_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/bem_candidato_2020/bem_candidato_2020_BRASIL.csv",
                      encoding = "Latin-1",
                      colClasses = c(SQ_CANDIDATO = "character"))

bens_2018_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/bem_candidato_2018/bem_candidato_2018_BRASIL.csv",
                      encoding = "Latin-1",
                      colClasses = c(SQ_CANDIDATO = "character"))

bens_2016_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/bem_candidato_2016/bem_candidato_2016_BRASIL.csv",
                      encoding = "Latin-1",
                      colClasses = c(SQ_CANDIDATO = "character"))

### 2020
grouped_bens_2020 <- bens_2020_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

cand_patrimonio_2020 <- cand_2020_BR %>%
  select(SQ_CANDIDATO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO, 
         NR_CPF_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO,
         SG_PARTIDO, SG_UE, NM_UE, SG_UF) %>%
  left_join(grouped_bens_2020, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) %>%
  rename(`SQ_CANDIDATO_2020` = `SQ_CANDIDATO`,
         `DS_CARGO_2020` = `DS_CARGO`,
         `NR_TITULO_2020` = `NR_TITULO_ELEITORAL_CANDIDATO`, 
         `NM_URNA_CANDIDATO_2020` = `NM_URNA_CANDIDATO`,
         `SG_PARTIDO_2020` = `SG_PARTIDO`,
         `NM_CANDIDATO_2020` = `NM_CANDIDATO`,
         `SG_UE_2020` = `SG_UE`,
         `NM_UE_2020` = `NM_UE`,
         `SG_UF_2020` = `SG_UF`,
         `patrimonio_2020` = `total_patrimonio`)

### 2018
grouped_bens_2018 <- bens_2018_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

cand_patrimonio_2018 <- cand_2018_BR %>%
  select(SQ_CANDIDATO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO, 
         NR_CPF_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO,
         SG_PARTIDO, SG_UE, NM_UE, SG_UF) %>%
  left_join(grouped_bens_2018, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) %>%
  rename(`SQ_CANDIDATO_2018` = `SQ_CANDIDATO`,
         `DS_CARGO_2018` = `DS_CARGO`,
         `NR_TITULO_2018` = `NR_TITULO_ELEITORAL_CANDIDATO`, 
         `NM_URNA_CANDIDATO_2018` = `NM_URNA_CANDIDATO`,
         `SG_PARTIDO_2018` = `SG_PARTIDO`,
         `NM_CANDIDATO_2018` = `NM_CANDIDATO`,
         `SG_UE_2018` = `SG_UE`,
         `NM_UE_2018` = `NM_UE`,
         `SG_UF_2018` = `SG_UF`,
         `patrimonio_2018` = `total_patrimonio`)


## 2016
grouped_bens_2016 <- bens_2016_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

cand_patrimonio_2016 <- cand_2016_BR %>%
  select(SQ_CANDIDATO, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO, 
         NR_CPF_CANDIDATO, NR_TITULO_ELEITORAL_CANDIDATO,
         SG_PARTIDO, SG_UE, NM_UE, SG_UF) %>%
  left_join(grouped_bens_2016, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) %>%
  rename(`SQ_CANDIDATO_2016` = `SQ_CANDIDATO`,
          `DS_CARGO_2016` = `DS_CARGO`,
          `NR_TITULO_2016` = `NR_TITULO_ELEITORAL_CANDIDATO`, 
          `NM_URNA_CANDIDATO_2016` = `NM_URNA_CANDIDATO`,
          `SG_PARTIDO_2016` = `SG_PARTIDO`,
          `NM_CANDIDATO_2016` = `NM_CANDIDATO`,
          `SG_UE_2016` = `SG_UE`,
          `NM_UE_2016` = `NM_UE`,
          `SG_UF_2016` = `SG_UF`,
          `patrimonio_2016` = `total_patrimonio`)
        
## 2020 + 2018 + 2016 
cand_patrimonio_2020_2018_2016 <- cand_patrimonio_2020 %>%
    left_join(cand_patrimonio_2016, by = "NR_CPF_CANDIDATO") %>%
    left_join(cand_patrimonio_2018, by = "NR_CPF_CANDIDATO") %>%
    mutate(dif_patrimonio_16 = patrimonio_2020 - patrimonio_2016,
          var_patrimonio_16 = round(((patrimonio_2020 - patrimonio_2016) / patrimonio_2016) * 100),
          dif_patrimonio_18 = patrimonio_2020 - patrimonio_2018,
          var_patrimonio_18 = round(((patrimonio_2020 - patrimonio_2018) / patrimonio_2018) * 100)) %>%
    select(SQ_CANDIDATO_2020, 
           DS_CARGO_2020, DS_CARGO_2018, DS_CARGO_2016, 
           NM_CANDIDATO_2020, NM_CANDIDATO_2018, NM_CANDIDATO_2016,
           patrimonio_2020, patrimonio_2018, patrimonio_2016, 
           dif_patrimonio_16, dif_patrimonio_18,
           var_patrimonio_16, var_patrimonio_18, 
           SG_PARTIDO_2020, SG_PARTIDO_2018, SG_PARTIDO_2016, 
           SG_UE_2020, SG_UE_2018, SG_UE_2016, 
           NM_UE_2020, NM_UE_2018, NM_UE_2016, 
           SG_UF_2020, SG_UF_2018, SG_UF_2016,
           NR_TITULO_2020, NR_TITULO_2018, NR_TITULO_2016, 
           NR_CPF_CANDIDATO) %>%
  filter(DS_CARGO_2020 == "PREFEITO") %>%
  filter(SG_UE_2020 == "1392" | # Rio Branco - AC
           SG_UE_2020 == "27855" | # Maceió - AL
           SG_UE_2020 == "6050" | # Macapá - AP
           SG_UE_2020 == "2550" | # Manaus - AM
           SG_UE_2020 == "38490" | # Salvador - BA
           SG_UE_2020 == "13897" | # Fortaleza - CE
           SG_UE_2020 == "57053" | # Vitória - ES
           SG_UE_2020 == "93734" | # Goiânia - GO
           SG_UE_2020 == "9210" | # São Luís - MA
           SG_UE_2020 == "90670" | # Cuiabá - MT
           SG_UE_2020 == "90514" | # Campo Grande - MS
           SG_UE_2020 == "41238" | # Belo Horizonte - MG 
           SG_UE_2020 == "4278" | # Belém - PA
           SG_UE_2020 == "20516" | # João Pessoa - PB
           SG_UE_2020 == "75353" | # Curitiba - PR
           SG_UE_2020 == "25313" | # Recife - PE
           SG_UE_2020 == "12190" | # Teresina - PI
           SG_UE_2020 == "60011" | # Rio de Janeiro - RJ
           SG_UE_2020 == "17612" | # Natal - RN
           SG_UE_2020 == "88013" | # Porto Alegre - RS
           SG_UE_2020 == "35" | # Porto Velho - RO
           SG_UE_2020 == "3018" | # Boa Vista - RR
           SG_UE_2020 == "81051" | # Florianópolis - SC
           SG_UE_2020 == "71072" | # São Paulo - SP
           SG_UE_2020 == "31054" | # Aracaju - SE
           SG_UE_2020 == "73440") %>%
  distinct(SQ_CANDIDATO_2020, .keep_all = TRUE) %>%
  filter(NM_CANDIDATO_2016 != "NA" | NM_CANDIDATO_2018 != "NA")

write.csv(cand_patrimonio_2020_2018_2016, "cand_patrimonio_2020_2018_2016.csv")
