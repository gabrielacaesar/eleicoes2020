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

# colunas para selecionar
select_columns <- c("DT_PRESTACAO_CONTAS", "SQ_PRESTADOR_CONTAS", "NR_CNPJ_PRESTADOR_CONTA",
                    "SQ_CANDIDATO", "DS_FONTE_RECEITA", "DS_ORIGEM_RECEITA",
                    "DS_NATUREZA_RECEITA", "DS_ESPECIE_RECEITA", "NR_CPF_CNPJ_DOADOR",
                    "NM_DOADOR", "NM_DOADOR_RFB", "DS_ESFERA_PARTIDARIA_DOADOR", "SQ_CANDIDATO_DOADOR", 
                    "DS_CARGO_CANDIDATO_DOADOR", "SG_PARTIDO_DOADOR", "NR_RECIBO_DOACAO",
                    "NR_DOCUMENTO_DOACAO", "SQ_RECEITA", "DT_RECEITA", "DS_RECEITA", "VR_RECEITA")

# colunas para definir classe / CAND
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# colunas para definir classe / prestação de contas
class_columns_2 <- c(SQ_PRESTADOR_CONTAS = "character", 
                   SQ_CANDIDATO = "character",
                   NR_CPF_CNPJ_DOADOR = "character",
                   SQ_CANDIDATO_DOADOR = "character",
                   NR_RECIBO_DOACAO = "character",
                   NR_DOCUMENTO_DOACAO = 'character',
                   SQ_RECEITA = "character")

# importing CSV - candidatos
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_6out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns,
                      colClasses = class_columns)

# importing CSV - prestação de contas / CANDIDATOS
receitas_candidatos <- fread("C:/Users/acaesar/Downloads/dados_8out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                             encoding = "Latin-1",
                             select = select_columns,
                             colClasses = class_columns_2)

Varanda Suspensa

# 
recursos_proprios <- receitas_candidatos %>%
  mutate(VR_RECEITA = readr::parse_number(VR_RECEITA, 
                locale = readr::locale(decimal_mark = ","))) %>%
  filter(DS_ORIGEM_RECEITA == "Recursos próprios") %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(int = sum(VR_RECEITA)) %>%
  left_join(cand_2020_BR, by = "SQ_CANDIDATO") %>%
  select(-c(ANO_ELEICAO, NR_TURNO, DS_ELEICAO)) %>%
  arrange(desc(int))
  