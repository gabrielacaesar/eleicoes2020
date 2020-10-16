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
                  "NR_PROCESSO", "TP_AGREMIACAO")

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
cand_2020_BR <- fread("~/Downloads/dados_16out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      drop = drop_columns,
                      colClasses = class_columns)

# importing CSV - prestação de contas / CANDIDATOS - RECEITA
receitas_candidatos <- fread("~/Downloads/dados_16out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                             encoding = "Latin-1",
                             select = select_columns,
                             colClasses = class_columns_2)


# importing CSV - prestação de contas / ÓRGÃO PARTIDÁRIO - RECEITA
receitas_partidos <- fread("~/Downloads/dados_16out2020/prestacao_de_contas_eleitorais_orgaos_partidarios_2020/receitas_orgaos_partidarios_2020_BRASIL.csv",
                           encoding = "Latin-1",
                           select = select_columns,
                           colClasses = class_columns_2)

# quantidade de doacoes para candidatos diferentes / PF
receitas_candidatos_n <- receitas_candidatos %>%
  mutate(VR_RECEITA= readr::parse_number(VR_RECEITA, 
                                         locale = readr::locale(decimal_mark = ","))) %>%
  filter(DS_ORIGEM_RECEITA == "Recursos de pessoas físicas") %>%
  select(SQ_CANDIDATO, NR_CPF_CNPJ_DOADOR, NM_DOADOR, NM_DOADOR_RFB) %>%
  distinct(SQ_CANDIDATO, NR_CPF_CNPJ_DOADOR, .keep_all = TRUE) %>%
  mutate(NM_DOADOR_RFB = case_when(str_detect(NM_DOADOR_RFB, "#NULO#") ~ NM_DOADOR,
                                   !str_detect(NM_DOADOR_RFB, "#NULO#") ~ NM_DOADOR_RFB)) %>%
  group_by(NR_CPF_CNPJ_DOADOR, NM_DOADOR_RFB) %>%
  summarise(contagem = n())

# soma de doacoes / PF
receitas_candidatos_total <- receitas_candidatos %>%
  mutate(VR_RECEITA= readr::parse_number(VR_RECEITA, 
                                         locale = readr::locale(decimal_mark = ","))) %>%
  filter(DS_ORIGEM_RECEITA == "Recursos de pessoas físicas") %>%
  group_by(NR_CPF_CNPJ_DOADOR) %>%
  summarise(soma = sum(VR_RECEITA))

# total arrecadado por candidato / TOTAL
receitas_total <- receitas_candidatos %>%
  mutate(VR_RECEITA= readr::parse_number(VR_RECEITA, 
                                         locale = readr::locale(decimal_mark = ","))) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(receita_total = sum(VR_RECEITA))

# candidatos 
cand_2020_BR_n <- cand_2020_BR %>%
  select(SQ_CANDIDATO, SG_UF, NM_UE, DS_CARGO, NM_CANDIDATO, NM_URNA_CANDIDATO, 
         NR_CPF_CANDIDATO, DS_SITUACAO_CANDIDATURA, SG_PARTIDO, VR_DESPESA_MAX_CAMPANHA) 

# analise
t <- receitas_candidatos %>%
  mutate(VR_RECEITA= readr::parse_number(VR_RECEITA, 
                                         locale = readr::locale(decimal_mark = ","))) %>%
  filter(DS_ORIGEM_RECEITA == "Recursos de pessoas físicas") %>%
  group_by(SQ_CANDIDATO, NR_CPF_CNPJ_DOADOR) %>%
  summarise(int = sum(VR_RECEITA)) %>%
  arrange(desc(int)) %>% 
  left_join(receitas_candidatos_n, by = "NR_CPF_CNPJ_DOADOR") %>%
  left_join(receitas_candidatos_total, by = "NR_CPF_CNPJ_DOADOR") %>%
  mutate(perc_doacao_total = round((int / soma) * 100)) %>%
  left_join(cand_2020_BR_n, by = "SQ_CANDIDATO") %>%
  left_join(receitas_total, by = "SQ_CANDIDATO") %>%
  mutate(perc_receita = round((int / receita_total)) * 100) %>%
  mutate(perc_limite = round((int / VR_DESPESA_MAX_CAMPANHA) * 100)) %>%
  mutate(faixa_doacao = case_when(int > 100000 ~ "acima_de_100_mil",
                                  int > 50000 & int <= 100000 ~ "de_50_a_100_mil",
                                  int > 25000 & int <= 50000 ~ "de_25_a_50_mil",
                                  int > 10000 & int <= 25000 ~ "de_10_a_25_mil",
                                  int > 5000 & int <= 10000 ~ "de_5_a_10_mil",
                                  int > 0 & int <= 5000 ~ "de_0_a_5_mil")) %>%
  select("SQ_CANDIDATO", "NR_CPF_CNPJ_DOADOR", "NM_DOADOR_RFB", "NM_CANDIDATO", "NM_URNA_CANDIDATO", 
         "SG_PARTIDO", "SG_UF", "NM_UE", "DS_CARGO", "int", "contagem", "soma", "receita_total",
         "perc_receita", "perc_doacao_total", "perc_limite", "faixa_doacao", "VR_DESPESA_MAX_CAMPANHA") %>%
  arrange(desc(int))
  
