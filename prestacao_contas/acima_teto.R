# reading libraries
library(tidyverse)
library(data.table)

# colunas para selecionar - CAND
select_columns_1 <- c("SQ_CANDIDATO", "SG_UF", "NM_UE", "DS_CARGO", "NM_CANDIDATO", "NM_URNA_CANDIDATO", 
                      "NR_CPF_CANDIDATO", "DS_SITUACAO_CANDIDATURA", "SG_PARTIDO", "VR_DESPESA_MAX_CAMPANHA")

# colunas para selecionar - PREST
select_columns_2 <- c("DT_PRESTACAO_CONTAS", "SQ_PRESTADOR_CONTAS", "NR_CNPJ_PRESTADOR_CONTA",
                    "SQ_CANDIDATO", "DS_FONTE_RECEITA", "DS_ORIGEM_RECEITA",
                    "DS_NATUREZA_RECEITA", "DS_ESPECIE_RECEITA", "NR_CPF_CNPJ_DOADOR",
                    "NM_DOADOR", "NM_DOADOR_RFB", "DS_ESFERA_PARTIDARIA_DOADOR", "SQ_CANDIDATO_DOADOR", 
                    "DS_CARGO_CANDIDATO_DOADOR", "SG_PARTIDO_DOADOR", "NR_RECIBO_DOACAO",
                    "NR_DOCUMENTO_DOACAO", "SQ_RECEITA", "DT_RECEITA", "DS_RECEITA", "VR_RECEITA")

# colunas para definir classe / CAND
class_columns_1 <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# colunas para definir classe / PREST
class_columns_2 <- c(SQ_PRESTADOR_CONTAS = "character", 
                     SQ_CANDIDATO = "character",
                     NR_CPF_CNPJ_DOADOR = "character",
                     SQ_CANDIDATO_DOADOR = "character",
                     NR_RECIBO_DOACAO = "character",
                     NR_DOCUMENTO_DOACAO = 'character',
                     SQ_RECEITA = "character")

# importing CSV - CONSULTA CANDIDATOS
cand_2020_BR <- fread("~/Downloads/dados_16out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      select = select_columns_1,
                      colClasses = class_columns_1)

# importing CSV - prestação de contas / CANDIDATOS - RECEITA
receitas_candidatos <- fread("~/Downloads/dados_16out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                             encoding = "Latin-1",
                             select = select_columns_2,
                             colClasses = class_columns_2)

# receita - CANDIDATOS
receitas_candidatos_n <- receitas_candidatos %>%
  mutate(VR_RECEITA= readr::parse_number(VR_RECEITA, 
                                         locale = readr::locale(decimal_mark = ","))) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(receita = sum(VR_RECEITA))

# analise
g <- cand_2020_BR %>%
  left_join(receitas_candidatos_n, by = "SQ_CANDIDATO") %>%
  mutate(perc_teto = round((receita / VR_DESPESA_MAX_CAMPANHA), 2) * 100,
         diferenca = VR_DESPESA_MAX_CAMPANHA - receita)



  

