# resposta 2 - v 2016
# candidatos a candidatos a prefeito lançados em 2016 por UF e região

# leitura de bibliotecas e R scripts
library(tidyverse)
library(data.table)
library(writexl)
source('scripts/conversao_ideologia.R') # https://github.com/gabrielacaesar/eleicoes2020/blob/master/frila_2021/scripts/conversao_ideologia.R
source('scripts/conversao_partido.R') # https://github.com/gabrielacaesar/eleicoes2020/blob/master/frila_2021/scripts/conversao_partido.R

# header
columns_name <- c("DS_ELEICAO", 
                  "UF", 
                  "CIDADE", 
                  "CARGO", 
                  "CANDIDATO", 
                  "CPF", 
                  "SG_PARTIDO", 
                  "SITUACAO",
                  "TURNO")

# leitura candidatos 2016
cand_2016 <- fread("data_tse/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1",
                   col.names = columns_name,
                   select = c("DS_ELEICAO", "SG_UF", "NM_UE", "DS_CARGO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "NR_TURNO"))

# leitura regioes do BR 
regioes_br <- fread("data_analysis/regioes.csv")

# analise resultado 2 - v 2016
resultado_2_v2016 <- cand_2016 %>%
  filter(DS_ELEICAO == "Eleições Municipais 2016" & 
         CARGO == "PREFEITO") %>%
  corrigir_partidos() %>%
  definir_ideologia() %>%
  select(-c(DS_ELEICAO, CARGO, CPF, TURNO)) %>%
  left_join(regioes_br, by = c("UF" = "uf")) %>% # cria coluna regiao
  relocate(regiao, .before = "UF") %>% # regiao como primeira coluna
  rename(IDEOLOGIA = SG_IDEOLOGIA,
         PARTIDO = SG_PARTIDO) %>%
  janitor::clean_names() # cabecalho em caixa baixa

colnames(resultado_2_v2016) <- paste0(colnames(resultado_2_v2016), "_2016") 

writexl::write_xlsx(resultado_2_v2016, "data_analysis/resultado_2_v2016.xlsx")
