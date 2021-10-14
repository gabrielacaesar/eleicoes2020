#2) - dos candidatos a prefeitos reeleitos com partido e região
# Isto, para especificar o que busco encontrar no item 2: 
# tenho uma hipótese de que o lançamento de candidatos 
# foi maior em lugares onde haveria um eleitorado mais aberto 
# à entrada de competidores de direita. 
# Dito de outra forma, locais como o Nordeste, por exemplo, 
# onde temos um eleitorado que vem se posicionando mais 
# à esquerda do espectro ideológico, não teria um aumento 
# significativo de candidatos concorrendo por partidos de direita, 
# em comparação com outras regiões do país. 
# (Além disso, suponho que haveria maior entrada desses partidos
# em locais onde os candidatos que estão pleiteando 
# pela reeleição são de partidos de direita, 
# pois isso significaria uma maior abertura do eleitorado para esse bloco de partidos.)

# leitura de bibliotecas e R scripts
library(tidyverse)
library(data.table)
library(writexl)
source('scripts/conversao_ideologia.R')
source('scripts/conversao_partido.R')

# leitura candidatos 2016
cand_2016 <- fread("data_tse/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1",
                   select = c("DS_ELEICAO", "SG_UF", "NM_UE", "DS_CARGO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "NR_TURNO"))

# leitura candidatos 2020
cand_2020 <- fread("data_tse/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1",
                   select = c("DS_ELEICAO", "SG_UF", "NM_UE", "DS_CARGO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "NR_TURNO"))

cand_2020_pref <- cand_2020 %>%
  filter(str_detect(DS_ELEICAO, "Eleições Municipais 2020")) %>% # desconsidera eleicao suplementar
  filter(DS_CARGO == "PREFEITO") %>% # considera apenas candidatos a prefeito
  corrigir_partidos() %>%
  definir_ideologia() %>%
  select(UF_2020 = SG_UF, MUNICIPIO_2020 = NM_UE,
         CAND_2020 = NM_CANDIDATO, PARTIDO_2020 = SG_PARTIDO, IDEOLOGIA_2020 = SG_IDEOLOGIA,
         SIT_2020 = DS_SIT_TOT_TURNO, TURNO_2020 = NR_TURNO, NR_CPF_CANDIDATO)

# leitura regioes do BR 
regioes_br <- fread("data_analysis/regioes.csv")

# analise resultado 2
resultado_2 <- cand_2016 %>%
  filter(DS_ELEICAO == "Eleições Municipais 2016" & 
           DS_CARGO == "PREFEITO" & 
           DS_SIT_TOT_TURNO == "ELEITO") %>%
  corrigir_partidos() %>%
  definir_ideologia() %>%
  mutate(RESULTADO_2016 = case_when(NR_TURNO == 1 ~ "Eleito 1º turno",
                                    NR_TURNO == 2 ~ "Eleito 2º turno")) %>%
  select(-c(DS_ELEICAO, DS_CARGO, DS_SIT_TOT_TURNO, NR_TURNO)) %>%
  select(UF_2016 = SG_UF, MUNICIPIO_2016 = NM_UE,
         CAND_2016 = NM_CANDIDATO, PARTIDO_2016 = SG_PARTIDO, 
         IDEOLOGIA_2016 = SG_IDEOLOGIA, RESULTADO_2016,
         NR_CPF_CANDIDATO) %>%
  left_join(cand_2020_pref, by = c("NR_CPF_CANDIDATO")) %>% # cruza as bases de dados com o CPF
  mutate(RESULTADO_2020 = case_when(SIT_2020 == "ELEITO" & TURNO_2020 == 1 ~ "Reeleito 1º turno",
                                    SIT_2020 == "ELEITO" & TURNO_2020 == 2 ~ "Reeleito 2º turno",
                                    SIT_2020 == "2º TURNO" & TURNO_2020 == 1 ~ "Perdeu no 2º turno",
                                    SIT_2020 == "#NULO#" ~ "#NULO#",
                                    is.na(TURNO_2020) ~ "Não disputou",
                                    TRUE ~ "Não eleito")) %>%
  mutate(MUDOU_MUNICIPIO = MUNICIPIO_2016 != MUNICIPIO_2020) %>% # checa se o municipio foi o mesmo
  left_join(regioes_br, by = c("UF_2016" = "uf")) %>% # cria coluna regiao
  replace(is.na(.), "-") %>% # substitui celulas vazias por '-'
  select(-c(SIT_2020, TURNO_2020)) %>%
  relocate(regiao, .before = "UF_2016") %>% # regiao como primeira coluna
  janitor::clean_names() # cabecalho em caixa baixa

writexl::write_xlsx(resultado_2, "data_analysis/resultado_2.xlsx")
