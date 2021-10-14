# 1- candidatos a prefeitos (eleitos e não eleitos), contendo partido, uf e região
# Esquerda >> UP; PSTU; PCO; PCB; PSOL; PCdoB; PT.
# Centro >> MDB/PMDB; PSDB; PDT; PSB; Rede; Cidadania/PPS; PV; PTB; Avante/PTdoB; SD; PMN.
# Direita >> Brasil 35/PMB; PSD; Podemos/PTN; PRTB; Pros; PL/PR; Republicanos/PRB; PTC; DC/PSDC; PSL; Novo; Progressistas/PP; PSC; Patriota/PEN; DEM.
# Demais: "Sem classificação"

# leitura de bibliotecas e R scripts
library(tidyverse)
library(data.table)
library(writexl)
source('scripts/conversao_ideologia.R') # importa script para definir ideologias
source('scripts/conversao_partido.R') # importa script para corrigir partidos

# leitura candidatos 2020
cand_2020 <- fread("data_tse/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1",
                   select = c("DS_ELEICAO", "SG_UF", "NM_UE", "DS_CARGO", "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "NR_TURNO"))

# leitura regioes do BR 
regioes_br <- fread("data_analysis/regioes.csv")

# analise resultado 1
resultado_1 <- cand_2020 %>%
  filter(str_detect(DS_ELEICAO, "Eleições Municipais 2020")) %>% # desconsidera eleicao suplementar
  filter(DS_CARGO == "PREFEITO") %>% # apenas candidatos a prefeito
  corrigir_partidos() %>% # correcao dos partidos
  definir_ideologia() %>% # cria coluna com ideologia
  left_join(regioes_br, by = c("SG_UF" = "uf")) %>% # cria coluna co regiao
  mutate(RESULTADO_2020 = case_when(DS_SIT_TOT_TURNO == "ELEITO" & NR_TURNO == 1 ~ "Eleito 1º turno",
                                    DS_SIT_TOT_TURNO == "ELEITO" & NR_TURNO == 2 ~ "Eleito 2º turno",
                                    DS_SIT_TOT_TURNO == "2º TURNO" & NR_TURNO == 1 ~ "Perdeu no 2º turno",
                                    DS_SIT_TOT_TURNO == "#NULO#" ~ "#NULO#",
                                    TRUE ~ "Não eleito")) %>%
  select(REGIAO = regiao, 
         UF = SG_UF, 
         MUNICIPIO = NM_UE, 
         CARGO = DS_CARGO,
         CANDIDATO = NM_CANDIDATO, 
         #CPF = NR_CPF_CANDIDATO,
         PARTIDO = SG_PARTIDO, 
         IDEOLOGIA = SG_IDEOLOGIA, 
         SITUACAO = DS_SIT_TOT_TURNO,
         NR_TURNO,
         RESULTADO_2020) %>%
  janitor::clean_names()

writexl::write_xlsx(resultado_1, "data_analysis/resultado_1.xlsx")
