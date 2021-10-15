# 3 - prefeitos que venceram as eleições e foram reeleitos
# e quem é "novo" em 2020 e 2016
# Para especificar o que busco encontrar no item 3: 
# buscando olhar quem são os "novos" candidatos que
# passam a disputar espaços com caras já conhecidas em 2020, 
# ou seja, que já tinham concorrido antes ou estavam no cargo?
## Sobre a questão da reeleição, é muito difícil uma base que
# especifique quem tá se candidatando a reeleição igual 
# na disputa anterior ou quem assumiu como vice e agora está
# tentando candidatura, né? 
# Se não tiver mesmo, deixa de lado e eu tento manter 
# o foco no partido eleito pra justificar teoricamente.
# Item 3: A prioridade é 2016. Seria interessante ter essa 
# retomada histórica, mas não sei o quanto isso vai aumentar 
# demais o trabalho e o tempo. Se tiver isso mais fácil e
# achar que rola, eu ia amar!!!

# leitura de bibliotecas e R scripts
library(tidyverse)
library(data.table)
library(writexl)
source('scripts/conversao_ideologia.R') # importa script para definir ideologias
source('scripts/conversao_partido.R') # importa script para corrigir partidos

# leitura regioes do BR 
regioes_br <- fread("data_analysis/regioes.csv")

# colunas para manter
columns_selected <- c("DS_ELEICAO",                   
                      "SG_UF",                        
                      "NM_UE",                        
                      "DS_CARGO",                     
                      "NM_CANDIDATO",                 
                      "NR_CPF_CANDIDATO",             
                      "SG_PARTIDO",                   
                      "DS_SIT_TOT_TURNO")

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character")

# header
columns_name <- c("DS_ELEICAO", 
                  "UF", 
                  "CIDADE", 
                  "CARGO", 
                  "CANDIDATO", 
                  "CPF", 
                  "PARTIDO", 
                  "SITUACAO")

# leitura de BRASIL - 2020
cand_2020 <- fread("data_tse/consulta_cand_2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2020) <- paste0(colnames(cand_2020), "_2020") 

# leitura de BRASIL - 2018
cand_2018 <- fread("data_tse/consulta_cand_2018/consulta_cand_2018_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2018) <- paste0(colnames(cand_2018), "_2018") 

# leitura de BRASIL - 2016
cand_2016 <- fread("data_tse/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2016) <- paste0(colnames(cand_2016), "_2016") 

# leitura de BRASIL - 2014
cand_2014 <- fread("data_tse/consulta_cand_2014/consulta_cand_2014_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2014) <- paste0(colnames(cand_2014), "_2014") 

# leitura de BRASIL - 2012
cand_2012 <- fread("data_tse/consulta_cand_2012/consulta_cand_2012_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2012) <- paste0(colnames(cand_2012), "_2012") 

# leitura de BRASIL - 2010
cand_2010 <- fread("data_tse/consulta_cand_2010/consulta_cand_2010_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2010) <- paste0(colnames(cand_2010), "_2010") 

# leitura de BRASIL - 2008
cand_2008 <- fread("data_tse/consulta_cand_2008/consulta_cand_2008_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2008) <- paste0(colnames(cand_2008), "_2008") 

# leitura de BRASIL - 2006
cand_2006 <- fread("data_tse/consulta_cand_2006/consulta_cand_2006_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2006) <- paste0(colnames(cand_2006), "_2006") 

# leitura de BRASIL - 2004
cand_2004 <- fread("data_tse/consulta_cand_2004/consulta_cand_2004_BRASIL.csv",
                    encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2004) <- paste0(colnames(cand_2004), "_2004") 

# leitura de BRASIL - 2002
cand_2002 <- fread("data_tse/consulta_cand_2002/consulta_cand_2002_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2002) <- paste0(colnames(cand_2002), "_2002") 

# leitura de BRASIL - 2000
cand_2000 <- fread("data_tse/consulta_cand_2000/consulta_cand_2000_BRASIL.csv",
                   encoding = "Latin-1", 
                   select = columns_selected, 
                   colClasses = class_columns,
                   col.names = columns_name)
colnames(cand_2000) <- paste0(colnames(cand_2000), "_2000") 

# historico de prefeitos eleitos em 2020
resultado_3 <- cand_2020 %>%
  filter(str_detect(DS_ELEICAO_2020, "Eleições Municipais 2020")) %>% # desconsidera eleicao suplementar
  filter(CARGO_2020 == "PREFEITO") %>%
  filter(SITUACAO_2020 == "ELEITO") %>%
  rename(SG_PARTIDO = PARTIDO_2020) %>%
  corrigir_partidos() %>%
  definir_ideologia() %>%
  rename(PARTIDO_2020 = SG_PARTIDO,
         IDEOLOGIA_2020 = SG_IDEOLOGIA) %>%
  left_join(cand_2018, by = c("CPF_2020" = "CPF_2018")) %>%
  replace(is.na(.), "-") %>%
  rename(SG_PARTIDO = PARTIDO_2018) %>%
  corrigir_partidos() %>%
  definir_ideologia() %>%
  rename(PARTIDO_2018 = SG_PARTIDO,
         IDEOLOGIA_2018 = SG_IDEOLOGIA) %>%
  left_join(cand_2016, by = c("CPF_2020" = "CPF_2016")) %>%
  replace(is.na(.), "-") %>%
  rename(SG_PARTIDO = PARTIDO_2016) %>%
  corrigir_partidos() %>%
  definir_ideologia() %>%
  rename(PARTIDO_2016 = SG_PARTIDO,
         IDEOLOGIA_2016 = SG_IDEOLOGIA) %>%
  left_join(cand_2014, by = c("CPF_2020" = "CPF_2014")) %>%
  left_join(cand_2012, by = c("CPF_2020" = "CPF_2012")) %>%
  left_join(cand_2010, by = c("CPF_2020" = "CPF_2010")) %>%
  left_join(cand_2008, by = c("CPF_2020" = "CPF_2008")) %>%
  left_join(cand_2006, by = c("CPF_2020" = "CPF_2006")) %>%
  left_join(cand_2004, by = c("CPF_2020" = "CPF_2004")) %>%
  left_join(cand_2002, by = c("CPF_2020" = "CPF_2002")) %>%
  left_join(cand_2000, by = c("CPF_2020" = "CPF_2000")) %>%
  select(UF_2020, CIDADE_2020, CANDIDATO = CANDIDATO_2020,
         PARTIDO_2020, IDEOLOGIA_2020, SITUACAO_2020, CARGO_2018, PARTIDO_2018, IDEOLOGIA_2018, SITUACAO_2018,
         CARGO_2016, PARTIDO_2016, IDEOLOGIA_2016, SITUACAO_2016, CARGO_2014, PARTIDO_2014, SITUACAO_2014,
         CARGO_2012, PARTIDO_2012, SITUACAO_2012, CARGO_2010, PARTIDO_2010, SITUACAO_2010,
         CARGO_2008, PARTIDO_2008, SITUACAO_2008, CARGO_2006, PARTIDO_2006, SITUACAO_2006,
         CARGO_2004, PARTIDO_2004, SITUACAO_2004, CARGO_2002, PARTIDO_2002, SITUACAO_2002,
         CARGO_2000, PARTIDO_2000, SITUACAO_2000) %>%
  replace(is.na(.), "-") %>%
  mutate(reeleito = case_when(SITUACAO_2016 == "ELEITO" & CARGO_2016 == "PREFEITO" ~ "Sim",
                              TRUE ~ "Não")) %>%
  relocate(reeleito, .after = "SITUACAO_2020") %>%
  left_join(regioes_br, by = c("UF_2020" = "uf")) %>%
  relocate(regiao, .before = "UF_2020") %>%
  janitor::clean_names()
  
writexl::write_xlsx(resultado_3, "data_analysis/resultado_3.xlsx")
