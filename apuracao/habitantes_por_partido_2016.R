# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivo / HABITANTES
# considera habitantes em 2017
# quando acontece a posse
# em razão da criação de municípios
setwd("C:/Users/acaesar/Downloads/populacao/")
class_columns <- c(cd_uf = "character", cd_ue = "character", pop_est = "character")

hab_2017 <- fread("habitantes_2017_municipios.csv", encoding = "Latin-1", colClasses = class_columns, stringsAsFactors = FALSE)

# leitura de arquivo / CONVERSOR
# url: https://github.com/betafcc/Municipios-Brasileiros-TSE/blob/master/municipios_brasileiros_tse.csv

cod_file <- fread("municipios_brasileiros_tse.csv", encoding = "UTF-8", sep = ",", colClasses = c(codigo_ibge = "character"))

# leitura de arquivos / RESULTADO 
setwd("C:/Users/acaesar/Downloads/resultado_eleicoes/")

resultado_2016 <- fread("votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.csv",
                        select = c("DS_ELEICAO", "NR_TURNO", "SG_UF", "NM_UE", "CD_MUNICIPIO", "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                                   "DS_SITUACAO_CANDIDATURA", "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS"))

resultado_2016_n <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_totais = sum(QT_VOTOS_NOMINAIS)) %>%
  mutate(NM_UE_n = abjutils::rm_accent(toupper(NM_UE)))

resultado_hab_2016 <- hab_2017 %>%
  filter(nm_ue != "Brasília" &
           nm_ue != "Fernando de Noronha") %>%
  mutate(nm_ue_n = abjutils::rm_accent(toupper(nm_ue)),
         cd_ue = str_pad(cd_ue, 5, pad = 0)) %>%
  unite(cd_ibge, c(cd_uf, cd_ue), sep = "") %>%
  left_join(cod_file, by = c("cd_ibge" = "codigo_ibge")) %>%
  left_join(resultado_2016_n, by = c("codigo_tse" = "CD_MUNICIPIO")) %>%
  arrange(desc(NM_UE_n)) 

#write.csv(resultado_hab_2016, "resultado_hab_2016.csv")

hab_partido_2016 <- resultado_hab_2016 %>%
  mutate(SG_PARTIDO = case_when(codigo_tse == "8931" ~ "PSD",
                                codigo_tse != "8931" ~ SG_PARTIDO)) %>%
  mutate(votos_totais = case_when(codigo_tse == "8931" ~ as.character("2317"),
                                  codigo_tse != "8931" ~ as.character(votos_totais))) %>%
  filter(!is.na(SG_PARTIDO)) %>%
  group_by(SG_PARTIDO) %>%
  summarise(votos = sum(as.integer(votos_totais)),
            pop_estimada = sum(as.integer(pop_est)),
            pref_count = n())

#write.csv(hab_partido_2016, "hab_partido_2016.csv")


####
# CHECAGEM DE 2 TURNO + OUTROS
eleito_2turno_2016 <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "2") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_totais = sum(QT_VOTOS_NOMINAIS)) %>%
  mutate(NM_UE_n = abjutils::rm_accent(toupper(NM_UE)))

sem_dado_1turno_2016 <- resultado_hab_2016 %>%
  filter(is.na(NM_UE_n)) %>%
  select(`uf.x`, nm_ue, pop_est, nm_ue_n, codigo_tse, capital) %>%
  left_join(eleito_2turno_2016, by = c("codigo_tse" = "CD_MUNICIPIO")) %>%
  filter(is.na(NM_UE_n)) %>%
  left_join(resultado_2016_pref, by = c("codigo_tse" = "CD_MUNICIPIO"))

resultado_2016_pref <- resultado_2016 %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  filter(DS_ELEICAO != "ELEIÇÕES MUNICIPAIS 2016")

