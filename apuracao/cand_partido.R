# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivos
# url 1: https://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
# url 2: https://www.tse.jus.br/imprensa/noticias-tse/2020/Setembro/eleicoes-2020-95-municipios-com-mais-de-200-mil-eleitores-poderao-ter-2o-turno-em-novembro
resultado_2016 <- fread("C:/Users/acaesar/Downloads/resultado_eleicoes/votacao_candidato_munzona_2016/votacao_candidato_munzona_2016_BRASIL.csv",
                        select = c("DS_ELEICAO", "NR_TURNO", "SG_UF", "NM_UE", "CD_MUNICIPIO", "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                                   "DS_SITUACAO_CANDIDATURA", "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", "DS_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS"))

g_95 <- fread("C:/Users/acaesar/Downloads/resultado_eleicoes/lista-cidades-G95.csv",
              encoding = "UTF-8")

# eleitos no 1 turno + eleitos no 2 turno
eleitos_1_turno <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_1_turno = sum(QT_VOTOS_NOMINAIS)) %>%
  mutate(votos_2_turno == "nao_teve_2turno")

# eleitos no 2 turno
eleitos_2_turno <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "2") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_2_turno = sum(QT_VOTOS_NOMINAIS)) 

eleitos_2_turno_n <- resultado_2016 %>%
  filter(DS_ELEICAO == "ELEIÇÕES MUNICIPAIS 2016") %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "Prefeito") %>%
  filter(DS_SIT_TOT_TURNO == "2º TURNO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF, CD_MUNICIPIO) %>%
  summarise(votos_1_turno = sum(QT_VOTOS_NOMINAIS)) %>%
  left_join(eleitos_2_turno, by = c("CD_MUNICIPIO", "NM_CANDIDATO")) %>%
  filter(!is.na(`NM_UE.y`)) %>%
  select(-c(`SG_PARTIDO.y`, `NM_UE.y`, `SG_UF.y`)) %>%
  rename(SG_PARTIDO = `SG_PARTIDO.x`, NM_UE = `NM_UE.x`, SG_UF = `SG_UF.x`)

# juntando os arquivos
total_eleitos_1_turno <- eleitos_1_turno %>%
  rbind(eleitos_2_turno_n) %>%
  left_join(g_95, by = c("SG_UF" = "UF", "NM_UE" = "MUNICÍPIO")) %>%
  filter(!is.na(ELEITORADO)) %>%
  mutate(ELEITORADO = str_remove_all(ELEITORADO, "\\."),
         ELEITORADO = as.integer(ELEITORADO)) %>%
  arrange(desc(ELEITORADO))
  
write.csv(total_eleitos_1_turno, "total_eleitos_1_turno.csv")




