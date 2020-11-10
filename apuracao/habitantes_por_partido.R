# leitura de pacotes
library(tidyverse)
library(data.table)

# leitura de arquivo / HABITANTES
setwd("C:/Users/acaesar/Downloads/populacao/")
class_columns <- c(cd_uf = "character", cd_ue = "character", pop_est = "character")

hab_2016 <- fread("habitantes_2016_municipios.csv", encoding = "Latin-1", colClasses = class_columns)

# leitura de arquivo / CONVERSOR
# url: https://github.com/betafcc/Municipios-Brasileiros-TSE/blob/master/municipios_brasileiros_tse.csv
cod_file <- fread("https://raw.githubusercontent.com/betafcc/Municipios-Brasileiros-TSE/master/municipios_brasileiros_tse.csv",
                  encoding = "UTF-8", sep = ",", colClasses = c(codigo_ibge = "character"))

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

resultado_hab_2016 <- hab_2016 %>%
  filter(nm_ue != "Brasília" &
         nm_ue != "Fernando de Noronha") %>%
  mutate(nm_ue_n = abjutils::rm_accent(toupper(nm_ue)),
         cd_ue = str_pad(cd_ue, 5, pad = 0)) %>%
  unite(cd_ibge, c(cd_uf, cd_ue), sep = "") %>%
  left_join(cod_file, by = c("cd_ibge" = "codigo_ibge")) %>%
  left_join(resultado_2016_n, by = c("codigo_tse" = "CD_MUNICIPIO")) %>%
  arrange(desc(NM_UE_n))

write.csv(resultado_hab_2016, "resultado_hab_2016.csv")

