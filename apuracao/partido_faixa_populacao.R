# leitura de pacotes
library(data.table)
library(tidyverse)

############
### 2020 ###
############

# leitura de arquivo
cand_2020_coe <- fread("C:/Users/acaesar/Downloads/resultado_candidatos-COE-19nov2020_n.csv",
                       encoding = "UTF-8")

### PREFEITO
partido_faixa_2020 <- cand_2020_coe %>%
  filter(eleito == "TRUE") %>%
  select(uf, nome_municipio, nome_candidato, cargo, sigla_partido, sigla_partido_vice, eleito, populacao) %>%
  mutate(faixa = case_when(populacao > 500000 ~ "Acima de 500 mil habitantes",
         populacao >= 300001  & populacao <= 500000 ~ "De 300 mil a 500 mil habitantes",
         populacao >= 50001 & populacao <= 300000 ~ "De 50 mil a 300 mil habitantes",
         populacao >= 10000 & populacao <= 50000 ~ "De 10 mil 50 mil habitantes",
         populacao < 10000 ~ "Abaixo de 10 mil habitantes")) %>%
  mutate(id_faixa = case_when(populacao > 500000 ~ "5",
                           populacao >= 300001  & populacao <= 500000 ~ "4",
                           populacao >= 50001 & populacao <= 300000 ~ "3",
                           populacao >= 10000 & populacao <= 50000 ~ "2",
                           populacao < 10000 ~ "1")) %>%
  group_by(sigla_partido, faixa, id_faixa) %>%
  summarise(int = n()) %>%
  rename(SG_PARTIDO = sigla_partido) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "AVANTE", "Avante"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "CIDADANIA", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "NOVO", "Novo"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PATRIOTA", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REDE", "Rede"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REPUBLICANOS", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "SOLIDARIEDADE", "SD")) %>%
  mutate(ano = "2020")

############
### 2016 ###
############

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

partido_faixa_2016 <- resultado_hab_2016 %>%
  mutate(SG_PARTIDO = case_when(codigo_tse == "8931" ~ "PSD",
                                codigo_tse != "8931" ~ SG_PARTIDO)) %>%
  mutate(votos_totais = case_when(codigo_tse == "8931" ~ as.character("2317"),
                                  codigo_tse != "8931" ~ as.character(votos_totais))) %>%
  filter(!is.na(SG_PARTIDO)) %>%
  mutate(populacao = as.integer(pop_est)) %>%
  mutate(faixa = case_when(populacao > 500000 ~ "Acima de 500 mil habitantes",
                           populacao >= 300001  & populacao <= 500000 ~ "De 300 mil a 500 mil habitantes",
                           populacao >= 50001 & populacao <= 300000 ~ "De 50 mil a 300 mil habitantes",
                           populacao >= 10000 & populacao <= 50000 ~ "De 10 mil 50 mil habitantes",
                           populacao < 10000 ~ "Abaixo de 10 mil habitantes")) %>%
  mutate(id_faixa = case_when(populacao > 500000 ~ "5",
                              populacao >= 300001  & populacao <= 500000 ~ "4",
                              populacao >= 50001 & populacao <= 300000 ~ "3",
                              populacao >= 10000 & populacao <= 50000 ~ "2",
                              populacao < 10000 ~ "1")) %>%
  group_by(SG_PARTIDO, faixa, id_faixa) %>%
  summarise(int = n()) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PEN", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PMDB", "MDB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPS", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRB", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PR$", "PL"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PSDC", "DC"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PTN", "PODE"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "REDE", "Rede"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PT do B", "Avante")) %>%
  mutate(ano = "2016")
  
############
### 2012 ###
############

setwd("C:/Users/acaesar/Downloads/populacao/")
class_columns <- c(cd_uf = "character", cd_ue = "character", pop_est = "character")

hab_2013 <- fread("habitantes_2013_municipios.csv", encoding = "Latin-1", colClasses = class_columns, stringsAsFactors = FALSE)

# leitura de arquivo / CONVERSOR
# url: https://github.com/betafcc/Municipios-Brasileiros-TSE/blob/master/municipios_brasileiros_tse.csv

cod_file <- fread("municipios_brasileiros_tse.csv", encoding = "UTF-8", sep = ",", colClasses = c(codigo_ibge = "character"))

# leitura de arquivos / RESULTADO 2012
path_2012 <- "C:/Users/acaesar/Downloads/resultado_eleicoes/votacao_candidato_munzona_2012/"

header <- c("DT_ELEICAO", "HH_GERACAO", "ANO_ELEICAO", "NR_TURNO", 
            "DS_ELEICAO", "SG_UF", "SG_UE", "CD_UE", "NM_UE", 
            "NR_ZONA", "CD_CARGO", "NR_CANDIDATO", "SQ_CANDIDATO", 
            "NM_CANDIDATO", "NM_URNA_CANDIDATO", "DS_CARGO", 
            "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO", "CD_SIT_CANDIDATO",
            "DS_SIT_CANDIDATO", "CD_SIT_CAND_TOT", "DS_SIT_CAND_TOT",
            "NR_PARTIDO", "SG_PARTIDO", "NM_PARTIDO", "SQ_LEGENDA",
            "NM_COLIGACAO", "COMPOSICAO_COLIGACAO", "TOTAL_VOTOS")

resultado_2012 <- map_df(paste0(path_2012, 
                                list.files(path_2012, pattern = "*txt")), 
                         fread, 
                         encoding = "Latin-1", col.names = header)

resultado_2012_n <- resultado_2012 %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(votos_totais = sum(TOTAL_VOTOS))

# leitura de todas as UFs - 2012
# definindo nomes para cabeçalho de DF de 2012, 2008, 2006, 2004 e 2000
names_columns <- c("DT_GERACAO", "HH_GERACAO", "ANO_ELEICAO", 
                   "NR_TURNO", "DS_ELEICAO", "SG_UF", 
                   "SG_UE", "NM_UE", "CD_CARGO", 
                   "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                   "NR_CANDIDATO", "NR_CPF_CANDIDATO", "NM_URNA_CANDIDATO", 
                   "CD_SITUACAO_CANDIDATURA", "DS_SITUACAO_CANDIDATURA", 
                   "NR_PARTIDO", "SG_PARTIDO", "NM_PARTIDO", 
                   "CD_LEGENDA", "SGL_LEGENDA", "DS_COMPOSICAO_COLIGACAO", 
                   "NM_COLIGACAO", "CD_OCUPACAO", "DS_OCUPACAO", 
                   "DT_NASCIMENTO", "NR_TITULO_ELEITORAL_CANDIDATO", 
                   "NR_IDADE_DATA_ELEICAO", "CD_GENERO", "DS_GENERO", 
                   "CD_GRAU_INSTRUCAO", "DS_GRAU_INSTRUCAO", 
                   "CD_ESTADO_CIVIL", "DS_ESTADO_CIVIL", 
                   "CD_NACIONALIDADE", "DS_NACIONALIDADE", 
                   "SG_UF_NASCIMENTO", "CD_MUNICIPIO_NASCIMENTO", 
                   "NM_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", 
                   "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO", "NM_EMAIL")
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

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

path_2012 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2012/"
cand_2012 <- map_df(paste0(path_2012, list.files(path_2012, pattern = "*txt")), fread, 
                    encoding = "Latin-1", col.names = names_columns, drop = drop_columns, colClasses = class_columns)


eleitos_1turno_2012 <- cand_2012 %>%
  filter(DS_ELEICAO == "ELEIÇÃO MUNICIPAL 2012") %>%
  filter(NR_TURNO == "1") %>%
  filter(DS_CARGO == "PREFEITO") %>%
  filter(DS_SIT_TOT_TURNO == "ELEITO") %>%
  select(NM_URNA_CANDIDATO, SQ_CANDIDATO, SG_PARTIDO, SG_UF, SG_UE, NM_UE, DS_SITUACAO_CANDIDATURA)

partido_faixa_2012 <- hab_2013 %>%
  filter(nm_ue != "Brasília" &
           nm_ue != "Fernando de Noronha") %>%
  mutate(nm_ue_n = abjutils::rm_accent(toupper(nm_ue)),
         cd_ue = str_pad(cd_ue, 5, pad = 0)) %>%
  unite(cd_ibge, c(cd_uf, cd_ue), sep = "") %>%
  left_join(cod_file, by = c("cd_ibge" = "codigo_ibge")) %>%
  left_join(eleitos_1turno_2012, by = c("codigo_tse" = "SG_UE")) %>%
  filter(!is.na(SG_UF)) %>%
  mutate(populacao = as.integer(pop_est)) %>%
  mutate(faixa = case_when(populacao > 500000 ~ "Acima de 500 mil habitantes",
                           populacao >= 300001  & populacao <= 500000 ~ "De 300 mil a 500 mil habitantes",
                           populacao >= 50001 & populacao <= 300000 ~ "De 50 mil a 300 mil habitantes",
                           populacao >= 10000 & populacao <= 50000 ~ "De 10 mil 50 mil habitantes",
                           populacao < 10000 ~ "Abaixo de 10 mil habitantes")) %>%
  mutate(id_faixa = case_when(populacao > 500000 ~ "5",
                              populacao >= 300001  & populacao <= 500000 ~ "4",
                              populacao >= 50001 & populacao <= 300000 ~ "3",
                              populacao >= 10000 & populacao <= 50000 ~ "2",
                              populacao < 10000 ~ "1")) %>%
  left_join(resultado_2012_n, by = "SQ_CANDIDATO") %>%
  group_by(SG_PARTIDO, faixa, id_faixa) %>%
  summarise(int = n()) %>%
  mutate(SG_PARTIDO = str_replace_all(SG_PARTIDO, "PC do B", "PCdoB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PEN", "Patriota"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PMDB", "MDB"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PPS", "Cidadania"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PRB", "Republicanos"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PR$", "PL"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PSDC", "DC"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PTN", "PODE"),
         SG_PARTIDO = str_replace_all(SG_PARTIDO, "PT do B", "Avante")) %>%
mutate(ano = "2012")

##########################
### 2012 + 2016 + 2020 ###
##########################

partido_faixa_all <- partido_faixa_2020 %>%
  rbind(partido_faixa_2016, partido_faixa_2012) %>%
  #split(.$SG_PARTIDO) %>%
  #.[[1]] %>%
  pivot_wider(names_from = ano, values_from = int) %>%
  replace(is.na(.), 0) %>%
  select(SG_PARTIDO, faixa, id_faixa, "2012", "2016", "2020") %>%
  arrange(SG_PARTIDO, id_faixa)

write.csv(partido_faixa_all, "partido_faixa_all.csv")
