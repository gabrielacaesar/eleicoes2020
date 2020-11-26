# leitura de pacotes
library(data.table)
library(tidyverse)

# leitura de arquivos / PREFEITO DE CAPITAIS
resultado_2020_p <- fread("C:/Users/acaesar/Downloads/eleitos_capitais.csv",
                          encoding = "UTF-8",
                          colClasses = c(codigo_municipio_ibge = "character"))


# leitura de arquivos / VEREADORES
resultado_2020_v <- fread("C:/Users/acaesar/Downloads/resultado_final_vereadores_2020.csv",
                          encoding = "UTF-8",
                          colClasses = c(sq_candidato = "character", codigo_municipio_ibge = "character"),
                          select = c("regiao", "uf", "codigo_municipio_tse", "codigo_municipio_ibge", "nome_municipio", "total_eleitorado", "total_de_votos", "abstencao", "brancos", "nulos", "urnas_apuradas", "populacao", "eleito", "segundo_turno", "total_percentual", "total_percentual_float", "votos_validos", "nr_turno", "nome_candidato", "nome_urna_candidato", "cargo", "genero", "sigla_partido", "nome_candidato_vice", "nome_urna_candidato_vice", "cargo_vice", "sigla_partido_vice", "composicao_coligacao", "DS_DETALHE_SITUACAO_CAND", "sq_candidato", "reeleicao_resultado_2016", "pib_total", "pib_total_milhao", "receita_total", "receita_total_milhao"))


# prefeitos
coligacao_eleitos_2020_p <- resultado_2020_p %>%
  select(codigo_municipio_ibge, composicao_coligacao) %>%
  separate(composicao_coligacao, into = c("partido_1", "partido_2", "partido_3", "partido_4", "partido_5", "partido_6", "partido_7", "partido_8", "partido_9", "partido_10", "partido_11", "partido_12", "partido_13", "partido_14", "partido_15", "partido_16", "partido_17", "partido_18", "partido_19", "partido_20"), sep = ",") %>%
  pivot_longer(cols = 2:21, names_to = "partido") %>%
  mutate(value = str_trim(value)) %>%
  filter(!is.na(value)) %>%
  select(-c(partido)) %>%
  rename(partido = value) %>%
  mutate(tipo = "coligacao") 
  
# vereadores
eleitos_2020_v <- resultado_2020_v %>%
  filter(eleito == "S") %>%
  group_by(codigo_municipio_ibge, uf, nome_municipio, sigla_partido) %>%
  summarise(int = n()) %>%
  mutate(sigla_partido = str_replace_all(sigla_partido, "AVANTE", "Avante"),
         sigla_partido = str_replace_all(sigla_partido, "CIDADANIA", "Cidadania"),
         sigla_partido = str_replace_all(sigla_partido, "NOVO", "Novo"),
         sigla_partido = str_replace_all(sigla_partido, "PATRIOTA", "Patriota"),
         sigla_partido = str_replace_all(sigla_partido, "PC do B", "PCdoB"),
         sigla_partido = str_replace_all(sigla_partido, "REDE", "Rede"),
         sigla_partido = str_replace_all(sigla_partido, "REPUBLICANOS", "Republicanos"),
         sigla_partido = str_replace_all(sigla_partido, "SOLIDARIEDADE", "SD")) %>%
  filter(codigo_municipio_ibge == "2800308" |
           codigo_municipio_ibge == "1400100" |
           codigo_municipio_ibge == "1400100" |
           codigo_municipio_ibge == "5103403" |
           codigo_municipio_ibge == "2304400" |
           codigo_municipio_ibge == "5208707" |
           codigo_municipio_ibge == "2507507" |
           codigo_municipio_ibge == "2704302" |
           codigo_municipio_ibge == "1302603" |
           codigo_municipio_ibge == "4314902" |
           codigo_municipio_ibge == "1100205" |
           codigo_municipio_ibge == "2611606" |
           codigo_municipio_ibge == "1200401" |
           codigo_municipio_ibge == "3304557" |
           codigo_municipio_ibge == "2111300" |
           codigo_municipio_ibge == "3550308" |
           codigo_municipio_ibge == "2211001" |
           codigo_municipio_ibge == "3205309")

qt_vagas <- eleitos_2020_v %>%
  pivot_wider(values_from = int, names_from = sigla_partido) %>%
  replace(is.na(.), 0) %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = T)) %>%
  ungroup() %>%
  select(codigo_municipio_ibge, total)

final_eleitos_2020_v <- eleitos_2020_v %>%
  rename(partido = sigla_partido) %>%
  left_join(qt_vagas, by = "codigo_municipio_ibge") %>%
  left_join(coligacao_eleitos_2020_p, by = c("codigo_municipio_ibge", "partido")) 
  
coalizao_pref <- final_eleitos_2020_v %>%
  filter(!is.na(tipo)) %>%
  group_by(codigo_municipio_ibge, nome_municipio, uf, total) %>%
  summarise(vereadores = sum(int)) %>%
  mutate(coalizao = round((vereadores / total), 2) * 100)
