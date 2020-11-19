# leitura de pacotes
library(data.table)
library(tidyverse)

# leitura de arquivos / PREFEITO
resultado_2020_p <- fread("C:/Users/acaesar/Downloads/resultado_candidatos-COE-18nov2020.csv",
                        encoding = "UTF-8",
                        colClasses = c(sq_candidato = "character"),
                        select = c("regiao", "uf", "codigo_municipio_tse", "codigo_municipio_ibge", "nome_municipio", "total_eleitorado", "total_de_votos", "abstencao", "brancos", "nulos", "urnas_apuradas", "populacao", "eleito", "segundo_turno", "total_percentual", "total_percentual_float", "votos_validos", "nr_turno", "nome_candidato", "nome_urna_candidato", "cargo", "genero", "sigla_partido", "nome_candidato_vice", "nome_urna_candidato_vice", "cargo_vice", "sigla_partido_vice", "composicao_coligacao", "DS_DETALHE_SITUACAO_CAND", "sq_candidato", "reeleicao_resultado_2016", "pib_total", "pib_total_milhao", "receita_total", "receita_total_milhao"))

# leitura de arquivos / VEREADORES
resultado_2020_v <- fread("C:/Users/acaesar/Downloads/resultado_final_vereadores_2020.csv",
                          encoding = "UTF-8",
                          colClasses = c(sq_candidato = "character"),
                          select = c("regiao", "uf", "codigo_municipio_tse", "codigo_municipio_ibge", "nome_municipio", "total_eleitorado", "total_de_votos", "abstencao", "brancos", "nulos", "urnas_apuradas", "populacao", "eleito", "segundo_turno", "total_percentual", "total_percentual_float", "votos_validos", "nr_turno", "nome_candidato", "nome_urna_candidato", "cargo", "genero", "sigla_partido", "nome_candidato_vice", "nome_urna_candidato_vice", "cargo_vice", "sigla_partido_vice", "composicao_coligacao", "DS_DETALHE_SITUACAO_CAND", "sq_candidato", "reeleicao_resultado_2016", "pib_total", "pib_total_milhao", "receita_total", "receita_total_milhao"))


coligacao_eleitos_2020_p <- resultado_2020_p %>%
  filter(eleito == "TRUE") %>%
  distinct(codigo_municipio_ibge, .keep_all = TRUE) %>%
  select(codigo_municipio_tse, composicao_coligacao) %>%
  separate(composicao_coligacao, into = c("partido_1", "partido_2", "partido_3", "partido_4", "partido_5", "partido_6", "partido_7", "partido_8", "partido_9", "partido_10", "partido_11", "partido_12", "partido_13", "partido_14", "partido_15", "partido_16", "partido_17", "partido_18", "partido_19", "partido_20"), sep = ",") %>%
  pivot_longer(cols = 2:21, names_to = "partido") %>%
  mutate(value = str_trim(value)) %>%
  filter(!is.na(value)) %>%
  select(-c(partido)) %>%
  rename(partido = value) %>%
  mutate(tipo = "coligacao")

partido_eleitos_2020_p <- resultado_2020_p %>%
  filter(eleito == "TRUE") %>%
  distinct(codigo_municipio_ibge, .keep_all = TRUE) %>%
  select(codigo_municipio_tse, sigla_partido) %>%
  rename(partido = sigla_partido) %>%
  mutate(tipo = "sigla_prefeito")

partido_vice_eleitos_2020_p <- resultado_2020_p %>%
  filter(eleito == "TRUE") %>%
  distinct(codigo_municipio_ibge, .keep_all = TRUE) %>%
  select(codigo_municipio_tse, sigla_partido_vice) %>%
  rename(partido = sigla_partido_vice) %>%
  mutate(tipo = "sigla_vice")

eleitos_2020_v <- resultado_2020_v %>%
  filter(eleito == "S") %>%
  group_by(codigo_municipio_tse, uf, nome_municipio, sigla_partido) %>%
  summarise(int = n())
   
total_eleitos_2020_v <- eleitos_2020_v %>%
  pivot_wider(values_from = int, names_from = sigla_partido) %>%
  replace(is.na(.), 0) %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = T)) %>%
  ungroup() %>%
  select(codigo_municipio_tse, total)
  
final_eleitos_2020_v <- eleitos_2020_v %>%
  rename(partido = sigla_partido) %>%
  left_join(total_eleitos_2020_v, by = "codigo_municipio_tse") %>%
  left_join(coligacao_eleitos_2020_p, by = c("codigo_municipio_tse", "partido")) %>%
  left_join(partido_eleitos_2020_p, by = c("codigo_municipio_tse", "partido")) %>%
  left_join(partido_vice_eleitos_2020_p, by = c("codigo_municipio_tse", "partido")) %>%
  filter(!is.na(`tipo.x`) |
        !is.na(`tipo.y`) |
        !is.na(tipo)) %>%
  group_by(codigo_municipio_tse, nome_municipio, uf, total) %>%
  summarise(vereadores = sum(int)) %>%
  mutate(coalizao = round((vereadores / total), 2) * 100) %>%
  filter(coalizao == 100)
  
  
faixa_eleitos_2020_v <- final_eleitos_2020_v %>%
  mutate(faixa_coalizao = case_when(coalizao >= 75 ~ "igual_maior_3/4",
                                    coalizao >= 50 ~ "igual_maior_2/4",
                                    coalizao >= 25 ~ "igual_maior_1/4",
                                    coalizao < 25 ~ "menor_1/4")) %>%
  group_by(faixa_coalizao) %>%
  summarise(n())
  
mean(final_eleitos_2020_v$coalizao)
median(final_eleitos_2020_v$coalizao)
  