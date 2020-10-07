# ler pacotes
library(tidyverse)
library(data.table)

# definir pasta
setwd("C:/Users/acaesar/Documents/pesquisas-eleitorais/RJ/RJ/")
list.files()

# ler arquivo
t <- fread("LINHA_RiodeJaneiro_IDADE_16A24.csv", header = FALSE,
           encoding = "UTF-8")

teste <- t %>%
  pivot_longer(cols = everything()) %>%
  rename(coluna = name, candidato = value) %>%
  arrange(coluna) %>%
  mutate(percentual = case_when(str_detect(candidato, "^[0-9]*$") ~ as.integer(candidato))) %>%
  mutate(data = case_when(str_detect(candidato, "2020") ~ candidato)) %>%
  fill(data, .direction = "down") %>%
  filter(candidato != data) %>%
  fill(percentual, .direction = "up") %>%
  filter(candidato != percentual) %>%
  mutate(nao_candidato = case_when(str_detect(candidato, "/") ~ -1)) %>%
  mutate(nao_candidato = replace_na(nao_candidato, 0)) %>%
  arrange(desc(percentual)) %>%
  arrange(desc(nao_candidato)) %>%
  mutate(ordem = row_number()) %>%
  select(data, candidato, percentual, ordem)
  

partido_correcao <- teste %>%
  mutate(candidato = str_replace_all(candidato, "NOVO", "(Novo)"),
         candidato = str_replace_all(candidato, "REDE", "(Rede)"),
         candidato = str_replace_all(candidato, "MDB", "(MDB)"),
         candidato = str_replace_all(candidato, "PROS", "(PROS)"),
         candidato = str_replace_all(candidato, "DEM", "(DEM)"),
         candidato = str_replace_all(candidato, "PSL", "(PSL)"),
         candidato = str_replace_all(candidato, "PSTU", "(PSTU)"),
         candidato = str_replace_all(candidato, "PT", "(PT)"),
         candidato = str_replace_all(candidato, "PSC", "(PSC)"),
         candidato = str_replace_all(candidato, "PSOL", "(PSOL)"),
         candidato = str_replace_all(candidato, "PCO", "(PCO)"),
         candidato = str_replace_all(candidato, "PDT", "(PDT)"),
         candidato = str_replace_all(candidato, "REPUBLICANOS", "(Republicanos)"),
         candidato = str_replace_all(candidato, "PMB", "(PMB)"))





