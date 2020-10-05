# leitura de pacotes
library(tidyverse)
library(data.table)

# colunas de interesse
select_column <- c("SG_UF", "NM_MUNICIPIO", "NR_ZONA", "NR_SECAO", "CD_LOCAL_VOTACAO")

# caminho para os arquivos
path <- "C:/Users/acaesar/Downloads/perfil_eleitor_secao_2020_BR/BRASIL/"

# leitura dos arquivos
secao_eleitoral <- map_df(paste0(path, list.files(path, pattern = "*csv")), fread, 
            encoding = "Latin-1", select = select_column)

# salva e lê como RDS
saveRDS(secao_eleitoral, file = "secao_eleitoral_2020.rds")
secao_eleitoral_rds <- readRDS("secao_eleitoral_2020.rds")

# verifica número de seções por UF
t <- secao_eleitoral_rds %>%
  distinct(SG_UF, NR_ZONA, NR_SECAO, .keep_all = T) %>%
  group_by(SG_UF, NR_SECAO) %>%
  summarize(int = n()) %>%
  group_by(SG_UF) %>%
  summarize(qt = sum(int)) %>%
  mutate(mesario = qt * 4)

