library(tidyverse)
library(data.table)

### 2020
eleitorado_2020 <- fread("C:/Users/acaesar/Downloads/eleitorado/perfil_eleitorado_2020.csv", 
                         encoding = "Latin-1")

eleitorado_2020_n <- eleitorado_2020 %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, DS_GENERO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_GENERO, values_from = total) %>%
  replace(is.na(.), 0) %>%
  janitor::clean_names() %>%
  mutate(total = feminino + masculino + nao_informado,
         fem_perc = round((feminino / total) * 100),
         mas_perc = round((masculino / total) * 100),
         diferenca = feminino - masculino,
         mais_mulher = feminino > masculino) %>%
  arrange(desc(fem_perc)) 
  # filter(mais_mulher == TRUE)

### 2016
eleitorado_2016 <- fread("C:/Users/acaesar/Downloads/eleitorado/perfil_eleitorado_2016.txt", 
                         encoding = "Latin-1")

eleitorado_2016_n <- eleitorado_2016 %>%
  rename(SG_UF = V2, NM_MUNICIPIO = V3, CD_MUNICIPIO = V4, 
         DS_GENERO = V6, QT_ELEITORES_PERFIL = V9) %>%
  filter(SG_UF != "ZZ" & SG_UF != "DF") %>%
  filter(NM_MUNICIPIO != "FERNANDO DE NORONHA") %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, DS_GENERO) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_GENERO, values_from = total) %>%
  replace(is.na(.), 0) %>%
  janitor::clean_names() %>%
  mutate(total = feminino + masculino + nao_informado,
         fem_perc = round((feminino / total) * 100),
         mas_perc = round((masculino / total) * 100),
         diferenca = feminino - masculino,
         mais_mulher = feminino > masculino) %>%
  arrange(desc(fem_perc)) 
  # filter(mais_mulher == TRUE)

# 2016 + 2020
eleitorado_2016_2020 <- eleitorado_2020_n %>%
  left_join(eleitorado_2016_n, by = "cd_municipio") %>%
  filter(is.na(`total.y`))
