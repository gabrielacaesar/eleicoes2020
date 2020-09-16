library(tidyverse)
library(data.table)

getwd()
setwd("~/Downloads/perfil_eleitorado_total/")

# atual
eleitorado_atual <- fread("perfil_eleitorado_ATUAL.csv", encoding = "Latin-1")

eleitorado_atual_n <- eleitorado_atual %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(qt_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(df = "atual")

# 2020
eleitorado_2020 <- fread("perfil_eleitorado_2020.csv", encoding = "Latin-1")

eleitorado_2020_n <- eleitorado_2020 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(qt_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(df = "2020")

# 2018
eleitorado_2018 <- fread("perfil_eleitorado_2018.csv", encoding = "Latin-1")

eleitorado_2018_n <- eleitorado_2018 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(qt_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(df = "2018")

# 2016
eleitorado_2016 <- fread("perfil_eleitorado_2016.txt", encoding = "Latin-1")

eleitorado_2016_n <- eleitorado_2016 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2016") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2014
eleitorado_2014 <- fread("perfil_eleitorado_2014.txt", encoding = "Latin-1")

eleitorado_2014_n <- eleitorado_2014 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2014") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2012 
eleitorado_2012 <- fread("perfil_eleitorado_2012.csv", encoding = "Latin-1")

eleitorado_2012_n <- eleitorado_2012 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(DS_FAIXA_ETARIA) %>%
  summarise(qt_eleitores = sum(QT_ELEITORES_PERFIL)) %>%
  mutate(df = "2012")
  
# 2010
eleitorado_2010 <- fread("perfil_eleitorado_2010.txt", encoding = "Latin-1")

eleitorado_2010_n <- eleitorado_2010 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2010") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# binding dfs
eleitorado <- eleitorado_atual_n %>%
  rbind(eleitorado_2010_n) %>%
  rbind(eleitorado_2012_n) %>%
  rbind(eleitorado_2014_n) %>%
  rbind(eleitorado_2016_n) %>%
  rbind(eleitorado_2018_n) %>%
  rbind(eleitorado_2020_n)

# downloading 
write.csv(eleitorado, "eleitorado.csv")

