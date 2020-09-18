library(tidyverse)
library(data.table)

getwd()
setwd("~/Downloads/perfil_eleitorado_total/")

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

# 2008 
eleitorado_2008 <- fread("perfil_eleitorado_2008.txt", encoding = "Latin-1")

eleitorado_2008_n <- eleitorado_2008 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2008") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2006
eleitorado_2006 <- fread("perfil_eleitorado_2006.txt", encoding = "Latin-1")

eleitorado_2006_n <- eleitorado_2006 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2006") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2004 
eleitorado_2004 <- fread("perfil_eleitorado_2004.txt", encoding = "Latin-1")

eleitorado_2004_n <- eleitorado_2004 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2004") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2002
eleitorado_2002 <- fread("perfil_eleitorado_2002.txt", encoding = "Latin-1")

eleitorado_2002_n <- eleitorado_2002 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2002") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 2000
eleitorado_2000 <- fread("perfil_eleitorado_2000.txt", encoding = "Latin-1")

eleitorado_2000_n <- eleitorado_2000 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "2000") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 1998
eleitorado_1998 <- fread("perfil_eleitorado_1998.txt", encoding = "Latin-1")

eleitorado_1998_n <- eleitorado_1998 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "1998") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 1996
eleitorado_1996 <- fread("perfil_eleitorado_1996.txt", encoding = "Latin-1")

eleitorado_1996_n <- eleitorado_1996 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "1996") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# 1994
eleitorado_1994 <- fread("perfil_eleitorado_1994.txt", encoding = "Latin-1")

eleitorado_1994_n <- eleitorado_1994 %>%
  select(V2, V3, V7, V9) %>%
  group_by(V7) %>%
  summarise(qt_eleitores = sum(V9)) %>%
  mutate(df = "1994") %>%
  rename("DS_FAIXA_ETARIA" = V7)

# binding dfs
eleitorado_1994_2020 <- eleitorado_2020_n %>%
  rbind(eleitorado_2018_n) %>%
  rbind(eleitorado_2016_n) %>%
  rbind(eleitorado_2014_n) %>%
  rbind(eleitorado_2012_n) %>%
  rbind(eleitorado_2010_n) %>%
  rbind(eleitorado_2008_n) %>%
  rbind(eleitorado_2006_n) %>%
  rbind(eleitorado_2004_n) %>%
  rbind(eleitorado_2002_n) %>%
  rbind(eleitorado_2000_n) %>%
  rbind(eleitorado_1998_n) %>%
  rbind(eleitorado_1996_n) %>%
  rbind(eleitorado_1994_n)

####### 
# 2020 - UFs

UFs_2020 <- eleitorado_2020 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, DS_FAIXA_ETARIA) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_FAIXA_ETARIA, values_from = soma)

write.csv(UFs_2020, "UFs_2020.csv")

# 2020 - cidades

cidades_2020 <- eleitorado_2020 %>%
  select(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, NM_MUNICIPIO, DS_FAIXA_ETARIA) %>%
  summarise(soma = sum(QT_ELEITORES_PERFIL)) %>%
  pivot_wider(names_from = DS_FAIXA_ETARIA, values_from = soma)

write.csv(cidades_2020, "cidades_2020.csv")
