# lendo os pacotes
library(tidyverse)
library(data.table)

# listando os arquivos CSV
eleitor_def_20 <- list.files("~/Downloads/perfil_eleitor_deficiencia_2020/", pattern = "*.csv")
eleitor_def_18 <- list.files("~/Downloads/perfil_eleitor_deficiencia_2018/", pattern = "*.csv")

# criando DF com todas as UFs
eleitorado_def_2020 <- map_df(paste0("~/Downloads/perfil_eleitor_deficiencia_2020/", eleitor_def_20), fread, encoding = "Latin-1")
eleitorado_def_2018 <- map_df(paste0("~/Downloads/perfil_eleitor_deficiencia_2018/", eleitor_def_18), fread, encoding = "Latin-1")

# dados de 2018
df_eleitorado_2018 <- eleitorado_def_2018 %>%
  group_by(CD_MUNICIPIO, NM_MUNICIPIO, SG_UF, DS_TIPO_DEFICIENCIA) %>%
  summarize(contagem = n()) %>%
  # arrange(desc(contagem)) %>%
  pivot_wider(names_from = DS_TIPO_DEFICIENCIA, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  janitor::clean_names() %>%
  mutate(total = outros + deficiencia_de_locomocao + deficiencia_visual
         + dificuldade_para_o_exercicio_do_voto + deficiencia_auditiva) %>%
  filter(sg_uf != "ZZ" & sg_uf != "DF") %>%
  filter(nm_municipio != "FERNANDO DE NORONHA")

# dados de 2020 
df_eleitorado_2020 <- eleitorado_def_2020 %>%
  group_by(DS_TIPO_DEFICIENCIA) %>%
  summarize(contagem = n()) %>%
  # arrange(desc(contagem))
  pivot_wider(names_from = DS_TIPO_DEFICIENCIA, values_from = contagem) %>%
  replace(is.na(.), 0) %>%
  janitor::clean_names() %>%
  mutate(total = outros + deficiencia_de_locomocao + deficiencia_visual
         + dificuldade_para_o_exercicio_do_voto + deficiencia_auditiva) 

# eleitorado total 2020
eleitorado_2020 <- fread("~/Downloads/perfil_eleitorado_2020/perfil_eleitorado_2020.csv", encoding = "Latin-1")

eleitorado_2020_n <- eleitorado_2020 %>%
  group_by(NM_MUNICIPIO, SG_UF, CD_MUNICIPIO) %>%
  summarize(total = sum(QT_ELEITORES_PERFIL)) %>%
  janitor::clean_names()

# dados de 2018 e 2020 + eleitorado / municipio
df_eleitorado_MUN <- df_eleitorado_2020 %>%
  left_join(df_eleitorado_2018, by = "cd_municipio") %>%
  select(`sg_uf.x`, cd_municipio, `nm_municipio.x`, `total.x`, `total.y`) %>%
  replace(is.na(.), 0) %>%
  rename(n2020 = `total.x`,
         n2018 = `total.y`) %>%
  mutate(diferenca = n2020 - n2018) %>%
  left_join(eleitorado_2020_n, by = "cd_municipio") %>%
  select(sg_uf, nm_municipio, cd_municipio, n2020, n2018, diferenca, total) %>%
  mutate(variacao = (n2020 - n2018) / n2018,
         n_2020_perc = round((n2020 / total) * 100)) %>%
  arrange(desc(variacao))

# erro do TSE - PORTO VERA CRUZ / RS
df_eleitorado_teste <- df_eleitorado_2018 %>%
  left_join(df_eleitorado_2020, by = "cd_municipio") %>%
  filter(is.na(`total.y`))

# eleitorado UF
eleitorado_UF <- eleitorado_2020 %>%
  group_by(SG_UF) %>%
  summarise(total = sum(QT_ELEITORES_PERFIL)) %>%
  janitor::clean_names()

# dados de 2018 e 2020 + eleitorado / UF
df_eleitorado_UF <- df_eleitorado_MUN %>%
  select(-c(total, variacao, n_2020_perc)) %>%
  group_by(sg_uf) %>%
  summarize(n2020 = sum(n2020),
            n2018 = sum(n2018),
            diferenca = sum(diferenca)) %>%
  left_join(eleitorado_UF, by =  "sg_uf") %>%
  mutate(variacao = ((n2020 - n2018) / n2018) * 100,
         n2020_perc = (n2020  / total) * 100) %>%
  arrange(desc(n2020_perc))
