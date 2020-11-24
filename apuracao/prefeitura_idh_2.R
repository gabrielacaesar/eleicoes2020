library(data.table)
library(tidyverse)

# download de dados 
# Atlas do Desenvolvimento Humano no Brasil. Pnud Brasil, Ipea e FJP, 2020.
# http://www.atlasbrasil.org.br/ranking

resultado_2020 <- fread("C:/Users/acaesar/Downloads/dados_23nov2020/resultado_candidatos-COE-23nov2020.csv",
                        encoding = "UTF-8",
                        colClasses = c(codigo_municipio_ibge = "character"))

idh <- fread("C:/Users/acaesar/Downloads/idh/idh-m_2010.csv",
             encoding = "UTF-8",
             colClasses = c(IDHM = "character", Codmun6 = "character", Codmun7 = "character"))

cod_uf <- fread("https://raw.githubusercontent.com/leogermani/estados-e-municipios-ibge/master/estados.csv",
                encoding = "UTF-8")

faltantes <- data.frame(codigo_municipio_ibge = c("3147600", "2807006"),
                        nome_municipio = c("Passa Quatro", "São Miguel do Aleixo"),
                        uf = c("MG", "SE"),
                        cargo = c("Prefeito", "Prefeito"),
                        nome_candidato = c("HENRIQUE NOGUEIRA", "GILTON MENESES"),
                        sigla_partido = c("MDB", "PSD"),
                        sigla = c("MG", "SE"),
                        municipio = c("Passa Quatro", "São Miguel do Aleixo"),
                        idhm = c("0.715", "0.567"),
                        faixa = c("Alto", "Baixo"))
  
# análise geral

idh_tidy <- idh %>%
  left_join(cod_uf, by = c("UF" = "COD")) %>%
  janitor::clean_names() %>%
  mutate(idhm = str_replace_all(idhm, "\\,", ".")) %>%
  mutate(faixa = case_when(idhm >= 0.8 & idhm <= 1.000 ~ "Muito alto",
                           idhm >= 0.700  & idhm <= 0.799 ~ "Alto",
                           idhm >= 0.600 & idhm <= 0.699 ~ "Médio",
                           idhm >= 0.500 & idhm <= 0.599 ~ "Baixo",
                           idhm >= 0 & idhm <= 0.499 ~ "Muito baixo")) %>%
  filter(municipio != "BRASÍLIA" &
         municipio != "FERNANDO DE NORONHA" &
         municipio != "MACAPÁ") %>%
  select(-c(ano, uf, nome, codmun6)) %>%
  rename(codigo_municipio_ibge = codmun7) %>%
  relocate(sigla, before = municipio)

resultado_2020_n <- resultado_2020 %>%
  filter(eleito == "TRUE") %>%
  distinct(codigo_municipio_ibge, .keep_all = TRUE) %>%
  select(codigo_municipio_ibge, nome_municipio, uf, cargo, nome_candidato, sigla_partido) %>%
  left_join(idh_tidy, by = "codigo_municipio_ibge") %>%
  replace(is.na(.), "Não existia em 2010") %>%
  rbind(faltantes)
  
grouped_resultado_2020 <- resultado_2020_n %>%
  group_by(sigla_partido, faixa) %>%
  summarise(int = n()) %>%
  pivot_wider(names_from = faixa, values_from = int) %>%
  replace(is.na(.), 0) %>%
  mutate(total = sum(c_across(where(is.numeric)), na.rm = T)) %>%
  mutate(Alto_perc = round((Alto / total), 3) * 100,
         Baixo_perc = round((Baixo / total), 3) * 100,
         Médio_perc = round((Médio / total), 3) * 100,
         Muito_alto_perc = round((`Muito alto` / total), 3) * 100,
         Muito_baixo_perc = round((`Muito baixo` / total), 3) * 100) %>%
  arrange(desc(total)) %>%
  mutate(sigla_partido = str_replace_all(sigla_partido, "AVANTE", "Avante"),
         sigla_partido = str_replace_all(sigla_partido, "CIDADANIA", "Cidadania"),
         sigla_partido = str_replace_all(sigla_partido, "NOVO", "Novo"),
         sigla_partido = str_replace_all(sigla_partido, "PATRIOTA", "Patriota"),
         sigla_partido = str_replace_all(sigla_partido, "PC do B", "PCdoB"),
         sigla_partido = str_replace_all(sigla_partido, "REDE", "Rede"),
         sigla_partido = str_replace_all(sigla_partido, "REPUBLICANOS", "Republicanos"),
         sigla_partido = str_replace_all(sigla_partido, "SOLIDARIEDADE", "SD"))

#### faixa de desenvolvimento

long_MUITO_ALTO_2020 <- grouped_resultado_2020 %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(str_detect(idh, "Muito") &
         str_detect(idh, "alto") |
         str_detect(idh, "total")) %>%
  pivot_wider(names_from = idh, values_from = n) %>%
  `colnames<-`(c("Partido", "Nº de prefeituras com IDH muito alto", "Total de prefeituras conquistadas", "Percentual")) %>%
  arrange(desc(Percentual)) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ","))

write.csv(long_MUITO_ALTO_2020, "long_MUITO_ALTO_2020.csv")

long_ALTO_2020 <- grouped_resultado_2020 %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(str_detect(idh, "Alto") |
         str_detect(idh, "total")) %>%
  pivot_wider(names_from = idh, values_from = n) %>%
  `colnames<-`(c("Partido", "Nº de prefeituras com IDH alto", "Total de prefeituras conquistadas", "Percentual")) %>%
  arrange(desc(Percentual)) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ","))
  
write.csv(long_ALTO_2020, "long_ALTO_2020.csv")

long_MÉDIO_2020 <- grouped_resultado_2020 %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(str_detect(idh, "Médio") |
         str_detect(idh, "total")) %>%
  pivot_wider(names_from = idh, values_from = n) %>%
  `colnames<-`(c("Partido", "Nº de prefeituras com IDH médio", "Total de prefeituras conquistadas", "Percentual")) %>%
  arrange(desc(Percentual)) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ","))

write.csv(long_MÉDIO_2020, "long_MÉDIO_2020.csv")

long_BAIXO_2020 <- grouped_resultado_2020 %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(str_detect(idh, "Baixo") |
         str_detect(idh, "total")) %>%
  pivot_wider(names_from = idh, values_from = n) %>%
  `colnames<-`(c("Partido", "Nº de prefeituras com IDH baixo", "Total de prefeituras conquistadas", "Percentual")) %>%
  arrange(desc(Percentual)) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ","))

write.csv(long_BAIXO_2020, "long_BAIXO_2020.csv")

long_MUITO_BAIXO_2020 <- grouped_resultado_2020 %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(str_detect(idh, "Muito") &
         str_detect(idh, "baixo") |
         str_detect(idh, "total")) %>%
  pivot_wider(names_from = idh, values_from = n) %>%
  `colnames<-`(c("Partido", "Nº de prefeituras com IDH muito baixo", "Total de prefeituras conquistadas", "Percentual")) %>%
  arrange(desc(Percentual)) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ","))

write.csv(long_MUITO_BAIXO_2020, "long_MUITO_BAIXO_2020.csv")

#### partido - número absoluto e percentual

dir.create(paste0("arquivo_partidos_", Sys.Date()))
setwd(paste0("arquivo_partidos_", Sys.Date()))

get_partido_geral <- function(i){
  grouped_resultado_2020 %>%
  split(grouped_resultado_2020$sigla_partido) %>%
  .[[i]] %>%
  pivot_longer(cols = !sigla_partido, names_to = "idh", values_to = "n") %>%
  filter(idh == "Muito baixo" |
         idh == "Baixo" |
         idh == "Médio" |
         idh == "Alto" |
         idh == "Muito alto") %>%
  mutate(id_idh = case_when(idh == "Muito baixo" ~ 5,
                            idh == "Baixo" ~ 4,
                            idh == "Médio" ~ 3,
                            idh == "Alto" ~ 2,
                            idh == "Muito alto" ~ 1)) %>%
  mutate(Percentual = round((n / sum(n)), 3) * 100) %>%
  arrange(desc(id_idh)) %>%
  select(-c(id_idh)) %>%
  `colnames<-`(c("Partido", "IDH-M", "Nº de prefeituras conquistadas", "Percentual")) %>%
  mutate(Percentual = str_replace_all(Percentual, "\\.", ",")) %>%
  write.csv(., paste0("G1_", .$Partido[1], ".csv"))
}

map_df(1:28, get_partido_geral)


