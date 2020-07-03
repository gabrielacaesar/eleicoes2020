library(tidyverse)
library(data.table)

# consulta cand
consulta_cand_2016_BRASIL <- fread("~/Downloads/eleicao-2016/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                            encoding = "Latin-1", 
                            drop = c("DT_GERACAO", "HH_GERACAO",
                                     "CD_TIPO_ELEICAO", "NM_TIPO_ELEICAO",
                                     "CD_ELEICAO", "TP_ABRANGENCIA", "SG_UE",
                                     "NM_PARTIDO",
                                     "DT_ELEICAO", "DS_ELEICAO",
                                     "NM_EMAIL", "NM_SOCIAL_CANDIDATO"))

# consulta vagas
consulta_vagas_2016_BRASIL <- fread("~/Downloads/eleicao-2016/consulta_vagas_2016/consulta_vagas_2016_BRASIL.csv",
                                encoding = "Latin-1",
                                drop = c("DT_GERACAO", "HH_GERACAO",
                                         "CD_TIPO_ELEICAO", "NM_TIPO_ELEICAO",
                                         "CD_ELEICAO", "SG_UE",
                                         "DT_ELEICAO", "DS_ELEICAO"))

# bem candidato
bem_candidato_2016_BRASIL <- fread("~/Downloads/eleicao-2016/bem_candidato_2016/bem_candidato_2016_BRASIL.csv",
                               encoding = "Latin-1",
                               drop = c("DT_GERACAO", "HH_GERACAO",
                                        "CD_TIPO_ELEICAO", "NM_TIPO_ELEICAO",
                                        "CD_ELEICAO", "SG_UE",
                                        "DT_ELEICAO", "DS_ELEICAO",
                                        "DT_ULTIMA_ATUALIZACAO", "HH_ULTIMA_ATUALIZACAO"))

# left join bem candidato + consulta cand
consulta_cand_2016_BRASIL_small <- consulta_cand_2016_BRASIL %>%
  select("SG_UF", "NM_UE", "DS_CARGO", "SQ_CANDIDATO", "NM_CANDIDATO", "NM_URNA_CANDIDATO", "SG_PARTIDO")

teste <- bem_candidato_2016_BRASIL %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  left_join(consulta_cand_2016_BRASIL_small, by = "SQ_CANDIDATO") %>%
  group_by(NM_CANDIDATO, SG_PARTIDO, DS_CARGO) %>%
  summarize(SUM_BEM_CANDIDATO = sum(VR_BEM_CANDIDATO))

