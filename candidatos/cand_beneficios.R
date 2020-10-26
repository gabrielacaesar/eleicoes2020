# leitura de pacotes
library(tidyverse)
library(data.table)

## patrimônio candidatos / 2020
bens_cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_26out2020/bem_candidato_2020/bem_candidato_2020_BRASIL.csv", 
                        encoding = "Latin-1",
                        colClasses = c(SQ_CANDIDATO = "character"))

bens_cand_2020_n <- bens_cand_2020 %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

## recursos próprios candidatos / 2020
receitas_cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_26out2020/prestacao_de_contas_eleitorais_candidatos_2020/receitas_candidatos_2020_BRASIL.csv",
                            encoding = "Latin-1",
                            colClasses = c(SQ_CANDIDATO = "character"))

receitas_cand_2020_n <- receitas_cand_2020 %>%
  filter(DS_ORIGEM_RECEITA == "Recursos próprios") %>%
  mutate(VR_RECEITA = str_replace_all(VR_RECEITA, "\\,", "."),
         VR_RECEITA = as.double(VR_RECEITA)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(recursos_proprios = sum(VR_RECEITA))

## candidatos / 2020
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_26out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO"))

cand_2020_n <- cand_2020 %>%
  mutate(cpf_novo = str_sub(NR_CPF_CANDIDATO, 4)) %>%
  mutate(cpf_novo = str_sub(cpf_novo, end = -3)) %>%
  left_join(bens_cand_2020_n, by = "SQ_CANDIDATO") %>%
  left_join(receitas_cand_2020_n, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0)

write_rds(cand_2020_n, "cand_2020_n.rds")

## auxilio emergencial / agosto de 2020
aux <- fread("C:/Users/acaesar/Downloads/beneficios_cidadao/auxilio_emergencial/202008_AuxilioEmergencial/202008_AuxilioEmergencial.csv",
             encoding = "Latin-1",
             select = c("UF", "NOME MUNICÍPIO", "NIS BENEFICIÁRIO", "CPF BENEFICIÁRIO", "NOME BENEFICIÁRIO", "PARCELA", "VALOR BENEFÍCIO"))

write_rds(aux, "aux.rds")

aux_n <- aux %>%
  janitor::clean_names() %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\*")) %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\.")) %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\-")) %>%
  mutate(valor_beneficio = str_replace_all(valor_beneficio, "\\,", ".")) %>%
  distinct(cpf_beneficiario, nome_beneficiario, .keep_all = TRUE)

## bolsa família / setembro de 2020
bfamilia <- fread("C:/Users/acaesar/Downloads/beneficios_cidadao/bolsa_familia/202009_BolsaFamilia_Pagamentos/202009_BolsaFamilia_Pagamentos.csv",
                  encoding = "Latin-1")

write_rds(bfamilia, "bfamilia.rds")

bfamilia_n <- bfamilia %>%
  janitor::clean_names() %>%
  select(-c(mes_referencia, mes_competencia, codigo_municipio_siafi)) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\*")) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\.")) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\-")) %>%
  mutate(valor_parcela = str_replace_all(valor_parcela, "\\,", "."))

## seguro defeso / agosto de 2020
seg_defeso <- fread("C:/Users/acaesar/Downloads/beneficios_cidadao/seguro_defeso/202008_SeguroDefeso/202008_SeguroDefeso.csv",
                    encoding = "Latin-1")

seg_defeso_n <- seg_defeso %>%
  janitor::clean_names() %>%
  select(-c(mes_referencia, codigo_municipio_siafi, rgp_favorecido)) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\*")) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\.")) %>%
  mutate(cpf_favorecido = str_remove_all(cpf_favorecido, "\\-")) %>%
  mutate(valor_parcela = str_replace_all(valor_parcela, "\\,", "."))

# BPC / julho de 2020 /// NA no CPF (?)
bpc <- fread("C:/Users/acaesar/Downloads/beneficios_cidadao/bpc/202007_BPC/202007_BPC.csv",
             encoding = "Latin-1")

bpc_n <- bpc %>%
  janitor::clean_names() %>%
  filter(cpf_beneficiario != "***Titular menor de 16 anos***") %>%
  select(-c(mes_competencia, mes_referencia, codigo_municipio_siafi, nis_representante_legal, 
            cpf_representante_legal, nome_representante_legal, numero_beneficio, 
            beneficio_concedido_judicialmente)) %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\*")) %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\.")) %>%
  mutate(cpf_beneficiario = str_remove_all(cpf_beneficiario, "\\-")) %>%
  mutate(valor_parcela = str_replace_all(valor_parcela, "\\,", "."))

#####
## ANÁLISE

# e casos do PR? outra metodologia?


t <- cand_2020_n %>%
  left_join(aux_n, by = c("NM_CANDIDATO" = "nome_beneficiario",
                          "cpf_novo" = "cpf_beneficiario")) %>%
  mutate(check_uf = SG_UF == uf,
         check_cidade = NM_UE == nome_municipio) %>%
  filter(!is.na(valor_beneficio)) %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE)
