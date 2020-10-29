# leitura de pacotes
library(data.table)
library(tidyverse)

##### leitura de bases / SERVIDORES
# link: http://www.portaldatransparencia.gov.br/download-de-dados/servidores
servidor_militar <- fread("C:/Users/acaesar/Downloads/servidores/202007_Militares/202007_CadastroMilitares.csv")
remuneracao_militar <- fread("C:/Users/acaesar/Downloads/servidores/202007_Militares/202007_RemuneracaoMilitares.csv")

servidor_civil <- fread("C:/Users/acaesar/Downloads/servidores/202009_Servidores/202009_Cadastro.csv")
remuneracao_civil <- fread("C:/Users/acaesar/Downloads/servidores/202009_Servidores/202009_Remuneracao.csv")

# ajuste em bases
servidor_militar_n <- remuneracao_militar %>%
  janitor::clean_names() %>%
  select(id_servidor_portal, remuneracao_basica_bruta_r, outras_remuneracoes_eventuais_r) %>%
  left_join(servidor_militar, by = c("id_servidor_portal" = "Id_SERVIDOR_PORTAL")) %>%
  mutate(arquivo = "servidor_militar")

servidor_civil_n <- remuneracao_civil %>%
  janitor::clean_names() %>%
  select(id_servidor_portal, remuneracao_basica_bruta_r, outras_remuneracoes_eventuais_r) %>%
  left_join(servidor_civil, by = c("id_servidor_portal" = "Id_SERVIDOR_PORTAL")) %>%
  mutate(arquivo = "servidor_civil")

servidores <- servidor_militar_n %>%
  rbind(servidor_civil_n) %>%
  janitor::clean_names() %>%
  mutate(cpf = str_remove_all(cpf, "\\*"),
         cpf = str_remove_all(cpf, "\\."),
         cpf = str_remove_all(cpf, "\\-")) %>%
  mutate(remuneracao_basica_bruta_r = str_replace_all(remuneracao_basica_bruta_r, "\\,", "."),
         remuneracao_basica_bruta_r = as.double(remuneracao_basica_bruta_r),
         outras_remuneracoes_eventuais_r = str_replace_all(outras_remuneracoes_eventuais_r, "\\,", "."),
         outras_remuneracoes_eventuais_r = as.double(outras_remuneracoes_eventuais_r),
         rem_novo = remuneracao_basica_bruta_r + outras_remuneracoes_eventuais_r) 
  
  
##### leitura de bases / AFAST - SERVIDORES
# link: https://dados.gov.br/dataset/afastamento-remunerado
# afast_servidor <- fread("C:/Users/acaesar/Downloads/servidores/Gestao-de-Pessoas-Executivo-Federal-Afastamentos-e-Licencas/AFASTREM_092020.csv")

# afast_servidor_n <- afast_servidor %>%
#  janitor::clean_names() %>%
#  filter(str_detect(descricao_do_afastamento, "Política")) %>%
#  mutate(valor_rendimento_liquido = str_replace_all(valor_rendimento_liquido, "\\,", "."),
#         valor_rendimento_liquido = as.double(valor_rendimento_liquido))

##### leitura de base / CAND
# link: https://www.tse.jus.br/eleicoes/estatisticas/repositorio-de-dados-eleitorais-1/repositorio-de-dados-eleitorais
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_28out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_OCUPACAO",
                              "NM_EMAIL"))
# ajuste em bases
cand_2020_n <- cand_2020 %>%
  mutate(cpf_novo = str_sub(NR_CPF_CANDIDATO, 4)) %>%
  mutate(cpf_novo = str_sub(cpf_novo, end = -3))

# ANÁLISE
cand_servidores <- cand_2020_n %>%
  left_join(servidores, by = c("NM_CANDIDATO" = "nome",
                               "cpf_novo" = "cpf")) %>%
  filter(!is.na(org_lotacao)) %>%
  select(-c(tipo_vinculo, classe_cargo, referencia_cargo, padrao_cargo, nivel_cargo,
            sigla_funcao, nivel_funcao, codigo_atividade, opcao_parcial, cod_uorg_lotacao, 
            cod_org_lotacao, cod_orgsup_lotacao, cod_uorg_exercicio, cod_org_exercicio, 
            cod_orgsup_exercicio, diploma_ingresso_servicopublico, diploma_ingresso_orgao,
            diploma_ingresso_cargofuncao, data_diploma_ingresso_servicopublico,
            data_nomeacao_cargofuncao, data_ingresso_cargofuncao)) %>%
  arrange(desc(rem_novo)) %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE) 

write.csv(cand_servidores, "cand_servidores.csv")
