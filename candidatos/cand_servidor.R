# leitura de pacotes
library(data.table)
library(tidyverse)

# leitura de bases / SERVIDORES
servidor_militar <- fread("C:/Users/acaesar/Downloads/servidores/202007_Militares/202007_CadastroMilitares.csv")
remuneracao_militar <- fread("C:/Users/acaesar/Downloads/servidores/202007_Militares/202007_RemuneracaoMilitares.csv")

servidor_civil <- fread("C:/Users/acaesar/Downloads/servidores/202007_Servidores/202007_Cadastro.csv")
remuneracao_civil <- fread("C:/Users/acaesar/Downloads/servidores/202007_Servidores/202007_Remuneracao.csv")

# ajuste em bases
servidor_militar_n <- remuneracao_militar %>%
  janitor::clean_names() %>%
  select(id_servidor_portal, remuneracao_basica_bruta_r) %>%
  left_join(servidor_militar, by = c("id_servidor_portal" = "Id_SERVIDOR_PORTAL")) %>%
  mutate(arquivo = "servidor_militar")
  
servidor_civil_n <- remuneracao_civil %>%
  janitor::clean_names() %>%
  select(id_servidor_portal, remuneracao_basica_bruta_r) %>%
  left_join(servidor_civil, by = c("id_servidor_portal" = "Id_SERVIDOR_PORTAL")) %>%
  mutate(arquivo = "servidor_civil")

servidores <- servidor_militar_n %>%
  rbind(servidor_civil_n) %>%
  janitor::clean_names() %>%
  mutate(cpf = str_remove_all(cpf, "\\*"),
         cpf = str_remove_all(cpf, "\\."),
         cpf = str_remove_all(cpf, "\\-")) %>%
  mutate(remuneracao_basica_bruta_r = str_replace_all(remuneracao_basica_bruta_r, "\\,", "."),
         remuneracao_basica_bruta_r = as.double(remuneracao_basica_bruta_r))

# leitura de base / CAND
cand_2020 <- fread("C:/Users/acaesar/Downloads/dados_26out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                   encoding = "Latin-1",
                   colClasses = c(NR_CPF_CANDIDATO = "character", SQ_CANDIDATO = "character"),
                   select = c("SG_UF", "SG_UE", "NM_UE", "DS_CARGO", "SQ_CANDIDATO",
                              "NM_CANDIDATO", "NR_CPF_CANDIDATO", "SG_PARTIDO", "DS_OCUPACAO"))
# ajuste em bases
cand_2020_n <- cand_2020 %>%
  mutate(cpf_novo = str_sub(NR_CPF_CANDIDATO, 4)) %>%
  mutate(cpf_novo = str_sub(cpf_novo, end = -3))

# ANÁLISE
cand_servidores <- cand_2020_n %>%
  left_join(servidores, by = c("NM_CANDIDATO" = "nome",
                          "cpf_novo" = "cpf")) %>%
  filter(!is.na(org_lotacao)) %>%
  distinct(NM_CANDIDATO, NR_CPF_CANDIDATO, .keep_all = TRUE)

