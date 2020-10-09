# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")


# importing CSV - candidatos
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_8out2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1", colClasses = class_columns,
                      select = c("NR_CPF_CANDIDATO", "NM_CANDIDATO"))

# empresas
CEIS <- fread("C:/Users/acaesar/Downloads/Empresas_Inidoneas_e_Suspensas/20200929_CEIS.csv")

cand_2020_BR_n <- cand_2020_BR %>%
  separate_rows(NR_CPF_CANDIDATO)


CEIS_n <- CEIS %>%
  janitor::clean_names() %>%
  filter(str_detect(cpf_ou_cnpj_do_sancionado, "\\*"))
