# reading libraries
library(tidyverse)
library(data.table)

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# importing CSV
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_30set2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", 
                      encoding = "Latin-1",
                      colClasses = class_columns)

bens_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_29set2020/bem_candidato_2020/bem_candidato_2020_BRASIL.csv",
                      encoding = "Latin-1",
                      colClasses = c(SQ_CANDIDATO = "character"))

# grouping properties and calculating total
grouped_bens <- bens_2020_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))
  
# joining dataframes and replacing NA for 0
cand_patrimonio <- cand_2020_BR %>%
  left_join(grouped_bens, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) 

# RIO BRANCO + select
quem_escolho_RIO_BRANCO <- cand_patrimonio %>%
  select(SQ_CANDIDATO, SG_UE, SG_UF, NM_UE, NM_CANDIDATO, NM_URNA_CANDIDATO, NR_CPF_CANDIDATO, SG_PARTIDO, DS_OCUPACAO, 
         NR_IDADE_DATA_POSSE, DS_GENERO, DS_COR_RACA, DS_GRAU_INSTRUCAO, total_patrimonio, NR_CANDIDATO, NM_EMAIL) %>%
  filter(SG_UE == "1392")

write.csv(quem_escolho_RIO_BRANCO, "quem_escolho_RIO_BRANCO.csv")
write.csv(cand_patrimonio, "cand_patrimonio_30set2020.csv")
         