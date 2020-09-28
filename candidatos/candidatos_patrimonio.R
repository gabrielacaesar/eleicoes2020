# reading libraries
library(tidyverse)
library(data.table)

# importing CSV; saving and reading as RDS
## cand
cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_28set2020/consulta_cand_2020/consulta_cand_2020_BRASIL.csv", encoding = "Latin-1")

## bens
bens_cand_2020_BR <- fread("C:/Users/acaesar/Downloads/dados_28set2020/bem_candidato_2020/bem_candidato_2020_BRASIL.csv", encoding = "Latin-1")

# > options(scipen = 999) https://intellipaat.com/community/7352/force-r-not-to-use-exponential-notation-e-g-e-10
# grouping properties and calculating total
grouped_bens <- bens_cand_2020_BR %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))

# joining dataframes and replacing NA for 0
cand_patrimonio <- cand_2020_BR %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF) %>%
  left_join(grouped_bens, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0) 

write.csv(cand_patrimonio, "cand_patrimonio.csv")
