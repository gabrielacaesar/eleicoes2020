# reading libraries
library(tidyverse)
library(data.table)

# creating file and setting as working directory
dir.create(paste0("~/Downloads/dados_TSE_", Sys.Date()))
setwd(paste0("~/Downloads/dados_TSE_", Sys.Date()))

# importing CSV; saving and reading as RDS
## cand
cand_2016_BR <- fread("~/Downloads/consulta_cand_2016/consulta_cand_2016_BRASIL.csv", encoding = "Latin-1")
saveRDS(cand_2016_BR, file = "cand_2016_BR.rds")
cand_2016_BR_rds <- readRDS("cand_2016_BR.rds")
remove(cand_2016_BR)

## bens
bens_cand_2016_BR <- fread("~/Downloads/bem_candidato_2016/bem_candidato_2016_BRASIL.csv", encoding = "Latin-1")
saveRDS(bens_cand_2016_BR, file = "bens_cand_2016_BR.rds")
bens_cand_2016_BR_rds <- readRDS("bens_cand_2016_BR.rds")
remove(bens_cand_2016_BR)

# > options(scipen = 999) https://intellipaat.com/community/7352/force-r-not-to-use-exponential-notation-e-g-e-10
# grouping properties and calculating total
grouped_bens <- bens_cand_2016_BR_rds %>%
  mutate(VR_BEM_CANDIDATO = str_replace_all(VR_BEM_CANDIDATO, "\\,", "."),
         VR_BEM_CANDIDATO = as.double(VR_BEM_CANDIDATO)) %>%
  group_by(SQ_CANDIDATO) %>%
  summarise(total_patrimonio = sum(VR_BEM_CANDIDATO))
  
# joining dataframes and replacing NA for 0
cand_patrimonio <- cand_2016_BR_rds %>%
  select(SQ_CANDIDATO, NM_CANDIDATO, NM_URNA_CANDIDATO, SG_PARTIDO, NM_UE, SG_UF) %>%
  left_join(grouped_bens, by = "SQ_CANDIDATO") %>%
  replace(is.na(.), 0)



  
  
  
  
  