# lendo os pacotes
library(tidyverse)
library(data.table)

###### distinct(V11, .keep_all = TRUE) ????

## 2000
path_2000 <-  "C:/Users/acaesar/Downloads/candidatos/2000/"
cand_2000 <- list.files(path_2000, pattern = "*csv")

column_selected <- c("V3", "V6", "V7", "V8", "V10", "V11", "V15", "V19", "V43")

candidatos_2000 <- map_df(paste0(path_2000, cand_2000), fread, 
                          sep = ";",
                          encoding = "Latin-1", 
                          select = column_selected)

candidatos_2000_n <- candidatos_2000 %>%
  #rename("ano" = V3, "uf" = V6, "cod_num" = V7, "municipio" = V8, "cargo" = V10, "nome" = V11,
  #       "nome_urna" = V15, "partido" = V19, "status" = V43) %>%
  filter(cargo == "VEREADOR") %>%
  group_by(uf, municipio, cod_num) %>%
  summarise(qt_cand = n()) 

candidatos_2000_n2 <- candidatos_2000_n %>%
  filter(cod_num == "1392" | # Rio Branco - AC
           cod_num == "27855" | # Maceió - AL
           cod_num == "6050" | # Macapá - AP
           cod_num == "2550" | # Manaus - AM
           cod_num == "38490" | # Salvador - BA
           cod_num == "13897" | # Fortaleza - CE
           cod_num == "57053" | # Vitória - ES
           cod_num == "93734" | # Goiânia - GO
           cod_num == "9210" | # São Luís - MA
           cod_num == "90670" | # Cuiabá - MT
           cod_num == "90514" | # Campo Grande - MS
           cod_num == "41238" | # Belo Horizonte - MG 
           cod_num == "4278" | # Belém - PA
           cod_num == "20516" | # João Pessoa - PB
           cod_num == "75353" | # Curitiba - PR
           cod_num == "25313" | # Recife - PE
           cod_num == "12190" | # Teresina - PI
           cod_num == "60011" | # Rio de Janeiro - RJ
           cod_num == "17612" | # Natal - RN
           cod_num == "88013" | # Porto Alegre - RS
           cod_num == "35" | # Porto Velho - RO
           cod_num == "3018" | # Boa Vista - RR
           cod_num == "81051" | # Florianópolis - SC
           cod_num == "71072" | # São Paulo - SP
           cod_num == "31054" | # Aracaju - SE
           cod_num == "73440") # Palmas - TO



### 2004
cand_2004_AC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_AC.txt", 
                      select = column_selected)
cand_2004_AL <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_AL.txt",
                      select = column_selected)
cand_2004_AM <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_AM.txt",
                      select = column_selected)
cand_2004_AP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_AP.txt",
                      select = column_selected)
cand_2004_BA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_BA.txt",
                      select = column_selected)
cand_2004_CE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_CE.txt",
                      select = column_selected)
cand_2004_ES <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_ES.txt",
                      select = column_selected)
cand_2004_GO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_GO.txt",
                      select = column_selected)
cand_2004_MA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_MA.txt",
                      select = column_selected)
cand_2004_MG <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_MG.txt",
                      select = column_selected)
cand_2004_MS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_MS.txt",
                      select = column_selected)
cand_2004_MT <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_MT.txt",
                      select = column_selected)
cand_2004_PA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_PA.txt",
                      select = column_selected)
cand_2004_PB <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_PB.txt",
                      select = column_selected)
cand_2004_PE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_PE.txt",
                      select = column_selected)
cand_2004_PI <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_PI.txt",
                      select = column_selected)
cand_2004_PR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_PR.txt",
                      select = column_selected)
cand_2004_RJ <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_RJ.txt",
                      select = column_selected)
cand_2004_RN <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_RN.txt",
                      select = column_selected)
cand_2004_RO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_RO.txt",
                      select = column_selected)
cand_2004_RR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_RR.txt",
                      select = column_selected)
cand_2004_RS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_RS.txt",
                      select = column_selected)
cand_2004_SC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_SC.txt",
                      select = column_selected)
cand_2004_SE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_SE.txt",
                      select = column_selected)
cand_2004_SP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_SP.txt",
                      select = column_selected)
cand_2004_TO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2004/consulta_cand_2004_TO.txt",
                      select = column_selected)

cand_2004 <- rbind(cand_2004_AC, cand_2004_AL, cand_2004_AM, cand_2004_AP, 
                   cand_2004_BA, cand_2004_CE, cand_2004_ES, cand_2004_GO, 
                   cand_2004_MA, cand_2004_MG, cand_2004_MS, cand_2004_MT, 
                   cand_2004_PA, cand_2004_PB, cand_2004_PE, cand_2004_PI, 
                   cand_2004_PR, cand_2004_RJ, cand_2004_RN, cand_2004_RO, 
                   cand_2004_RR, cand_2004_RS, cand_2004_SC, cand_2004_SE,
                   cand_2004_SP, cand_2004_TO) %>%
  rename("ano" = V3, "uf" = V6, "cod_num" = V7, "municipio" = V8, "cargo" = V10, "nome" = V11,
         "nome_urna" = V15, "partido" = V19, "status" = V43) %>%
  group_by(uf, cod_num, municipio) %>%
  summarise(qt_cand = n())

cand_2004_n <- cand_2004 %>%
  filter(cod_num == "1392" |
           cod_num == "27855" |
           cod_num == "6050" |
           cod_num == "2550" |
           cod_num == "38490" |
           cod_num == "13897" |
           cod_num == "57053" |
           cod_num == "93734" |
           cod_num == "9210" |
           cod_num == "90670" |
           cod_num == "90514" |
           cod_num == "41238" |
           cod_num == "4278" |
           cod_num == "20516" |
           cod_num == "775353" |
           cod_num == "25313" |
           cod_num == "12190" |
           cod_num == "60011" |
           cod_num == "17612" |
           cod_num == "88013" |
           cod_num == "35" |
           cod_num == "3018" |
           cod_num == "81051" |
           cod_num == "71072" |
           cod_num == "31054" |
           cod_num == "73440")


## 2008 
cand_2008_AC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_AC.txt", 
                      select = column_selected)
cand_2008_AL <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_AL.txt",
                      select = column_selected)
cand_2008_AM <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_AM.txt",
                      select = column_selected)
cand_2008_AP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_AP.txt",
                      select = column_selected)
cand_2008_BA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_BA.txt",
                      select = column_selected)
cand_2008_CE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_CE.txt",
                      select = column_selected)
cand_2008_ES <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_ES.txt",
                      select = column_selected)
cand_2008_GO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_GO.txt",
                      select = column_selected)
cand_2008_MA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_MA.txt",
                      select = column_selected)
cand_2008_MG <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_MG.txt",
                      select = column_selected)
cand_2008_MS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_MS.txt",
                      select = column_selected)
cand_2008_MT <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_MT.txt",
                      select = column_selected)
cand_2008_PA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_PA.txt",
                      select = column_selected)
cand_2008_PB <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_PB.txt",
                      select = column_selected)
cand_2008_PE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_PE.txt",
                      select = column_selected)
cand_2008_PI <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_PI.txt",
                      select = column_selected)
cand_2008_PR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_PR.txt",
                      select = column_selected)
cand_2008_RJ <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_RJ.txt",
                      select = column_selected)
cand_2008_RN <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_RN.txt",
                      select = column_selected)
cand_2008_RO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_RO.txt",
                      select = column_selected)
cand_2008_RR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_RR.txt",
                      select = column_selected)
cand_2008_RS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_RS.txt",
                      select = column_selected)
cand_2008_SC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_SC.txt",
                      select = column_selected)
cand_2008_SE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_SE.txt",
                      select = column_selected)
cand_2008_SP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_SP.txt",
                      select = column_selected)
cand_2008_TO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2008/consulta_cand_2008_TO.txt",
                      select = column_selected)

cand_2008 <- rbind(cand_2008_AC, cand_2008_AL, cand_2008_AM, cand_2008_AP, 
                   cand_2008_BA, cand_2008_CE, cand_2008_ES, cand_2008_GO, 
                   cand_2008_MA, cand_2008_MG, cand_2008_MS, cand_2008_MT, 
                   cand_2008_PA, cand_2008_PB, cand_2008_PE, cand_2008_PI, 
                   cand_2008_PR, cand_2008_RJ, cand_2008_RN, cand_2008_RO, 
                   cand_2008_RR, cand_2008_RS, cand_2008_SC, cand_2008_SE,
                   cand_2008_SP, cand_2008_TO) %>%
  rename("ano" = V3, "uf" = V6, "cod_num" = V7, "municipio" = V8, "cargo" = V10, "nome" = V11,
         "nome_urna" = V15, "partido" = V19, "status" = V43) %>%
  group_by(uf, cod_num, municipio) %>%
  summarise(qt_cand = n())

cand_2008_n <- cand_2008 %>%
  filter(cod_num == "1392" |
           cod_num == "27855" |
           cod_num == "6050" |
           cod_num == "2550" |
           cod_num == "38490" |
           cod_num == "13897" |
           cod_num == "57053" |
           cod_num == "93734" |
           cod_num == "9210" |
           cod_num == "90670" |
           cod_num == "90514" |
           cod_num == "41238" |
           cod_num == "4278" |
           cod_num == "20516" |
           cod_num == "775353" |
           cod_num == "25313" |
           cod_num == "12190" |
           cod_num == "60011" |
           cod_num == "17612" |
           cod_num == "88013" |
           cod_num == "35" |
           cod_num == "3018" |
           cod_num == "81051" |
           cod_num == "71072" |
           cod_num == "31054" |
           cod_num == "73440") 

## 2012 
cand_2012_AC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_AC.txt", 
                      select = column_selected)
cand_2012_AL <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_AL.txt",
                      select = column_selected)
cand_2012_AM <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_AM.txt",
                      select = column_selected)
cand_2012_AP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_AP.txt",
                      select = column_selected)
cand_2012_BA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_BA.txt",
                      select = column_selected)
cand_2012_CE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_CE.txt",
                      select = column_selected)
cand_2012_ES <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_ES.txt",
                      select = column_selected)
cand_2012_GO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_GO.txt",
                      select = column_selected)
cand_2012_MA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_MA.txt",
                      select = column_selected)
cand_2012_MG <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_MG.txt",
                      select = column_selected)
cand_2012_MS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_MS.txt",
                      select = column_selected)
cand_2012_MT <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_MT.txt",
                      select = column_selected)
cand_2012_PA <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_PA.txt",
                      select = column_selected)
cand_2012_PB <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_PB.txt",
                      select = column_selected)
cand_2012_PE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_PE.txt",
                      select = column_selected)
cand_2012_PI <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_PI.txt",
                      select = column_selected)
cand_2012_PR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_PR.txt",
                      select = column_selected)
cand_2012_RJ <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_RJ.txt",
                      select = column_selected)
cand_2012_RN <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_RN.txt",
                      select = column_selected)
cand_2012_RO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_RO.txt",
                      select = column_selected)
cand_2012_RR <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_RR.txt",
                      select = column_selected)
cand_2012_RS <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_RS.txt",
                      select = column_selected)
cand_2012_SC <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_SC.txt",
                      select = column_selected)
cand_2012_SE <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_SE.txt",
                      select = column_selected)
cand_2012_SP <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_SP.txt",
                      select = column_selected)
cand_2012_TO <- fread("C:/Users/acaesar/Downloads/consulta_cand_2012/consulta_cand_2012_TO.txt",
                      select = column_selected)

cand_2012 <- rbind(cand_2012_AC, cand_2012_AL, cand_2012_AM, cand_2012_AP, 
                   cand_2012_BA, cand_2012_CE, cand_2012_ES, cand_2012_GO, 
                   cand_2012_MA, cand_2012_MG, cand_2012_MS, cand_2012_MT, 
                   cand_2012_PA, cand_2012_PB, cand_2012_PE, cand_2012_PI, 
                   cand_2012_PR, cand_2012_RJ, cand_2012_RN, cand_2012_RO, 
                   cand_2012_RR, cand_2012_RS, cand_2012_SC, cand_2012_SE,
                   cand_2012_SP, cand_2012_TO) %>%
  rename("ano" = V3, "uf" = V6, "cod_num" = V7, "municipio" = V8, "cargo" = V10, "nome" = V11,
         "nome_urna" = V15, "partido" = V19, "status" = V43)
