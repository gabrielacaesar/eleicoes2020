library(tidyverse)
library(data.table)

vagas_2016 <- fread("C:/Users/acaesar/Downloads/consulta_vagas_2016/consulta_vagas_2016_BRASIL.csv",
                    encoding = "Latin-1")

vagas_2016_n <- vagas_2016 %>%
  filter(CD_CARGO == "13") %>%
  filter(SG_UE == "1392" | # Rio Branco - AC
           SG_UE == "27855" | # Maceió - AL
           SG_UE == "6050" | # Macapá - AP
           SG_UE == "2550" | # Manaus - AM
           SG_UE == "38490" | # Salvador - BA
           SG_UE == "13897" | # Fortaleza - CE
           SG_UE == "57053" | # Vitória - ES
           SG_UE == "93734" | # Goiânia - GO
           SG_UE == "9210" | # São Luís - MA
           SG_UE == "90670" | # Cuiabá - MT
           SG_UE == "90514" | # Campo Grande - MS
           SG_UE == "41238" | # Belo Horizonte - MG 
           SG_UE == "4278" | # Belém - PA
           SG_UE == "20516" | # João Pessoa - PB
           SG_UE == "75353" | # Curitiba - PR
           SG_UE == "25313" | # Recife - PE
           SG_UE == "12190" | # Teresina - PI
           SG_UE == "60011" | # Rio de Janeiro - RJ
           SG_UE == "17612" | # Natal - RN
           SG_UE == "88013" | # Porto Alegre - RS
           SG_UE == "35" | # Porto Velho - RO
           SG_UE == "3018" | # Boa Vista - RR
           SG_UE == "81051" | # Florianópolis - SC
           SG_UE == "71072" | # São Paulo - SP
           SG_UE == "31054" | # Aracaju - SE
           SG_UE == "73440") %>%
  select(SG_UF, NM_UE, QT_VAGAS) %>%
  arrange(SG_UF)

write.csv(vagas_2016_n, "vagas_2016_n.csv")
