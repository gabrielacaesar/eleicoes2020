library(tidyverse)
library(data.table)

cand_2004 <- fread("~/Downloads/wetransfer-589937/consolidado2004.csv", encoding = "Latin-1")

lista_capitais <- c("RIO BRANCO", 
                    "MACEIO", 
                    "MACAPA", 
                    "MANAUS", 
                    "SALVADOR", 
                    "FORTALEZA", 
                    "VITORIA", 
                    "GOIANIA", 
                    "SAO LUIS", 
                    "CUIABA", 
                    "CAMPO GRANDE", 
                    "BELO HORIZONTE", 
                    "BELEM", 
                    "JOAO PESSOA", 
                    "CURITIBA", 
                    "RECIFE", 
                    "TERESINA", 
                    "RIO DE JANEIRO", 
                    "NATAL", 
                    "PORTO ALEGRE", 
                    "PORTO VELHO", 
                    "BOA VISTA", 
                    "FLORIANOPOLIS", 
                    "SAO PAULO", 
                    "ARACAJU", 
                    "PALMAS")

tidy_cand_2004 <- cand_2004 %>%
  select(V6, V8, V10, V11, V15, V19) %>%
  filter(V10 == "PREFEITO") 
  
v_2004_bruto <- v_2004 %>%
  group_by(V6, V8) %>%
  summarise(qt_cand = n()) 
  #mutate(capital = ifelse(str_detect(V8, 
  #                                   paste(lista_capitais, collapse = "|")), V8, NA)) %>%
  #filter(!is.na(capital))

v_2004 <- tidy_cand_2004 %>%
  filter(V8 == "RIO BRANCO"  |
           V8 == "MACEIO"  |
           V8 == "MACAPA"  |
           V8 == "MANAUS"  |
           V8 == "SALVADOR"  |
           V8 == "FORTALEZA" |
           V8 == "VITORIA"   |
           V8 == "GOIANIA"  |
           V8 == "SAO LUIS" |
           V8 == "CUIABA"  |
           V8 == "CAMPO GRANDE"  |
           V8 == "BELO HORIZONTE" |
           V8 == "BELEM"  |
           V8 == "JOAO PESSOA"  |
           V8 == "CURITIBA" |
           V8 == "RECIFE" |
           V8 == "TERESINA"  |
           V8 == "RIO DE JANEIRO" |
           V8 == "NATAL"  |
           V8 == "PORTO ALEGRE"  |
           V8 == "PORTO VELHO" |
           V8 == "BOA VISTA" |
           V8 == "FLORIANOPOLIS"  |
           V8 == "SAO PAULO" |
           V8 == "ARACAJU"  |
           V8 == "PALMAS" ) %>%
  distinct(V11, .keep_all = TRUE)


write.csv(v_2004, "v_2004.csv")