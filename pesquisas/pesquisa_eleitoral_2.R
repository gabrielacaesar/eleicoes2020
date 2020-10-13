# instalar pacote (se necessário)
# install.packages("tidyverse")

# ler pacote
library(tidyverse)

# definir pasta
# ATENÇÃO 'path' e 'data_para_ordem' PRECISAM SER ALTERADOS
path <- "C:/Users/acaesar/Downloads/pesquisa_13out2020/"
data_para_ordem <- "2020-10-10"

setwd(path)

# ler todos os arquivos
arquivo_bruto <- list.files(path) %>%
  set_names() %>%
  map_df(read_delim, 
         delim = ",", 
         col_names = FALSE,
         .id = "arquivo")

# separar em colunas
arquivo_tidy <- arquivo_bruto %>%
  separate(arquivo, into = c("tipo_arquivo", "instituto", "cidade", 
                   "categoria", "tipo_categoria"), sep = "_") %>%
  mutate(tipo_categoria = str_remove_all(tipo_categoria, "\\.csv"))
  
# definir cabeçalho 
cabecalho_1 <- colnames(arquivo_tidy[1:5])
cabecalho_2 <- arquivo_tidy[1,6:length(colnames(arquivo_tidy))]

colnames(arquivo_tidy) <- c(cabecalho_1, cabecalho_2)

# tirar linhas excedentes
arquivo_tidy_2 <- arquivo_tidy %>%
  filter(Data != 'Data') 

# ordenar considerando total
ordem_candidatos <- arquivo_tidy_2 %>%
  filter(tipo_categoria == "TOTAL") %>%
  filter(Data == data_para_ordem) %>%
  pivot_longer(cols = everything()) %>%
  mutate(percentual = case_when(str_detect(value, "^[0-9]*$") ~ as.integer(value))) %>%
  filter(percentual != "NA") %>%
  mutate(nao_candidato = case_when(str_detect(name, "/") ~ -1)) %>%
  mutate(nao_candidato = replace_na(nao_candidato, 0)) %>%
  arrange(desc(percentual)) %>%
  arrange(desc(nao_candidato)) %>%
  mutate(ordem = as.character(row_number())) %>%
  select(name, ordem) %>%
  pivot_wider(names_from = "name", values_from = "ordem") %>%
  mutate(categoria = "ORDEM",
         tipo_categoria = "ORDEM", 
         cidade = "ORDEM",
         instituto = "ORDEM",
         tipo_arquivo = "ORDEM",
         Data = data_para_ordem)

# juntar ORDEM + DADOS
arquivo_final <- arquivo_tidy_2 %>%
  bind_rows(ordem_candidatos)

# criar pasta e baixar o arquivo completo
dir.create(paste0(path, "resultado_R_", Sys.Date()))
setwd(paste0(path, "resultado_R_", Sys.Date()))

write.csv(arquivo_final, "arquivo_final.csv")
