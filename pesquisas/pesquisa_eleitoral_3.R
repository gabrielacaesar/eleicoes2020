# instalar pacote (se necessário)
# install.packages("tidyverse")

# ler pacote
library(tidyverse)

# definir pasta
# ATENÇÃO 'path' e 'data_para_ordem' PRECISAM SER ALTERADOS
# ATENÇÃO: mexer apenas em 'path' e 'data_para_ordem' nas linhas 11 e 12
# ATENÇÃO: apenas uma capital por vez
path <- "C:/Users/acaesar/Downloads/pesquisa_15out2020/Recife/"
data_para_ordem <- "2020-10-02"

setwd(path)

# ler todos os arquivos
arquivo_bruto <- list.files(path, pattern = "*csv") %>%
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
  mutate(tipo_arquivo = "ORDEM",
         instituto = "ORDEM",
         cidade = "ORDEM",
         categoria = "ORDEM",
         tipo_categoria = "ORDEM", 
         Data = data_para_ordem)

# juntar ORDEM + DADOS
max_column <- length(colnames(cabecalho_2)) - 1

arquivo_final <- arquivo_tidy_2 %>%
  rbind(ordem_candidatos) %>%
  select(cabecalho_1, 
         "Data",
         paste(colnames(ordem_candidatos[1:max_column])))

# criar pasta e baixar o arquivo completo
dir.create(paste0(path, "resultado_R_", Sys.Date()))
setwd(paste0(path, "resultado_R_", Sys.Date()))

# separar e baixar arquivos por tipo_categoria
baixar_arquivos <- function(i){
  arquivo_final[i,] %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    `colnames<-`(paste(colnames(arquivo_final))) %>%
    write.csv(., paste0("G1_",
                        .$tipo_arquivo[1],
                        "_",
                        .$instituto[1],
                        "_",
                        .$cidade[1],
                        "_",
                        .$categoria[1],
                        "_",
                        .$tipo_categoria[1],
                        ".csv"),
              fileEncoding = "UTF-8")
}

map_dfr(1:nrow(arquivo_final), baixar_arquivos)
