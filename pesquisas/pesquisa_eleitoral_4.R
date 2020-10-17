# check: https://discourse.curso-r.com/t/download-de-todos-os-arquivos-do-google-cloud-platform/642/2
# 1) instalar pacote (se necessário)
# install.packages("tidyverse")

# 2) ler pacote
library(tidyverse)

# 3) definir pasta
# ATENÇÃO 'path' e 'data_para_ordem' PRECISAM SER ALTERADOS
# ATENÇÃO: mexer apenas em 'path' e 'data_para_ordem' nas linhas 11 e 12
# ATENÇÃO: apenas uma capital por vez
path <- "~/Downloads/Pesquisa/BH/"
data_para_ordem <- "2020-10-15"

setwd(path)

# 4) ler todos os arquivos
arquivo_bruto <- list.files(path, pattern = "*csv") %>%
  set_names() %>%
  map_df(read_delim, 
         delim = ",", 
         col_names = FALSE,
         .id = "arquivo")

# 5) separar em colunas
arquivo_tidy <- arquivo_bruto %>%
  separate(arquivo, into = c("tipo_arquivo", "instituto", "cidade", 
                             "categoria", "tipo_categoria"), sep = "_") %>%
  mutate(tipo_categoria = str_remove_all(tipo_categoria, "\\.csv"))

# 6) definir cabeçalho 
cabecalho_1 <- colnames(arquivo_tidy[1:5])
cabecalho_2 <- arquivo_tidy[1,6:length(colnames(arquivo_tidy))]

colnames(arquivo_tidy) <- c(cabecalho_1, cabecalho_2)

# 7) tirar linhas excedentes
arquivo_tidy_2 <- arquivo_tidy %>%
  filter(Data != "Data") 

# 8) ordenar considerando total
ordem_candidatos <- arquivo_tidy_2 %>%
  filter(tipo_categoria == "TOTAL") %>%
  filter(Data == data_para_ordem) %>%
  pivot_longer(cols = everything()) %>%
  mutate(percentual = case_when(str_detect(as.numeric(value), 
                                           "^[0-9]*$") ~ as.integer(value))) %>%
  filter(name != "tipo_arquivo" &
         name != "instituto" &
         name != "cidade" &
         name != "categoria" &
         name != "tipo_categoria" &
         name != "Data") %>%
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

# 9) juntar ORDEM + DADOS
max_column <- length(colnames(cabecalho_2)) - 1

arquivo_final <- arquivo_tidy_2 %>%
  rbind(ordem_candidatos) %>%
  select(cabecalho_1, 
         "Data",
         paste(colnames(ordem_candidatos[1:max_column])))

# 10) criar pasta e baixar o arquivo completo
dir.create(paste0(path, "resultado_R_", Sys.Date()))
setwd(paste0(path, "resultado_R_", Sys.Date()))

# 11) separar e baixar arquivos por tipo_categoria
splited_df <- split(arquivo_final, list(arquivo_final$categoria, arquivo_final$tipo_categoria), drop = TRUE)
  
baixar_arquivos <- function(i){
    splited_df %>%
    .[i] %>%
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

map_dfr(1:length(splited_df), baixar_arquivos)
