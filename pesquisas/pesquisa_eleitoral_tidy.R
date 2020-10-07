# instalar pacotes (se necessário)
# install.packages("tidyverse")

# ler pacotes
library(tidyverse)

# definir pasta
path <- "C:/Users/acaesar/Documents/new_pesquisas/RJ/RJ/"
setwd(path)

# ler arquivo
arquivo_bruto <- list.files(path) %>%
     set_names() %>%
     map_df(read_delim, 
            delim = ",", 
            col_names = FALSE,
            .id = "arquivo")
 
# organizar em colunas
arquivo_tidy <- arquivo_bruto %>%
  pivot_longer(cols = everything()) %>%
  rename(coluna = name, candidato = value) %>%
  mutate(arquivo = case_when(str_detect(candidato, "LINHA_") ~ candidato)) %>%
  fill(arquivo, .direction = "down") %>%
  filter(arquivo != candidato) %>%
  arrange(coluna) %>%
  mutate(percentual = case_when(str_detect(candidato, "^[0-9]*$") ~ as.integer(candidato))) %>%
  mutate(data = case_when(str_detect(candidato, "2020") ~ candidato)) %>%
  fill(data, .direction = "down") %>%
  filter(candidato != data) %>%
  fill(percentual, .direction = "up") %>%
  filter(candidato != percentual) %>%
  filter(candidato != "Data") %>%
  select(arquivo, data, candidato, percentual) %>%
  arrange(arquivo)

# separar informações do arquivo
arquivo_tidy_2 <- arquivo_tidy %>%
  mutate(arquivo = str_remove_all(arquivo, "LINHA_")) %>%
  mutate(arquivo = str_remove_all(arquivo, "\\.csv")) %>%
  mutate(arquivo = sub("_", "_-_", arquivo)) %>% 
  separate(arquivo, into = c("cidade", "categoria"), sep = "_-_") %>%
  separate(categoria, into = c("categoria", "recorte"), sep="_(?=[^_]+$)") %>%
  mutate(cidade = str_replace_all(cidade, "BeloHorizonte", "Belo Horizonte")) %>%
  mutate(cidade = str_replace_all(cidade, "RiodeJaneiro", "Rio de Janeiro")) %>%
  mutate(ordem = NA) %>%
  select(cidade, data, categoria, recorte, candidato, percentual, ordem)

# corrigir nome de partidos
arquivo_tidy_3 <- arquivo_tidy_2 %>%
  mutate(candidato = str_replace_all(candidato, "NOVO", "(Novo)"),
         candidato = str_replace_all(candidato, "REDE", "(Rede)"),
         candidato = str_replace_all(candidato, "MDB", "(MDB)"),
         candidato = str_replace_all(candidato, "PROS", "(PROS)"),
         candidato = str_replace_all(candidato, "DEM", "(DEM)"),
         candidato = str_replace_all(candidato, "PSL", "(PSL)"),
         candidato = str_replace_all(candidato, "PSTU", "(PSTU)"),
         candidato = str_replace_all(candidato, "PT", "(PT)"),
         candidato = str_replace_all(candidato, "PSC", "(PSC)"),
         candidato = str_replace_all(candidato, "PSOL", "(PSOL)"),
         candidato = str_replace_all(candidato, "PCO", "(PCO)"),
         candidato = str_replace_all(candidato, "PDT", "(PDT)"),
         candidato = str_replace_all(candidato, "REPUBLICANOS", "(Republicanos)"),
         candidato = str_replace_all(candidato, "PMB", "(PMB)"))

# ordenar considerando total
ordem_candidatos <- arquivo_tidy_3 %>%
  filter(categoria == "TOTAL") %>%
  mutate(nao_candidato = case_when(str_detect(candidato, "/") ~ -1)) %>%
  mutate(nao_candidato = replace_na(nao_candidato, 0)) %>%
  arrange(desc(percentual)) %>%
  arrange(desc(nao_candidato)) %>%
  mutate(ordem = row_number()) %>%
  select(cidade, data, categoria, recorte, candidato, percentual, ordem)
  
# consolidar arquivo final
arquivo_final <- arquivo_tidy_3 %>%
  filter(categoria != "TOTAL") %>%
  filter(cidade != "Belo Horizonte") %>%
  rbind(ordem_candidatos) %>%
  arrange(candidato) %>%
  fill(ordem, .direction = "up") %>%
  arrange(categoria, recorte, ordem)
  
# criar pasta e baixar o arquivo completo
dir.create(paste0(path, "resultado_R_", Sys.Date()))
setwd(paste0(path, "resultado_R_", Sys.Date()))

write.csv(arquivo_final, "arquivo_final.csv")
