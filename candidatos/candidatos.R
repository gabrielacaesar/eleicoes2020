library(tidyverse)
library(data.table)

# colunas para eliminar
drop_columns <- c("DT_GERACAO", "HH_GERACAO", "CD_TIPO_ELEICAO", 
                  "NM_TIPO_ELEICAO", "CD_ELEICAO", 
                  "DT_ELEICAO", "TP_ABRANGENCIA", "CD_CARGO", 
                  "CD_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", 
                  "NR_PARTIDO", "NM_PARTIDO", "NM_SOCIAL_CANDIDATO", "CD_NACIONALIDADE", 
                  "CD_MUNICIPIO_NASCIMENTO", "CD_GRAU_INSTRUCAO", 
                  "CD_ESTADO_CIVIL", "CD_COR_RACA", "CD_OCUPACAO", 
                  "CD_SIT_TOT_TURNO", "CD_SITUACAO_CANDIDATO_PLEITO", 
                  "CD_SITUACAO_CANDIDATO_URNA", "DS_NACIONALIDADE", 
                  "SG_UF_NASCIMENTO", "NM_MUNICIPIO_NASCIMENTO", 
                  "DT_NASCIMENTO", "NR_IDADE_DATA_POSSE", "DS_GRAU_INSTRUCAO",
                  "DS_ESTADO_CIVIL", "DS_OCUPACAO", "DS_SIT_TOT_TURNO",
                  "NR_PROTOCOLO_CANDIDATURA", "DS_SITUACAO_CANDIDATO_PLEITO",
                  "DS_SITUACAO_CANDIDATO_URNA", "ST_CANDIDATO_INSERIDO_URNA",
                  "VR_DESPESA_MAX_CAMPANHA", "NR_PROCESSO", "TP_AGREMIACAO")

# definindo texto como classe da coluna
class_columns <- c(NR_CPF_CANDIDATO = "character", 
                   SQ_CANDIDATO = "character",
                   SQ_COLIGACAO = "character",
                   NR_TITULO_ELEITORAL_CANDIDATO = "character",
                   NR_PROCESSO = "character")

# definindo nomes para cabeçalho de DF de 2012, 2008, 2006, 2004 e 2000
names_columns <- c("DT_GERACAO", "HH_GERACAO", "ANO_ELEICAO", 
                  "NR_TURNO", "DS_ELEICAO", "SG_UF", 
                  "SG_UE", "NM_UE", "CD_CARGO", 
                  "DS_CARGO", "NM_CANDIDATO", "SQ_CANDIDATO", 
                  "NR_CANDIDATO", "NR_CPF_CANDIDATO", "NM_URNA_CANDIDATO", 
                  "CD_SITUACAO_CANDIDATURA", "DS_SITUACAO_CANDIDATURA", 
                  "NR_PARTIDO", "SG_PARTIDO", "NM_PARTIDO", 
                  "CD_LEGENDA", "SGL_LEGENDA", "DS_COMPOSICAO_COLIGACAO", 
                  "NM_COLIGACAO", "CD_OCUPACAO", "DS_OCUPACAO", 
                  "DT_NASCIMENTO", "NR_TITULO_ELEITORAL_CANDIDATO", 
                  "NR_IDADE_DATA_ELEICAO", "CD_GENERO", "DS_GENERO", 
                  "CD_GRAU_INSTRUCAO", "DS_GRAU_INSTRUCAO", 
                  "CD_ESTADO_CIVIL", "DS_ESTADO_CIVIL", 
                  "CD_NACIONALIDADE", "DS_NACIONALIDADE", 
                  "SG_UF_NASCIMENTO", "CD_MUNICIPIO_NASCIMENTO", 
                  "NM_MUNICIPIO_NASCIMENTO", "DESPESA_MAX_CAMPANHA", 
                  "CD_SIT_TOT_TURNO", "DS_SIT_TOT_TURNO", "NM_EMAIL")

# leitura de BRASIL - 2020
cand_2020 <- fread("C:/Users/acaesar/Downloads/24set2020/consulta_cand_2020_24set2020/consulta_cand_2020_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

# leitura de BRASIL - 2016
cand_2016 <- fread("C:/Users/acaesar/Downloads/candidatos/consulta_cand_2016/consulta_cand_2016_BRASIL.csv",
                   encoding = "Latin-1", drop = drop_columns, colClasses = class_columns)

# leitura de todas as UFs - 2012
path_2012 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2012/"
cand_2012 <- map_df(paste0(path_2012, list.files(path_2012, pattern = "*txt")), fread, 
                           encoding = "Latin-1", col.names = names_columns, drop = drop_columns, colClasses = class_columns)

# leitura de todas as UFs - 2008
path_2008 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2008/"
cand_2008 <- map_df(paste0(path_2008, list.files(path_2008, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                    CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                    CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                    CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                    CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                    CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))

# leitura de todas as UFs - 2004
path_2004 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2004/"
cand_2004 <- map_df(paste0(path_2004, list.files(path_2004, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                                                               CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                                                               CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                                                               CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                                                               CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                                                               CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))


# leitura de todas as UFs - 2000
path_2000 <- "C:/Users/acaesar/Downloads/candidatos/consulta_cand_2000/"
cand_2000 <- map_df(paste0(path_2000, list.files(path_2000, pattern = "*txt")), read_delim, delim = ";",
                    col_names = names_columns, col_types = cols(SG_UE = col_character(), 
                                                               CD_SITUACAO_CANDIDATURA = col_character(), NR_PARTIDO = col_character(),
                                                               CD_LEGENDA = col_character(), CD_OCUPACAO = col_character(), 
                                                               CD_GENERO = col_character(), CD_GRAU_INSTRUCAO = col_character(),
                                                               CD_ESTADO_CIVIL = col_character(), CD_NACIONALIDADE = col_character(),
                                                               CD_MUNICIPIO_NASCIMENTO = col_character(), CD_SIT_TOT_TURNO = col_character()),
                    locale = readr::locale(encoding = "latin1"))

# binding DFs
cand_2000_2020 <- cand_2000 %>%
  rbind(cand_2004, cand_2008, cand_2012) %>%
  filter(!str_detect(DT_GERACAO, "Elapsed")) %>%
  mutate("ST_DECLARAR_BENS" = "NA",
         "ST_REELEICAO" = "NA",
         "DS_DETALHE_SITUACAO_CAND" = "NA",
         "DS_DETALHE_SITUACAO_CAND" = "NA",
         "SQ_COLIGACAO" = "NA",
         "DS_COR_RACA" = "NA") %>%
  select("ANO_ELEICAO", "DS_ELEICAO", "NR_TURNO", "SG_UF", "SG_UE", "NM_UE", 
         "DS_CARGO", "SQ_CANDIDATO", "NR_CANDIDATO", 
         "NM_CANDIDATO", "NM_URNA_CANDIDATO", "NR_CPF_CANDIDATO", 
         "NM_EMAIL", "DS_SITUACAO_CANDIDATURA", 
         "DS_DETALHE_SITUACAO_CAND", "SG_PARTIDO", 
         "SQ_COLIGACAO", "NM_COLIGACAO", 
         "DS_COMPOSICAO_COLIGACAO", 
         "NR_TITULO_ELEITORAL_CANDIDATO", "CD_GENERO",
         "DS_GENERO", "DS_COR_RACA", "ST_REELEICAO", 
         "ST_DECLARAR_BENS") %>%
  rbind(cand_2016, cand_2020) %>%
  mutate(CD_GENERO = ifelse(str_detect(CD_GENERO, "4"),
                            "FEMININO", CD_GENERO)) %>%
  mutate(CD_GENERO = ifelse(str_detect(CD_GENERO, "2"),
                            "MASCULINO", CD_GENERO)) %>%
  filter(DS_ELEICAO == "ELEICOES 2000" |
         DS_ELEICAO == "ELEICOES 2004" |
         DS_ELEICAO == "Eleições 2008" |
         DS_ELEICAO == "ELEIÇÃO MUNICIPAL 2012" |
         DS_ELEICAO == "Eleições Municipais 2016" |
         DS_ELEICAO == "Eleições Municipais 2020")

# saveRDS(cand_2000_2020, file = "cand_2000_2020.rds")
  
t <- cand_2000_2020 %>% 
  group_by(ANO_ELEICAO, DS_CARGO, CD_GENERO) %>% 
  summarise(int = n()) %>%
  filter(DS_CARGO == "VEREADOR") %>%
  pivot_wider(names_from = "CD_GENERO", values_from = "int") %>%
  mutate(total = FEMININO + MASCULINO,
         fem_perc = round((FEMININO / total) * 100),
         mas_perc = round((MASCULINO / total)) * 100)
