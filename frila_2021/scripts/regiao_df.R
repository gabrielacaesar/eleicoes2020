# criação de DF para converter UF para região
norte <- data.frame(regiao = "Norte", uf = c("AM", "AC", "AP", "PA", "TO", "RO", "RR"))
nordeste <- data.frame(regiao = "Nordeste", uf = c("MA", "PI", "CE", "RN", "PE", "PB", "SE", "AL", "BA"))
centro_oeste <- data.frame(regiao = "Centro-Oeste", uf = c("MT", "MS", "GO"))
sudeste <- data.frame(regiao = "Sudeste", uf = c("SP", "RJ", "ES", "MG"))
sul <- data.frame(regiao = "Sul", uf = c("RS", "SC", "PR"))

regioes <- rbind(norte, nordeste, centro_oeste, sudeste, sul)

write.csv(regioes, "data_analysis/regioes.csv", row.names = FALSE)
