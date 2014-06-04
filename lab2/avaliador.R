args <- commandArgs(trailingOnly = TRUE)
dados <- read.table(args[1], header=TRUE)
attach(dados)
//Range rule of thumb
dados <- dados[which(intervalo < media + 2*desvio),]
dados <- dados[which(intervalo > media - 2*desvio),]

# Inserindo uma coluna com as volatilidades das maquinas
volatilidades <- table(dados$maquina)
dados$volatilidade <- 1 
for(i in 1:nrow(volatilidades)) { 
  respectivos <- which(names(volatilidades[i]) == dados$maquina)
  for(j in respectivos) {
    dados$volatilidade[j] <- volatilidades[i]
  }
}

# Inserindo uma coluna com as disponibilidades das maquinas
disponibilidades <- tapply(dados$intervalo, dados$maquina, sum)
dados$disponibilidade <- 1 
for(i in 1:nrow(disponibilidades)) { 
  respectivos <- which(names(disponibilidades[i]) == dados$maquina)
  for(j in respectivos) {
    dados$disponibilidade[j] <- disponibilidades[i]
  }
}
# Calculando os percentis
percentil55_vol <- quantile(volatilidades, c(.55), na.rm=TRUE)
percentil55_disp <- quantile(disponibilidades, c(.55), na.rm=TRUE)

# Colocando a restricao do desafio, nao sobrou nada :(
dados <- dados[which(dados$disponibilidade > percentil55_disp),]
dados <- dados[which(dados$volatilidade < percentil55_vol),]

media_filtrada_vol <- mean(dados$volatilidade)
mediana_filtrada_vol <- median(dados$volatilidade)
media_filtrada_disp <- mean(dados$disponibilidade)
mediana_filtrada_disp <- median(dados$disponibilidade)

dados <- dados[order(dados$disponibilidade, decreasing=TRUE),]
mais_disponivel <- dados[1,]

porcentagem_media_disponivel <- 100/media_filtrada_disp*mais_disponivel$disponibilidade
porcentagem_mediana_disponivel <- 100/mediana_filtrada_disp*mais_disponivel$disponibilidade
porcentagem_media_volatilidade <- 100/media_filtrada_vol*mais_disponivel$volatilidade
porcentagem_mediana_volatilidade <- 100/mediana_filtrada_vol*mais_disponivel$volatilidade

sink("maquina_escolhida.txt")
cat(mais_disponivel$maquina) 
cat("Porcentagem de mais disponivel que a media ")
cat(porcentagem_media_disponivel)
cat("Porcentagem de mais disponivel que a mediana ")
cat(porcentagem_mediana_disponivel)
cat("Porcentagem de mais volatil que a media ")
cat(porcentagem_media_volatilidade)
cat("Porcentagem de mais volatil que a mediana ")
cat(porcentagem_mediana_volatilidade)
sink()
