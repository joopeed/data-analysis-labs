dados <- read.table("C:\\atividade-maquinas-dsc.txt", header=TRUE)
attach(dados)
# Removendo alguns outliers
#dados <- dados[which(intervalo < 1000000), ]
#dados <- subset(dados, ociosa==TRUE)

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
dados <- dados[which(dados$disponibilidade > percentil55_disp && dados$volatilidade < percentil55_vol)]

