dados <- read.table("C:\\atividade-maquinas-dsc.txt", header=TRUE)
install.packages("e1071")
//Trocando nomes dos laboratorios
dados$laboratorio <- gsub("lccpeer@xmpp.ourgrid.org", "LCC1", dados$laboratorio)
dados$laboratorio <- gsub("peer-lcc2@xmpp.ourgrid.org", "LCC2", dados$laboratorio)
dados$laboratorio <- gsub("gaita@gaita.gmf.ufcg.edu.br", "SPLab", dados$laboratorio)
dados$laboratorio <- gsub("peer-lsd@xmpp.ourgrid.org", "LSD", dados$laboratorio)
//Removendo outliers
media <- mean(dados$intervalo)
desvio <- sd(dados$intervalo)
dados <- dados[which(intervalo < media + 2*desvio),]
//Quantidade de intervalos por laboratorio
table(dados$laboratorio)
//Quantidade de intervalos por maquina
maq <- table(dados$maquina)
//Maquinas com mais dados
maq[order(maq, decreasing=TtRUE)]
//Separando dados sobre maquinas ociosas
dados_ociosos <- subset(dados, ociosa==TRUE)
// e nao ociosas
dados_nao_ociosos <- subset(dados, ociosa==FALSE)
//Maquinas ociosas com mais dados
maqo <- table(dados_ociosos$maquina)
maqo[order(maqo, decreasing=TRUE)]
summary(dados_ociosos)
// 45 dias seguidos de trabalho 
// 12410   3960692   TRUE       gato_1.lsd.ufcg.edu.br@xmpp.ourgrid.org         LSD

library("ggplot2")
//Visualizando boxplots por laboratorio e por status ocioso
ggplot(dados, aes(ociosa, intervalo)) + geom_boxplot(aes(fill=laboratorio)) + facet_grid(laboratorio~., scales="free")
// Vendo a densidade dos intervalos
ggplot(dados_ociosos, aes(x = intervalo)) + geom_density()
// Vendo os intervalos de ociosidade por maquina e por lab
ggplot(dados_ociosos, aes(maquina, intervalo)) + geom_point(aes(colour=laboratorio, size=intervalo)) + scale_size_area()
//Vendo a porcentagem de dados por laboratorio
prop.table(table(dados$laboratorio))
//Somandos os tempos por stuatus ocioso e laboratorio
tempos_por_lab <- ddply(dados, c("laboratorio", "ociosa"), summarize, sum=sum(intervalo))
// Vendo a proporcao de status ocioso por laboratorio
ggplot(tempos_por_lab, aes(laboratorio, sum,  fill=ociosa)) + geom_bar(stat="identity")
//Vendo a quantidade de tempo total de ociosidade por maquina
somas_por_maquina <- tapply(dados_ociosos$intervalo, dados_ociosos$maquina, sum)
ordem <- order(somas_por_maquina, decreasing=TRUE)
somas_por_maquina[ordem]
//Vendo a propor��o total de trabalho por laboratorio nos dados
somas_por_lab <- sort(tapply(dados_ociosos$intervalo, dados_ociosos$laboratorio, sum))
sort(tapply(dados_ociosos$intervalo, dados_ociosos$laboratorio, median))
ggplot(tempos_por_lab, aes(ociosa, sum,  fill=laboratorio)) + geom_bar(stat="identity")

ociosidade <- ddply(dados, c("ociosa"), summarize, sum=sum(intervalo))
ggplot(ociosidade, aes(ociosa, sum,  fill=ociosa)) + geom_bar(stat="identity")

ddply(dados, c("ociosa"), summarize, median=median(intervalo))
ddply(dados, c("ociosa"), summarize, mode=mode(intervalo))
ddply(dados, c("ociosa"), summarize, mean=mean(intervalo))


quantile(dados_ociosos$intervalo, c(.25), na.rm=TRUE)
quantile(dados_nao_ociosos$intervalo, c(.25), na.rm=TRUE)
quantile(dados_nao_ociosos$intervalo, c(.75), na.rm=TRUE)
quantile(dados_ociosos$intervalo, c(.75), na.rm=TRUE)

sd(dados_ociosos$intervalo)
sd(dados_nao_ociosos$intervalo)

library(e1071)
skewness(dados$intervalo)
kurtosis(dados$intervalo)
