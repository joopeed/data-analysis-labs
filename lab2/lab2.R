dados <- read.table("atividade-maquinas-dsc.txt", header=TRUE)
//Trocando nomes dos laboratorios
dados$laboratorio <- gsub("lccpeer@xmpp.ourgrid.org", "LCC1", dados$laboratorio)
dados$laboratorio <- gsub("peer-lcc2@xmpp.ourgrid.org", "LCC2", dados$laboratorio)
dados$laboratorio <- gsub("gaita@gaita.gmf.ufcg.edu.br", "SPLab", dados$laboratorio)
dados$laboratorio <- gsub("peer-lsd@xmpp.ourgrid.org", "LSD", dados$laboratorio)
//Quantidade de intervalos por laboratorio
table(dados$laboratorio)
//Quantidade de intervalos por maquina
maq <- table(dados$maquina)
//Maquinas com mais dados
maq[order(maq, decreasing=TRUE)]
//Separando dados sobre maquinas ociosas
dados_ociosos <- subset(dados, ociosa==TRUE)
// e nao ociosas
dados_nao_ociosos <- subset(dados, ociosa==FALSE)
//Maquinas ociosas com mais dados
maqo <- table(dados_ociosos$maquina)
maqo[order(maqo, decreasing=TRUE)]
summary(dados_ociosos)
//Vendo os outliers mais drasticos
dados_ociosos[which(intervalo > 2000000), ]
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
