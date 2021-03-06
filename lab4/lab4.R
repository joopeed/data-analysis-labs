dados <- read.table("C:\\velocidade-isp.txt", header=TRUE)
attach(dados)
dias_da_semana <- c("Quinta", "Sexta", "Sabado", "Domingo", "Segunda", "Terca", "Quarta")
dias_da_semana2 <- c("Segunda", "Terca", "Quarta", "Quinta", "Sexta", "Sabado", "Domingo")
for(i in 1:nrow(dados)) {
  if(dados$ano[i] == 2011){
      dados$dia_semana[i] <- dias_da_semana[dados$dia[i]]
  }  
  else {
      dados$dia_semana[i] <- dias_da_semana2[dados$dia[i]]
  }  
}

dados_pb <- dados[which(estado == "PB"),]
summary(dados_pb)
library(ggplot2)
attach(dados_pb)
ggplot(dados_pb, aes( x = velocidade)) + geom_density()
ggplot(dados_pb, aes(fill=provedor,  x = velocidade)) + geom_density() +facet_grid(provedor~., scales="free")
tapply(velocidade, provedor, mean)
tapply(velocidade, provedor, median)
boxplot(velocidade~provedor, dados_pb, fill=provedor)
library(e1071)
kurtosis(dados_pb[which(provedor == "GZT"),]$velocidade)/sqrt(24/length(dados_pb[which(provedor == "GZT"),]$velocidade))
kurtosis(dados_pb[which(provedor == "Ola"),]$velocidade)/sqrt(24/length(dados_pb[which(provedor == "Ola"),]$velocidade))
skewness(dados_pb[which(provedor == "GZT"),]$velocidade)/sqrt(6/length(dados_pb[which(provedor == "GZT"),]$velocidade))
skewness(dados_pb[which(provedor == "Ola"),]$velocidade)/sqrt(6/length(dados_pb[which(provedor == "Ola"),]$velocidade))


ci_by_group <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                        conf.interval=.95, .drop=TRUE) {
  require(plyr)
  
  #Usando a funcao de raquel e adaptando
  ci = function(x, alpha = 0.01) {
    q = 1 - alpha/2
    s = sd(x)
    xbar = mean(x)
    n = length(x)
    if(n < 30) { 
      #usando t-student
      error = qt(q, n - 1)*s/sqrt(n)
    } else {
      #usando normal
      error = qnorm(q)*s/sqrt(n)
    }
    
    return(error)
  }
  
  #criando colunas para facilitar a visualizacao e obtencao de graficos
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun= function(xx, col, na.rm) {
                   c( N    = length (xx[,col]),
                      mean = mean   (xx[,col]),
                      sd   = sd     (xx[,col]),
                      ci   = ci     (xx[,col])
                   )
                 },
                 measurevar,
                 na.rm
  )
  
  return(datac)
}
mm <- ci_by_group(dados_pb, measurevar="velocidade", groupvars="dia_semana")
ggplot(mm, aes(x = dia_semana, y = mean, fill=dia_semana)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

qqnorm(dados_pb[which(provedor == "Ola"),]$velocidade)
qqnorm(dados_pb[which(provedor == "GZT"),]$velocidade)

mm <- ci_by_group(dados_pb, measurevar="velocidade", groupvars="provedor")
ggplot(mm, aes(x = dia_semana, y = mean, fill=dia_semana)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

shapiro.test(dados_pb$velocidade)
library(nortest)
ad.test(dados_pb$velocidade)
library(lawstat)
bartlett.test(dados_pb$velocidade, dados_pb$provedor)
levene.test(dados_pb$velocidade, dados_pb$provedor)


t.test(dados_pb[which(provedor == "GZT"),]$velocidade, dados_pb[which(provedor == "Ola"),]$velocidade, paired=F, var.equal=T, alternative="less")

t.test(dados_pb[which(provedor == "GZT"),]$velocidade, paired=F, var.equal=T, alternative="less", mu = 50)

t.test(dados_pb[which(provedor == "Ola"),]$velocidade, paired=F, var.equal=T, alternative="less", mu = 50)

gzt <- dados[which(provedor == "GZT"),]
dados_rj <- dados[which(estado == "RJ"),]
qqnorm(dados_rj$velocidade)
dados_sp <- dados[which(estado == "SP"),]
qqnorm(dados_sp$velocidade)
shapiro.test(dados_rj$velocidade)
ad.test(dados_rj$velocidade)
shapiro.test(dados_sp$velocidade)
ad.test(dados_sp$velocidade)
bartlett.test(gzt$velocidade, gzt$estado)
levene.test(gzt$velocidade, gzt$estado)
attach(gzt)
boxplot(velocidade ~ estado)
oneway.test(velocidade ~ estado, var.equal=TRUE)

mm <- ci_by_group(gzt, measurevar="velocidade", groupvars="estado")
ggplot(mm, aes(x = estado, y = mean, fill=estado)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

t.test(gzt[which(estado == "PB"),]$velocidade, gzt[which(estado == "RJ"),]$velocidade, paired=F, var.equal=T, alternative="less")
t.test(gzt[which(estado == "PB"),]$velocidade, gzt[which(estado == "RJ"),]$velocidade, paired=F, var.equal=T, alternative="less")
t.test(gzt[which(estado == "SP"),]$velocidade, gzt[which(estado == "RJ"),]$velocidade, paired=F, var.equal=T, alternative="less")
t.test(gzt[which(estado == "SP"),]$velocidade, gzt[which(estado == "PB"),]$velocidade, paired=F, var.equal=T, alternative="less")
t.test(gzt[which(estado == "PB"),]$velocidade, gzt[which(estado == "PB"),]$velocidade, paired=F, var.equal=T, alternative="less")

