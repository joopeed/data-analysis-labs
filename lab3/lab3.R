dados <- read.csv("C:\\dados-deputados.csv", header=TRUE)
//Verificando se ha nomes repetidos
attach(dados)
library(ggplot2)
library(plyr)
which(table(nome) != 1)
ggplot(dados, aes(factor(estado), fill=factor(estado))) + geom_bar()
ggplot(dados, aes(factor(1), fill=factor(regiao))) + geom_bar() + coord_polar(theta = "y")
ggplot(dados, aes(factor(partido), fill=factor(partido))) + geom_bar() 
ggplot(dados, aes(factor(posicao), fill=factor(posicao))) + geom_bar()
ggplot(dados, aes(fill=factor(regiao), x = gastos.total)) + geom_density()
qplot(gastos.total, ..density.., data=dados, geom="density", fill=regiao, position="stack")
qplot(gastos.total, ..count.., data=dados, geom="density", fill=regiao, position="stack")
qplot(gastos.total, ..count.., data=dados, geom="density", fill=posicao, position="stack")
ggplot(dados, aes(regiao, gastos.total)) + geom_boxplot(aes(fill=regiao))
ggplot(dados, aes(regiao, presencas.total)) + geom_boxplot(aes(fill=regiao)) 
ddply(dados, c("partido"), summarize, median_gasto=median(gastos.total), sum_gasto=sum(gastos.total), median_presenca=median(presencas.total))
sort(tapply(presencas.total, partido, median), decreasing=TRUE)
sort(tapply(presencas.total, posicao, median), decreasing=TRUE)
sort(tapply(gastos.total, posicao, median), decreasing=TRUE)

mm <- ci_by_group(dados, measurevar="gastos.total", groupvars="regiao")
ggplot(mm, aes(x = regiao, y = mean, fill=regiao)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(dados, measurevar="gastos.total", groupvars="posicao")
ggplot(mm, aes(x = posicao, y = mean, fill=posicao)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3))

mm <- ci_by_group(dados, measurevar="presencas.total", groupvars="regiao")
ggplot(mm, aes(x = regiao, y = mean, fill=regiao)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(dados, measurevar="presencas.total", groupvars="posicao")
ggplot(mm, aes(x = posicao, y = mean, fill=posicao)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3))


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

mm <- ci_by_group(dados, measurevar="gastos.total", groupvars="estado")
ggplot(mm, aes(x = estado, y = mean, fill=estado)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(dados, measurevar="gastos.total", groupvars="partido")
ggplot(mm, aes(x = partido, y = mean, fill=partido)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 


mm <- ci_by_group(dados, measurevar="presencas.total", groupvars="estado")
ggplot(mm, aes(x = estado, y = mean, fill=estado)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(dados, measurevar="presencas.total", groupvars="partido")
ggplot(mm, aes(x = partido, y = mean, fill=partido)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

dados[which(estado == "DF"),]

gg <- ddply(dados, c("regiao", "estado"), summarize, median_gasto=median(gastos.total), sum_gasto=sum(gastos.total), median_presenca=median(presencas.total))
tt<- gg[order(gg$median_presenca),]