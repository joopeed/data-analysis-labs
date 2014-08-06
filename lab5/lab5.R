dados <- read.csv("C:\\comerciais.csv", header=TRUE)
summary(dados)
attach(dados)
table(teste)
table(duracao)
plot(duracao, teste)
qqnorm(teste)
shapiro.test(teste)
library(nortest)
ad.test(teste)
qqnorm(duracao)
shapiro.test(duracao)
library(nortest)
ad.test(duracao)
cor(duracao, teste)
cor.test(duracao, teste, method="spearman")
lm(formula = teste~duracao)
abline(lm(teste~duracao))
r = lm(teste~duracao); summary(r)
qqnorm(r$residuals)
confint(r, level=.95)
library(ggplot2)
ggplot(dados, aes(x=duracao, y=teste)) +
  geom_point(shape=5) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)
predict(r, data.frame(duracao=10), interval="confidence", level = 0.95)
predict(r, data.frame(duracao=100), interval="confidence", level = 0.95)
predict(r, data.frame(duracao=40), interval="confidence", level = 0.95)
predict(r, data.frame(duracao=40), interval="prediction", level = 0.95)



dados <- read.csv("C:\\cartao-credito.csv", header=TRUE)
summary(dados)
attach(dados)
qqnorm(dados$num.cards)
qqnorm(dados$family.size)
qqnorm(dados$income)
install.packages("GGally")
library("GGally")
ggpairs(dados)
plot(income, num.cards)
ggpairs(dados)
ggpairs(dados, aes(colour=num.cards)
)
ggpairs(dados, colour=num.cards)
ggpairs(dados)
cor.test(num.cards,income, method="spearman")
cor.test(num.cards,num.cards, method="spearman")
cor.test(num.cards,num.cars, method="spearman")
cor.test(num.cards,family.size, method="spearman")
cor.test(num.cars,income, method="spearman")

r = lm(formula = num.cards ~ income + family.size)
ggplot(dados, aes(x=income, y=num.cards)) +
  geom_point(aes(size = 3), colour = "red") +   # Use hollow circles
  geom_smooth(method=lm) # Add linear regression line 
#  (by default includes 95% confidence region)
ggplot(dados, aes(x=family.size, y=num.cards)) +
  geom_point(aes(size = 3), colour = "red") +   # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)
summary(r)
qqnorm(r$residuals)

dados <- read.csv("C:\\dados-imc.csv", header=TRUE)
summary(dados)
attach(dados)
qqnorm(imcs)
qqnorm(alturas)
qqnorm(massas)
ad.test(alturas)
ad.test(imcs)
ad.test(massas)
ggplot(dados, aes(x=imc, y=altura)) +
geom_point(aes(size = 3), colour = "red") +   # Use hollow circles
geom_smooth(method=lm)   # Add linear regression line
#  (by default includes 95% confidence region)
ggplot(dados, aes(x=imcs, y=alturas)) +
geom_point(aes(size = 3), colour = "red") +   # Use hollow circles
geom_smooth(method=lm)   # Add linear regression line
#  (by default includes 95% confidence region)
r = lm(formula = alturas ~ imc + massa)
r = lm(formula = alturas ~ imcs + massas)
r = lm(formula = imcs ~ log(massas) + log(alturas))
qqnorm(r$residuals)
r = lm(formula = log(imcs) ~ log(massas) + log(alturas))
qqnorm(r$residuals)
ad.test(r$residuals)
r = lm(formula = imcs ~ massas + alturas)
ad.test(r$residuals)
r = lm(formula = log(imcs) ~ log(massas) + log(alturas))
r
summary(r)

summary(r)
r = lm(formula = alturas ~ imcs )
summary(r)
r = lm(formula = alturas ~ imcs + massas)
summary(r)
qqnorm(r$residuals)
cor.test(imcs, massas, method="spearman")
r = lm(formula = imcs ~ massas + alturas)
qqnorm(r$residuals)
qqnorm(r$residuals)
ad.test(r$residuals)
r = lm(formula = log(imcs) ~ log(massas) + log(alturas)
)
ad.test(r$residuals)
shapiro.test(r$residuals)
r = lm(formula = imcs ~ massas + alturas)
shapiro.test(r$residuals)
r = lm(formula = imcs ~ massas + alturas)
qqnorm(r$residuals)
summary(r)



dados <- read.csv("C:\\dados-educacao.csv", header=TRUE)
bestdados <- c()
for(i in 1:9) { 
  # funcao para melhorar a forma dos dados
  temp <- c()
  cols <- grep(toString(i), colnames(dados))
  temp <- dados[c(1, 2, 3, 4, 5, 6, cols)]
  temp$ano <- toString(i)
  names(temp) <- gsub(toString(i), "", names(temp))
  bestdados <- rbind(bestdados, temp)
}
attach(dados)


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
mm <- ci_by_group(bestdados, measurevar="ano.horas.aula", groupvars="ano")
ggplot(mm, aes(x = ano, y = mean, fill=ano)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(bestdados, measurevar="ano.num.alunos", groupvars="ano")
ggplot(mm, aes(x = ano, y = mean, fill=ano)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) 

mm <- ci_by_group(bestdados, measurevar="aprov", groupvars=c("ano", "rede"))
ggplot(mm, aes(x = ano, y = mean, fill=ano)) + geom_bar(position=position_dodge(.9), stat = "identity", width=.5)  + geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci),width=.2,position=position_dodge(.3)) +facet_grid(rede~., scales="free")

ggplot(dados, aes(ano.horas.aula, fill = regiao)) +
geom_density(alpha = 0.2)
ggplot(dados, aes(ano.horas.aula, fill = regiao) + geom_density(alpha = 0.2)
d
ggplot(dados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2)
ggplot(bestdados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2) +facet_grid(uf~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2) +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2) +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2) + xlim(3, 6) +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = rede)) + geom_density(alpha = 0.2) + xlim(3, 6) +facet_grid(rede~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = local)) + geom_density(alpha = 0.2) + xlim(3, 6) +facet_grid(local~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = regiao)) + geom_density(alpha = 0.2) + xlim(3, 6) +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(aprov, fill = regiao)) + geom_density(alpha = 0.2) + xlim(3, 6) +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(ano.horas.aula, fill = local)) + geom_density(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aprov, fill = regiao)) + geom_density(alpha = 0.2)  +facet_grid(regiao~., scales="free")
ggplot(bestdados, aes(aprov, fill = rede)) + geom_density(alpha = 0.2)  +facet_grid(rede~., scales="free")
ggplot(bestdados, aes(aprov, fill = local)) + geom_density(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aprov, fill = regiao)) + geom_density(alpha = 0.2)
ggplot(bestdados, aes(aprov, fill = ano)) + geom_density(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aprov, fill = local)) + geom_density(alpha = 0.2)  +facet_grid(ano~., scales="free")
ggplot(bestdados, aes(reprv, fill = local)) + geom_density(alpha = 0.2)  +facet_grid(ano~., scales="free")
ggplot(bestdados, aes(reprov, fill = ano)) + geom_density(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aband, fill = ano)) + geom_density(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aband, fill = ano)) + geom_bar(alpha = 0.2)  +facet_grid(local~., scales="free")
ggplot(bestdados, aes(aband, fill = ano)) + geom_bar(alpha = 0.5)
ggplot(bestdados, aes(aband)) + geom_bar(alpha = 0.5)
ggplot(bestdados, aes(aband, fill= rede)) + geom_bar(alpha = 0.5)
ggplot(bestdados, aes(aband, fill= rede))
ggplot(dados, aes(regiao, fill=regiao) + geom_bar()
ggplot(dados, aes(regiao, fill=regiao)) + geom_bar()
ggplot(dados, aes(regiao, fill=regiao)) + geom_bar(alpha = 05.)
ggplot(dados, aes(regiao, fill=regiao) + geom_bar(alpha = 0.5)
ggplot(dados, aes(regiao, fill=regiao) + geom_bar(alpha = 0.5)
ggplot(dados, aes(regiao, fill=regiao)) + geom_bar(alpha = 0.5)
ggplot(dados, aes(fill=regiao)) + geom_bar(alpha = 0.5)
ggplot(dados, aes(regiao, fill=regiao)) + geom_bar(alpha = 0.5)
ggplot(dados, aes(factor(1), fill=factor(regiao))) + geom_bar() + coord_polar(theta = "y")

attach(bestdados)
cor.test(regiao, aprov, method="spearman")

ggplot(bestdados, aes(x=regiao, y=aprov)) +
  geom_point(aes(size = 3), colour = "red") +   # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line
#  (by default includes 95% confidence region)