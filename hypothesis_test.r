# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 10/13/2018
# Description = Hypothesis test


# INIT #

# abrindo e lendo os dados 

rm(list=ls(all=TRUE))

dados=read.table("estacoes_exercicio_1.txt", header=T)
attach(dados)
names(dados)

verao = subset(dados, mth >= 1 & mth <= 3, select=c(ano,mth,p12))
attach(verao)
names(verao)

m = mean(verao[,3]) #media no verao de entre 1900 e 2014

par(mfrow=c(1,3))

m2005 = mean(verao[verao[,1]==2005,3]) #media no verao em 2005
m2009 = mean(verao[verao[,1]==2009,3]) #media no verao em 2009

boxplot(verao[,3], ylab="Precipitação (mm)", ylim=c(0,400), main="Climatologia - DJF")
points(m,col="4",pch=18)

verao2005 = (verao[verao[,1]==2005,3])
boxplot(verao2005, ylim=c(0,400), main="DJF - 2005")
points(m2005,col="4",pch=18)

verao2009 = (verao[verao[,1]==2009,3])
boxplot(verao2009, ylim=c(0,400), main="DJF - 2009")
points(m2009,col="4",pch=18)

t.test(verao2005,mu=m)
t.test(verao2009,mu=m)
t.test(verao2005,verao2009)


# abrindo e lendo os dados 

clima = round(aggregate(x = dados, by = list(dados[,2]), FUN = "mean")[,-(1:2)],2) 

clima[,14]                 # Coluna p12
dados[dados[,1]==2005,14]  # as linhas de 2005, para coluna de p12
dados[dados[,1]==2009,14]  # as linhas de 2009, para coluna de p12

chuva2005 = dados[dados[,1]==2005,14] # Chuva de Jan a Dez de 2005 da coluna p12
chuva2009 = dados[dados[,1]==2009,14] # Chuva de Jan a Dez de 2009 da coluna p12

# Analise descritiva

par(mfrow=c(1,3))
boxplot(clima[,14], ylab="Precipitação (mm)", ylim=c(0,400), main="Climatologia");
points(mean(clima[,4]),col="red",pch=18)
 
boxplot(chuva2005, ylim=c(0,400), main="Ano - 2005")
points(mean(clima[,4]),col="red",pch=18)

boxplot(chuva2009, ylim=c(0,400), main="Ano - 2009")
points(mean(clima[,4]),col="red",pch=18)

var.test(chuva2005, clima[,14]) # Compare o resultado com Boxplot
var.test(chuva2009, clima[,14]) # Compare o resultado com Boxplot
var.test(chuva2005, chuva2009) # 

t.test(chuva2005, clima[,13])
t.test(chuva2009, clima[,13])
t.test(chuva2005,chuva2009)

# THE END #
