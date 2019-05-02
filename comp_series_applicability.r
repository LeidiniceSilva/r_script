# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 05/09/2018
# Description = Series applicability


# INIT #

# abrindo e lendo os dados 

rm(list=ls(all=TRUE))

chuva = read.table("estacoes_exercicio_1.txt", header=T, dec=".", sep="")
attach(chuva); # FIXA OS NOMES DAS COLUNAS
names(chuva);  # MOSTRA OS NOMES 

chuva$p12    # MOSTRA AS PRIMEIRAS 5 LINHAS E TODAS AS COLUNAS

# CLIMATOLOGIA MENSAL 

clima = aggregate(x = chuva, by = list(chuva[,2]), FUN = "mean")[,-(1:2)] 
clima[,13]
round(aggregate(x = chuva, by = list(chuva[,2]), FUN = "mean")[,-(1:2)],2) # com duas casas decimais

sort(chuva[,13])                               
plot(clima[,13],type="b", main="Estação p12", ylim=c(0,400), ylab="Climatologia de Precipitação", xaxt="n", lwd=3);
axis(at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), side=1, line=0, cex=0.5)
 

subset(sort(chuva[,14]), sort(chuva[,14] > 0)) # ORDEM ACIMA DE ZERO

# PERCENTIL DA SÉRIE COMPLETA

summary(chuva[,14]) 
boxplot(chuva[,14], main="Estação p12", ylim=c(0,600), ylab="Boxplot de Precipitação")
points(1,mean(chuva[,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)  

quantile(sort(chuva[,14]), c(.25,0.5,0.75))
quantile(sort(chuva[,14]), c(.05,0.95))

Q95 = quantile(sort(chuva[,14]), c(.05,0.95))
Q95

# MENSAL 

par(mfrow=c(1,2)) # DIVIDE A JANELA 1 LINHA E 2 COLUNAS

boxplot(chuva[,14]~chuva[,2],main="Estação p12",ylim=c(0,1000), xaxt="n"); 
points(1,mean(chuva[chuva[,2]==1,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(2,mean(chuva[chuva[,2]==2,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(3,mean(chuva[chuva[,2]==3,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(4,mean(chuva[chuva[,2]==4,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(5,mean(chuva[chuva[,2]==5,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(6,mean(chuva[chuva[,2]==6,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(7,mean(chuva[chuva[,2]==7,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(8,mean(chuva[chuva[,2]==8,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(9,mean(chuva[chuva[,2]==9,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(10,mean(chuva[chuva[,2]==10,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(11,mean(chuva[chuva[,2]==11,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
points(12,mean(chuva[chuva[,2]==12,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
axis(at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), side=1,line=0,cex=0.5)
identify(chuva[,14]~chuva[,2],labels =chuva[,14],col="4")

# OU 

boxplot(chuva[,14]~chuva[,2],main="Estação p12",ylim=c(0,1000), xaxt="n"); 

# OU FAÇA UMA FUNÇÃO PARA PLOTAR 12 MESES 

boxplot(chuva[,14]~chuva[,2],main="Estação p12",ylim=c(0,1000), xaxt="n"); 
MEDIAmes <-numeric(0)
for(i in 1:12){           
MEDIAmes [i]<- points(i,mean(chuva[chuva[,2]==i,14]),col="4", pch = 18, cex.lab=1.5, cex=1.5)
}                         
axis(at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), side=1, line=0, cex=0.5)
identify(chuva[,14]~chuva[,2],labels =chuva[,14],col="4")

# ANUAL 

boxplot(chuva[,14]~chuva[,1],main="Estação p12",ylim=c(0,1000),lty=5); 
points(1,mean(chuva[chuva[,1]==1961,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)

# OU FAÇA UMA FUNÇÃO PARA PLOTAR 48 ANOS 

boxplot(chuva[,14]~chuva[,1],main="Estação p12",ylim=c(0,1000),lty=5); 
MEDIAanual <-numeric(0)
for(i in 1:49){           
MEDIAanual [i]<- points(i,mean(chuva[chuva[,1]==i+1960,14]),col="4",pch = 18, cex.lab=1.5, cex=1.5)
}                         

# Quando o R necessita que reconheça os dados como séries temporais

names(chuva)
stAREA = ts(chuva[,14],frequency=12,start=c(1961, 1)) # TODA SÉRIE 
stAREA

par(mfrow=c(1,2))

# Anual

plot(chuva[,1],stAREA,col="1",type="p", ylim=c(0,1000),xaxt="n",ylab="Precipitação diária", las=1,cex=1.2);
axis(at=1961:2014, labels=c(1961:2014), side=1,line=0,cex=0.5,las=2)
identify(chuva[,1],stAREA,labels =chuva[,3],col="4")

# mensal

plot(chuva[,2],stAREA,col="1",type="p", ylim=c(0,1000), xaxt="n",ylab="Precipitação diária", las=2,cex=1.2);
axis(at=1:12, labels=c("J","F","M","A","M","J","J","A","S","O","N","D"), side=1,line=0,cex=0.5)
identify(chuva[,2],stAREA,labels =chuva[,3],col="4")

precJan <- ts(chuva[chuva[,2]==1,14],frequency=1,start=c(1961, 1)) 
precFeb <- ts(chuva[chuva[,2]==2,14],frequency=1,start=c(1961, 2))
precMar <- ts(chuva[chuva[,2]==3,14],frequency=1,start=c(1961, 3))
precAbr <- ts(chuva[chuva[,2]==4,14],frequency=1,start=c(1961, 4))
precMay <- ts(chuva[chuva[,2]==5,14],frequency=1,start=c(1961, 5))
precJun <- ts(chuva[chuva[,2]==6,14],frequency=1,start=c(1961, 6))
precJul <- ts(chuva[chuva[,2]==7,14],frequency=1,start=c(1961, 7))
precAug <- ts(chuva[chuva[,2]==8,14],frequency=1,start=c(1961, 8))
precSep <- ts(chuva[chuva[,2]==9,14],frequency=1,start=c(1961, 9))
precOct <- ts(chuva[chuva[,2]==10,14],frequency=1,start=c(1961, 10))
precNov <- ts(chuva[chuva[,2]==11,14],frequency=1,start=c(1961, 11))
precDec <- ts(chuva[chuva[,2]==12,14],frequency=1,start=c(1961, 12))

length(subset(precJan, precJan > Q95[2]))
length(subset(precFeb, precFeb > Q95[2]))
length(subset(precMar, precMar > Q95[2]))
length(subset(precAbr, precAbr > Q95[2]))
length(subset(precMay, precMay > Q95[2]))
length(subset(precJun, precJun > Q95[2]))
length(subset(precJul, precJul > Q95[2]))
length(subset(precAug, precAug > Q95[2]))
length(subset(precSep, precSep > Q95[2]))
length(subset(precOct, precOct > Q95[2]))
length(subset(precNov, precNov > Q95[2]))
length(subset(precDec, precDec > Q95[2]))

length(subset(chuva[,14], chuva[,14] > Q95[2])) # OU DE TODA A SÉRIE

# CONTAGEM ANUAL

names(chuva)

length(subset(chuva[chuva[,1]==2000,3], chuva[chuva[,1]==2000,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2001,3], chuva[chuva[,1]==2001,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2002,3], chuva[chuva[,1]==2002,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2003,3], chuva[chuva[,1]==2003,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2004,3], chuva[chuva[,1]==2004,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2005,3], chuva[chuva[,1]==2005,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2006,3], chuva[chuva[,1]==2006,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2007,3], chuva[chuva[,1]==2007,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2008,3], chuva[chuva[,1]==2008,3] > Q95[2]))
length(subset(chuva[chuva[,1]==2009,3], chuva[chuva[,1]==2009,3] > Q95[2]))


# gerando série temporal dos P95 e análise da tendência na intensidade dos P95

quantile(precJan, c(0.95))
  
subset(precJan, precJan > 441.54) 

# THE END #


