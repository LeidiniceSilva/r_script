install.packages("stats")
library(stats)
dados=read.table("c:/monica/curso-R/PRECNEB2.txt", header=T, sep="\t",dec=",");dados

#   header :-> Significa cabe�alho, isto �, atrav�s deste par�metro voc� indica ao programa se deseja ou n�o manter o nome das vari�veis descritas em seu arquivo original.  Escolhas poss�veis: TRUE (T) ou FALSE (F) 
#   dec :-> Indica a separa��o decimal que seu arquivo original ou banco de dados esta utilizando, isto �, se o arquivo esta considerando pontos (.) ou virgula (,). Escolhas poss�veis: "." ou "," ou ";" 
#   sep :->  Indica a separa��o existente entre as colunas no seu arquivo original. � poss�vel considerar esse como o principal par�metro, pois caso voc� informe o s�mbolo errado o programa R ir� importar seu conjunto de maneira errada.


## rotina para preencher series temporais com falhas

install.packages("mice")
require(mice)

### md.pattern exibe padr�es de dados perdidos. 
### quando um quadro de dados ou uma matriz contendo os dados incompletos. Valores em falta s�o codificados como NA's. 
## agora o comando summary mostrar� as estat�sticas de media, mediana, etc...

summary(dados)
md.pattern(dados) 

## Uma representa��o visual talvez mais �til para mostrar que a serie possue falhas
## pode ser vista usando o pacote VIM da seguinte forma 


install.packages("VIM") 
library(VIM) 
aggr_plot <- aggr(dados[,3:11], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
marginplot(data[c(1,2)]) 

##salvando o grafico

png("c:/monica/curso-R/graficofalhas.png") 
aggr_plot <- aggr(dados[,3:11], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern")) 
marginplot(data[c(1,2)])
dev.off() 


##corrigindo as series com as falhas com o 

tempData <- mice(dados,m=5,maxit=50,meth='pmm',seed=500) 
dados2 <- complete(tempData,1) ## completou os dados 
dados2 ## vendo os dados completados


## Algumas notas sobre os par�metros: 
## M = 5 refere-se ao n�mero de conjuntos de dados imputados. Cinco � o valor padr�o. 
## Meth = 'pmm' refere-se ao m�todo de imputa��o. Neste caso, estamos usando  
## a correspond�ncia m�dia preditiva como m�todo de imputa��o.  


## gerando a matriz dos dados para continuar com as analises
##pegando apenas S�o Luis
## fazendo names(dados) percebe-se que a coluna 3 refere-se a S�o Luis

names(dados)
B=matrix(dados[,3],ncol=12,byrow=T) 
colnames(B)<-c("jan", "fev", "mar", "abr", "mai", "jun", "jul" ,"ago", "set" ,"out", "nov", "dez")
rownames(B)<-c(1961:2009)
ts.plot(B[,2])

##pelo grafico percebe-se as falhas

##agora para os dados preenchhidos
windows()
C=matrix(dados2[,3],ncol=12,byrow=T) 
colnames(C)<-c("jan", "fev", "mar", "abr", "mai", "jun", "jul" ,"ago", "set" ,"out", "nov", "dez")
rownames(C)<-c(1961:2009)
ts.plot(C[,2])

ts.plot(C, col=heat.colors(n=2, alpha = 1)) #faz o grafico para todos os meses
ts.plot(C,gpars= list(col=rainbow(12))) # 12 porque tem 12 colunas
legend("topright", legend = 1:12, col = 1:12, lty = 1)

#alpha e a transparencia que vai de 0 a 1 vc pode testar colocar 0; 0.5; 0.85; 1

# legend "topleft", acima esquerda
 #"bottomleft", abaixo esquerda
  #"left", centrada
   #"bottomright", abaixo direita
    #"topright", acima direita

##############################################################
install.packages("lattice") 
library(lattice) 
names(dados2)
densityplot(tempData) 
boxplot(dados2) # coloca a coluna dos anos e meses temos que retirar entao coloca-se conforme exemplo abaixo
boxplot(dados2[,3:11]) # faz o boxplot para cada cidade
boxplot(C) # faz o boxplot para a cidade de S�o Luis 

#boxplot colorido

boxplot(C,col=topo.colors(12))


#############################################################
install.packages("Kendall") 
library(Kendall)
#analise de tendencia
K<-MannKendall(C); K
print(K)#salva as estatisticas do kendall 
summary(K)

#nesse pacote abaixo pode ser obtido Mann Kendall total e sazonal
install.packages("trend") 
require(trend) 

MK<-mk.test(C, continuity = TRUE) 
MK

##para aplicar o teste sazonal de Mann-Kendall � necess�rio
## que os dados estejam em rela��o a sua frequencia, por isso, 
###nesse caso temos que aplicar "ts" nos dados

dados3<-ts(dados2[,3],start=1961,freq=12);  dados3 
res3<-smk.test(dados3, alternative = c("two.sided"), continuity = TRUE)
res3; summary(res3)

#verifique que cada season refere-se cada mes e o simbolo * a nivel de significancia


#com esse mesmo pacote podemos aplicar Pettit Test para saber a quebra na serie do mes de maio 

plot(C[,5],type="l") 
 s.res <- pettitt.test(C[,5])
 n <- s.res$nobs 
 i <- s.res$estimate  
s.1 <- mean(C[1:i,5]) 
 s.2 <- mean(C[(i+1):n,5]) 
 s <- ts(c(rep(s.1,i), rep(s.2,(n-i))))
 tsp(s) <- tsp(C[,5])
 lines(s,col='red', lty=2) 
 print(s.res)



############################################################################ TESTES DE NORMALIDADE ##### install.packages("nortest")

##aplicando os testes 
install.packages("nortest")
require(nortest)

dados4 <- ts(dados2[,3],frequency=12,start=c(1961, 1));dados4
	 
# Visualiza��o do sinal no dom�nio do tempo: 

ts.plot(dados4, xlab="Ano", ylab="Precipita��o", main="Precipita��o em Sao Luis") 
# O per�odo especifica o comprimento de tempo requerido para completar um ciclo # de freq��ncia de uma vari�vel ou objeto.  # Associando per�odos com as estimativas do periodograma podemos visualizar as  # escalas de tempo nas quais as varia��es importantes dos dados est�o ocorrendo. 
spec.ar(dados4)

## esse comando ajusta os dados a um modelo de regress�o e calcula a densidade espectral do modelo ajustado 
windows ( )
mxtmed <- decompose(dados4,type="m")
plot(mxtmed) 


#Intervalo de Confian�a
#como dados4 e uma frequencia atraves o ts nao podemos usar o t.test por isso vamos
#fazer a matriz
dados5=matrix(dados2[,3],ncol=12,byrow=T) 
colnames(C)<-c("jan", "fev", "mar", "abr", "mai", "jun", "jul" ,"ago", "set" ,"out", "nov", "dez")
rownames(C)<-c(1961:2009)

t.test(c(dados5[,5])) ##aplicando teste t para maio
t.test(c(dados5[,2],conf.level = 0.90)) ## aplinado teste t para fevereiro dizendo qual o nivel de confian�a que quer

 
##Uma distribui��o normal � exigida em an�lise estat�stica pelas suas propriedades e
##pela facilidade de conhecer sua probabilidade. Ela � muito usada em m�todos 
##estat�sticos como o teste-t e a ANOVA
##Para verificar se uma amostra � normal utilizaremos de 3 ferramentas importantes: o histograma,
##o QQ-Plot e por fim o uso de um ou v�rios testes de normalidades 
##O histograma mostra a forma de distribui��o quanto mais pr�ximo do m�dia 
##(na forma de um sino) mais evid�ncias dos dados ser normais 

windows() 
par(mfrow = c(2,2)) # esse comando divide a janela gr�fica numa matriz 2x2  hist(dados) 
hist(dados5) 
acf(dados5) ##grafico de auto-correla��o acf(dados5)

##qqnorm � uma fun��o gen�rica cujo m�todo padr�o produz um gr�fico QQ normal ##dos valores em y, enquanto que o qqline adiciona uma linha "padr�o", 
##quantil-quantil, que passa pelo primeiro e o terceiro quartis. 
windows()
qqnorm(dados5);qqline(dados5,col="blue") 

##O quantile plot ou QQ-Plot mostrar� em um gr�fico uma compara��o dois a dois dos 
##quantis te�ricos de uma Normal e os quantis dos seus dados
##assim pelos gr�ficos percebe-se que os dados n�o s�o normais ent�o devemos aplicar ##os teste de normalidades para confirmar que os dados n�o 
##seguem uma distribui��o normal 

install.packages("nortest") 
require(nortest) 
# Teste de Shapiro-Wilk shapiro.test(dados) 
### Teste de Kolmogorov-Smirnov  lillie.test(dados) 
### Teste Cramer-von Mises  cvm.test(dados) 
#### Teste de Anderson-Darlin ad.test(dados) 
#aplicando os teste nos dados ######################### 
# Teste de Shapiro-Wilk 
shapiro.test(dados5) 

### Teste de Kolmogorov-Smirnov 
lillie.test(dados5) 

### Teste Cramer-von Mises
cvm.test(dados5) 


####  Teste de Anderson-Darlin 
ad.test(dados5)

##Testes de Normalidade s�o testes apropriados para testar a hip�tese dos valores que 
##apresentarem uma distribui��o normal (H0) contra a hip�tese de n�o apresentarem
##uma normalidade. Os testes mais usuais s�o: 
#########################################################
##Shapiro-Wilk � Teste de f�cil aplica��o sem o uso de software (manualmente).
## possui a restri��o de ser muito sens�vel � falta de simetria. S�o permitidos valores perdidos e 
##� recomendado amostras grandes 
########################################################
#Kolmogorov-Smirnov � Teste usado para grandes conjuntos de dados. � um dos mais utilizados em conjunto com o teste de Cramer-von Mises
#Cramer-von Mises � � um teste n�o fact�vel sem o uso de softwares e � um dos testes ##mais usados na literatura.
#S�o permitidos valores perdidos e n�o tem restri��es sobre o n�mero de valores. 
#Anderson-Darlin � Apresenta resultados muito parecidos com os do Cramer-von Mises.
#S�o permitidos valores perdidos e n�o tem restri��es sobre o n�mero de valores.  


