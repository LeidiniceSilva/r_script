# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 12/10/2018
# Description = Time Series Modeling


# INIT #

rm(list=ls(all=TRUE))

require("hydroGOF")

library("WaveletComp")
library("forecast")
library("gof")

# windowsFonts(A = windowsFont("Times New Roman"))

# Part I - Analyzing the characteristics of the stations (Exploring the data)

# Open and read data
rain = read.table("precip_estacoes_inmet_1980_2017_novo.csv", header=T, dec=".", sep=";")
attach(rain)
names(rain)   
rain [1:20,]    

# Exploring the data
summary(rain[,3])
sd=sd(rain[,3])
cv=sd(rain[,3], na.rm=TRUE)/mean(rain[,3], na.rm=TRUE)*100
sd
cv

# Boxplot of precipitation
boxplot(rain[,3]~rain[,2], xlab="Meses", ylab="Precipitação (mm)", main="Boxplot da série de precipitação de Manaus (AM)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
points(1,mean(rain[rain[,2]==1,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(2,mean(rain[rain[,2]==2,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(3,mean(rain[rain[,2]==3,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(4,mean(rain[rain[,2]==4,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(5,mean(rain[rain[,2]==5,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(6,mean(rain[rain[,2]==6,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(7,mean(rain[rain[,2]==7,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(8,mean(rain[rain[,2]==8,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(9,mean(rain[rain[,2]==9,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(10,mean(rain[rain[,2]==10,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(11,mean(rain[rain[,2]==11,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(12,mean(rain[rain[,2]==12,3]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)

# Analisis wavelet power spectrum
my_wave = analyze.wavelet(rain,"Manaus", loess.span=0, dt=1, dj=1/100, n.sim=100)
wt.image(my_wave, color.key="quantile", n.levels=100, periodlab="period (months)", main="Wavelet Power Spectrum")
wt.avg(my_wave, siglvl = c(0.05, 0.1), sigcol = c("red", "blue"), periodlab = "Period (Months)", main="Period Wavelet Power")

# Creating time series
timeseries_station1 = ts(rain[,3], frequency=12, start=c(1980,1))
timeseries_station1

# Time series decompose
par(mfrow=c(1,1))
mxtprec = decompose(timeseries_station1, type='m')
plot(mxtprec)


# Part II - Time series modeling per Holt-Winters method

# Calculate Holt-Winters Fitted
rain_HW = HoltWinters(timeseries_station1, seasonal="addit")
rain_HW

# Constantes de suavização calculadas	
const = c(rain_HW$alpha, rain_HW$beta, rain_HW$gamma)	
const

# Estimativas finais (em t=n) nivel, tendência e componentes sazonais
coeffic = rain_HW$coefficients	
coeffic  
 
# Valores ajustados as componentes calculadas ao longo do tempo         
fitted = rain_HW$fitted	
fitted  

# Cálculo dos valores ajustados ao longo dos níveis          	
level = rain_HW$fitted[1,2]
level

# cálculo da componente tendência em t-12
trend = rain_HW$fitted[1,3]
trend

# cálculo do componente sazonal em t-12
season = rain_HW$fitted[1,4]
season

fit_HW = fitted[,1]
ts_obs = timeseries_station1[13:456]

length(fit_HW)
length(ts_obs) 

# Plot obs time series vs. H-W_fitted 
plot(rain_HW, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="Série Temporal de Precipitação OBS vs. Filtrada H-W de Manaus (AM)", cex=1, cex.main=1.3) 
legend(1980, 800, legend=c("Obs", "Sim_H-W"), col=c("black", "red"), lty=c(1,1))
mtext("Alpha: 0.10267849 - Beta: 0.01677265 - Gamma: 0.19325347")

# Modeling and ploting with predict and forecast functions
pred_HW = predict(rain_HW, n.ahead=12, prediction.interval=TRUE, level=0.95)
pred_HW

plot(rain_HW, pred_HW, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="Previsão de 12 meses pelo método H-W de de Manaus (AM)", cex=1, cex.main=1.3)
legend(1980, 800, legend=c("Obs", "Pred_H-W", "IC 95%"), col=c("black", "red", "blue"), lty=c(1,1))

fcst_HW = forecast(rain_HW, h=12)
fcst_HW

plot(fcst_HW, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="Previsão de 12 meses pelo método H-W de Manaus (AM)", cex=1, cex.main=1.3)
lines(fit_HW, col=2, pch=0.4)
legend(1980, 800, legend=c("Obs", "Sim_H-W", "Previsto"), col=c("black", "red", "blue"), lty=c(1,1))
mtext("Alpha: 0.06259707 - Beta: 0.01667885 - Gamma: 0.11127708")

par(mfrow=c(2,1))
hist(fcst_HW$residuals, col=4, xlim=c(-400,400), ylim=c(0,200), xlab="Resíduos", ylab="Frequência", main="Histograma de Resíduos", cex=1, cex.main=1.3)
plot(fit_HW, ts_obs, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado H-W", ylab="Observado", main="Dispersão entre Observação e Simulação H-W de Manaus (AM)", cex=1, cex.main=1.3)  

# Calculate estatistic methods (Skill)
accuracy(fit_HW, ts_obs)

erros=gof(sim=fit_HW, obs=ts_obs, na.rm=TRUE)
erros

correl=cor(fit_HW, ts_obs)
correl


# Part III - Time series modeling per Box-Jenkins method (ARIMA)

# This time series precipitation have a seasonal sinal
par(mfrow=c(1,1))
plot(acf(timeseries_station1), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Função de Altocorrelação da série temporal de precipitação de Manaus (AM)", cex=1, cex.main=1.3)
plot(pacf(timeseries_station1), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parcial", main="Função de Altocorrelação Parcial da série temporal de precipitação de Manaus (AM)", cex=1, cex.main=1.3)

# Applicated differenciation (1 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(timeseries_station1), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferenciação da série Temporal - 1 Dif", cex=1, cex.main=1.3)
plot(acf(diff(timeseries_station1)), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Função de Altocorrelação da série temporal de precipitação de Recife (PE) - 1 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(timeseries_station1)), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parcial", main="Função de Altocorrelação Parcial da série temporal de precipitação de Recife (PE) - 1 Dif", cex=1, cex.main=1.3)

# Applicated differenciation (1 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(diff(timeseries_station1)), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferenciação da série Temporal - 2 Dif", cex=1, cex.main=1.3)
plot(acf(diff(diff(timeseries_station1))), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Função de Altocorrelação da série temporal de precipitação de Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(diff(timeseries_station1))), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parial", main="Função de Altocorrelação Parcial da série temporal de precipitação de Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)

# Applicated differenciation (12 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(diff(timeseries_station1, lag=12)), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferenciação da série Temporal - 2 Dif", cex=1, cex.main=1.3)
plot(acf(diff(diff(timeseries_station1, lag=12))), ylim=c(-1,1), xlab="Defasagem - Lag 12", ylab="ACF", main="Função de Altocorrelação da série temporal de precipitação Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(diff(timeseries_station1, lag=12))), ylim=c(-1,1), xlab="Defasagem - Lag 12", ylab="ACF Parcial", main="Função de Altocorrelação Parcial da série temporal de precipitação Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)

# Aplicated ARIMA model with sazonalite (SARIMA)
arima1 = auto.arima(timeseries_station1)
arima1

fcst_arima = forecast(arima1, h=12)
fcst_arima

plot(fcst_arima, ylim=c(0, 800), main="Previsão de 12 meses pelo modelo ARIMA (1,0,0)(1,1,0) de Manaus (AM)", xlab="Anos", ylab="Precipitação (mm)", cex=1, cex.main=1.3)
lines(arima1$x, col="black")
lines(fitted(arima1), col="red")
legend(1980, 800, legend=c("Obs", "Sim_ARIMA", "Previsto"), col=c("black","red","blue"), lty=c(1,1))

tsdisplay(residuals(arima1))

par(mfrow=c(1,1))
tsdiag(arima1)
qqnorm(arima1$residuals); qqline(arima1$residuals, col="red")
shapiro.test(arima1$residuals)

# Calculate estatistic methods (Skill)
length(arima1$fitted)
length(timeseries_station1)

accuracy(arima1$fitted, timeseries_station1)

erros=gof(sim=arima1$fitted, obs=timeseries_station1, na.rm=TRUE)
erros

correl=cor(arima1$fitted, timeseries_station1)
correl

Part IV - Time series modeling per Box-Jenkins-Tcao method (SARIMAX)

# Extracting variable amm_sst
timeseries_ammsst = ts(rain$amm_sst, frequency=12, start=c(1980,1))
timeseries_ammsst

amm_sst = rain$amm_sst
amm_sst

sarimax1 = arima(timeseries_station1, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12), xreg=timeseries_ammsst)
sarimax1

amm_sst2017 = c(1.69,0.82,-1.07,0.13,2.16,1.16,2.10,1.93,3.10,2.94,4.48,6.14)
fcst_sarimax = forecast(sarimax1, h=12, xreg=amm_sst2017)
fcst_sarimax

tsdisplay(residuals(sarimax1))

par(mfrow=c(1,1))
tsdiag(sarimax1)
qqnorm(sarimax1$residuals); qqline(sarimax1$residuals, col="red")
shapiro.test(sarimax1$residuals)

plot(fcst_sarimax, ylim=c(0, 800), main="Previsão de 12 meses pelo modelo SARIMAX (1,0,0)(1,1,0) de Manaus (AM)", xlab="Anos", ylab="Precipitação (mm)", cex=1, cex.main=1.3)
lines(sarimax1$x, col="black")
lines(fitted(sarimax1),col="red")
legend(1980, 800, legend=c("Obs", "Sim_SARIMAX", "Previsto"), col=c("black", "red", "blue"), lty=c(1,1))

# Calculate estatistic methods (Skill)
length(fitted(sarimax1))
length(timeseries_station1)

accuracy(fitted(sarimax1), timeseries_station1)

erros=gof(sim=fitted(sarimax1), obs=timeseries_station1, na.rm=TRUE)
erros

correl=cor(fitted(sarimax1), timeseries_station1)
correl

# THE END # 

