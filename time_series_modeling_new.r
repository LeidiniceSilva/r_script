# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 22/04/2020
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
rain = read.table("rain_inmet_1980_2017.csv", header=T, dec=".", sep=";")
attach(rain)
names(rain)   
rain [100:300,]    

brasilia = rain[,4]
manaus = rain[,5]
recife = rain[,6]

# Exploring the data
# brasilia
summary(brasilia)
sd_brasilia = sd(brasilia)
cv_brasilia = sd(brasilia, na.rm=TRUE)/mean(brasilia, na.rm=TRUE)*100

# manaus
summary(manaus)
sd_manaus = sd(manaus)
cv_manaus = sd(manaus, na.rm=TRUE)/mean(manaus, na.rm=TRUE)*100

# recife
summary(recife)
sd_recife = sd(recife)
cv_recife = sd(recife, na.rm=TRUE)/mean(recife, na.rm=TRUE)*100

# Boxplot of precipitation
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/boxplot.png", width=15,height=15, units="cm", res=600) 

par(mfrow=c(3,1))
boxplot(rain[,4]~rain[,2], ylab="Precipitação (mm)", main="A) Boxplot da série de precipitação de Brasília (DF)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
points(1,mean(rain[rain[,2]==1,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(2,mean(rain[rain[,2]==2,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(3,mean(rain[rain[,2]==3,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(4,mean(rain[rain[,2]==4,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(5,mean(rain[rain[,2]==5,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(6,mean(rain[rain[,2]==6,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(7,mean(rain[rain[,2]==7,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(8,mean(rain[rain[,2]==8,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(9,mean(rain[rain[,2]==9,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(10,mean(rain[rain[,2]==10,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(11,mean(rain[rain[,2]==11,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(12,mean(rain[rain[,2]==12,4]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)

boxplot(rain[,5]~rain[,2], ylab="Precipitação (mm)", main="B) Boxplot da série de precipitação de Manaus (AM)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
points(1,mean(rain[rain[,2]==1,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(2,mean(rain[rain[,2]==2,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(3,mean(rain[rain[,2]==3,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(4,mean(rain[rain[,2]==4,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(5,mean(rain[rain[,2]==5,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(6,mean(rain[rain[,2]==6,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(7,mean(rain[rain[,2]==7,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(8,mean(rain[rain[,2]==8,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(9,mean(rain[rain[,2]==9,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(10,mean(rain[rain[,2]==10,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(11,mean(rain[rain[,2]==11,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(12,mean(rain[rain[,2]==12,5]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)

boxplot(rain[,6]~rain[,2], xlab="Meses", ylab="Precipitação (mm)", main="C) Boxplot da série de precipitação de Recife (PE)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
points(1,mean(rain[rain[,2]==1,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(2,mean(rain[rain[,2]==2,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(3,mean(rain[rain[,2]==3,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(4,mean(rain[rain[,2]==4,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(5,mean(rain[rain[,2]==5,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(6,mean(rain[rain[,2]==6,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(7,mean(rain[rain[,2]==7,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(8,mean(rain[rain[,2]==8,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(9,mean(rain[rain[,2]==9,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(10,mean(rain[rain[,2]==10,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(11,mean(rain[rain[,2]==11,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
points(12,mean(rain[rain[,2]==12,6]), col="4", pch = 18, cex.lab=1.5, cex=1.5)
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)
dev.off()


# Analisis wavelet power spectrum
my_wave = analyze.wavelet(rain,"brasilia", loess.span=0, dj=1/100, n.sim=100)

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/wavelet_image_brasilia.png", width=15,height=15, units="cm", res=600) 
wt.image(my_wave, color.key="quantile", n.levels=100, periodlab="Período (Meses)", main="A) Espectro de Potência")
dev.off()

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/wavelet_avg_brasilia.png", width=15,height=15, units="cm", res=600) 
wt.avg(my_wave, siglvl = c(0.05, 0.1), sigcol = c("red", "blue"), periodlab = "Período (Meses)", main="Período de potência - Brasilia (DF)")
dev.off()

# Creating time series
timeseries_brasilia = ts(rain[,4], frequency=12, start=c(1980,1))
timeseries_manaus = ts(rain[,5], frequency=12, start=c(1980,1))
timeseries_recife = ts(rain[,6], frequency=12, start=c(1980,1))

# Time series decompose
mxtprec_brasilia = decompose(timeseries_brasilia, type='m')
mxtprec_manaus = decompose(timeseries_manaus, type='m')
mxtprec_recife = decompose(timeseries_recife, type='m')

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/decompose_brasilia.png", width=10,height=16, units="cm", res=600) 
plot(mxtprec_brasilia)
dev.off()

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/decompose_manaus.png", width=10,height=16, units="cm", res=600) 
plot(mxtprec_manaus)
dev.off()

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/decompose_recife.png", width=10,height=16, units="cm", res=600) 
plot(mxtprec_recife)
dev.off()

# Part II - Time series modeling per Holt-Winters method

# Calculate Holt-Winters Fitted
rain_HW = HoltWinters(timeseries_recife, seasonal="addit")
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

# Cálculo dos valores ajustados ao longo dos n?veis          	
level = rain_HW$fitted[1,2]
level

# cálculo da componente tendência em t-12
trend = rain_HW$fitted[1,3]
trend

# cálculo do componente sazonal em t-12
season = rain_HW$fitted[1,4]
season

fit_HW = fitted[,1]
ts_obs = timeseries_recife[13:456]

length(fit_HW)
length(ts_obs) 

# Plot obs time series vs. H-W_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_hw_brasilia.png", width=20,height=15, units="cm", res=600) 
plot(rain_HW, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="A) Série Temporal de Precipitação OBS vs. Filtrada H-W de Brasilia (DF)", cex=1, cex.main=1.3) 
legend(1980, 800, legend=c("Obs", "Sim_H-W"), col=c("black", "red"), lty=c(1,1))
mtext("Alpha: 0.02477703 - Beta: 0.02199106 - Gamma: 0.119794")
mtext("Alpha: 0.06259707 - Beta: 0.01667885 - Gamma: 0.1112771")
mtext("Alpha: 0.1026785 - Beta: 0.01677265 - Gamma: 0.1932535")
dev.off()

# Modeling and ploting with predict and forecast functions
fcst_HW = forecast(rain_HW, h=24)
fcst_HW

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_hw_pre_manaus.png", width=25,height=15, units="cm", res=600) 
plot(fcst_HW, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="B) Previsão de 24 meses pelo método H-W - Manaus (AM)", cex=1, cex.main=1.3)
lines(fit_HW, col=2, pch=0.4)
legend(1980, 800, legend=c("Obs", "Sim_H-W", "Previsto"), col=c("black", "red", "blue"), lty=c(1,1))
mtext("Alpha: 0.06259707 - Beta: 0.01667885 - Gamma: 0.1112771")
dev.off()

png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/hist_recife.png", width=15,height=15, units="cm", res=600) 
par(mfrow=c(2,1))
hist(fcst_HW$residuals, col=4, xlim=c(-400,400), ylim=c(0,200), xlab="Resíduos", ylab="Frequência", main="C) Histograma de Resíduos - Recife (PE)", cex=1, cex.main=1.3)
plot(fit_HW, ts_obs, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado H-W", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
dev.off()

# Calculate estatistic methods (Skill)
accuracy(fit_HW, ts_obs)

erros=gof(sim=fit_HW, obs=ts_obs, na.rm=TRUE)
erros

correl=cor(fit_HW, ts_obs)
correl


# Part III - Time series modeling per Box-Jenkins method (ARIMA)

# This time series precipitation have a seasonal sinal
par(mfrow=c(1,1))
plot(acf(timeseries_station1), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Fun??o de Altocorrela??o da s?rie temporal de precipita??o de Manaus (AM)", cex=1, cex.main=1.3)
plot(pacf(timeseries_station1), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parcial", main="Fun??o de Altocorrela??o Parcial da s?rie temporal de precipita??o de Manaus (AM)", cex=1, cex.main=1.3)

# Applicated differenciation (1 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(timeseries_station1), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferencia??o da s?rie Temporal - 1 Dif", cex=1, cex.main=1.3)
plot(acf(diff(timeseries_station1)), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Fun??o de Altocorrela??o da s?rie temporal de precipita??o de Recife (PE) - 1 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(timeseries_station1)), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parcial", main="Fun??o de Altocorrela??o Parcial da s?rie temporal de precipita??o de Recife (PE) - 1 Dif", cex=1, cex.main=1.3)

# Applicated differenciation (1 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(diff(timeseries_station1)), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferencia??o da s?rie Temporal - 2 Dif", cex=1, cex.main=1.3)
plot(acf(diff(diff(timeseries_station1))), ylim=c(-1,1), xlab="Defasagem", ylab="ACF", main="Fun??o de Altocorrela??o da s?rie temporal de precipita??o de Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(diff(timeseries_station1))), ylim=c(-1,1), xlab="Defasagem", ylab="ACF Parial", main="Fun??o de Altocorrela??o Parcial da s?rie temporal de precipita??o de Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)

# Applicated differenciation (12 Diff) to remove the sazonlite
par(mfrow=c(1,1))
plot(diff(diff(timeseries_station1, lag=12)), ylim=c(-400,400), xlab="Anos", ylab="", main="Diferencia??o da s?rie Temporal - 2 Dif", cex=1, cex.main=1.3)
plot(acf(diff(diff(timeseries_station1, lag=12))), ylim=c(-1,1), xlab="Defasagem - Lag 12", ylab="ACF", main="Fun??o de Altocorrela??o da s?rie temporal de precipita??o Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)
plot(pacf(diff(diff(timeseries_station1, lag=12))), ylim=c(-1,1), xlab="Defasagem - Lag 12", ylab="ACF Parcial", main="Fun??o de Altocorrela??o Parcial da s?rie temporal de precipita??o Manaus (AM) - 2 Dif", cex=1, cex.main=1.3)

# Aplicated ARIMA model with sazonalite (SARIMA)
arima1 = auto.arima(timeseries_station1)
arima1

fcst_arima = forecast(arima1, h=12)
fcst_arima

plot(fcst_arima, ylim=c(0, 800), main="Previs?o de 12 meses pelo modelo ARIMA (1,0,0)(1,1,0) de Manaus (AM)", xlab="Anos", ylab="Precipita??o (mm)", cex=1, cex.main=1.3)
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

plot(fcst_sarimax, ylim=c(0, 800), main="Previs?o de 12 meses pelo modelo SARIMAX (1,0,0)(1,1,0) de Manaus (AM)", xlab="Anos", ylab="Precipita??o (mm)", cex=1, cex.main=1.3)
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

