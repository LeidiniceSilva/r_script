# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 22/04/2020
# Description = Time Series Modeling


# INIT #

rm(list=ls(all=TRUE))

require(nortest) 
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
manaus = rain[,5]
recife = rain[,6]

# Exploring the data
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
par(mfrow=c(2,1))
boxplot(rain[,5]~rain[,2], xlab="Meses", ylab="Precipitação (mm)", main="A) Boxplot da série de precipitação de Manaus (AM)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
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

boxplot(rain[,6]~rain[,2], xlab="Meses", ylab="Precipitação (mm)", main="B) Boxplot da série de precipitação de Recife (PE)", xaxt="n", ylim=c(0,800), side=1, line=0, cex=1, cex.main=1.3)
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
my_wave = analyze.wavelet(rain,"manaus", loess.span=0, dj=1/100, n.sim=100)
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/wavelet_image_manaus.png", width=15,height=15, units="cm", res=600) 
wt.image(my_wave, color.key="quantile", n.levels=100, periodlab="Período (Meses)", main="A) Espectro de Potência")
dev.off()
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/wavelet_avg_manaus.png", width=15,height=15, units="cm", res=600) 
wt.avg(my_wave, siglvl = c(0.05, 0.1), sigcol = c("red", "blue"), periodlab = "Período (Meses)", main="Período de potência - Manaus (AM)")
dev.off()

# Creating time series
timeseries_manaus = ts(rain[,5], frequency=12, start=c(1980,1))
timeseries_recife = ts(rain[,6], frequency=12, start=c(1980,1))

# Time series decompose
mxtprec_manaus = decompose(timeseries_manaus, type='m')
mxtprec_recife = decompose(timeseries_recife, type='m')
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/decompose_manaus.png", width=10,height=16, units="cm", res=600) 
plot(mxtprec_manaus)
dev.off()
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/decompose_recife.png", width=10,height=16, units="cm", res=600) 
plot(mxtprec_recife)
dev.off()

# Part II - Time series modeling per Holt-Winters method
# Calculate Holt-Winters Fitted - Manaus
rain_hw_manaus = HoltWinters(timeseries_manaus, seasonal="addit")
rain_hw_manaus

# Constantes de suavização calculadas	
const1 = c(rain_hw_manaus$alpha, rain_hw_manaus$beta, rain_hw_manaus$gamma)	
const1

# Estimativas finais (em t=n) nivel, tendência e componentes sazonais
coeffic1 = rain_hw_manaus$coefficients	
coeffic1  
 
# Valores ajustados as componentes calculadas ao longo do tempo         
fitted1 = rain_hw_manaus$fitted	
fitted1  

# Cálculo dos valores ajustados ao longo dos niveis          	
level1 = rain_hw_manaus$fitted[1,2]
level1

# cálculo da componente tendência em t-12
trend1 = rain_hw_manaus$fitted[1,3]
trend1

# cálculo do componente sazonal em t-12
season1 = rain_hw_manaus$fitted[1,4]
season1

# Calculate Holt-Winters Fitted - Recife
rain_hw_recife = HoltWinters(timeseries_recife, seasonal="addit")
rain_hw_recife

# Constantes de suavização calculadas	
const2 = c(rain_hw_recife$alpha, rain_hw_recife$beta, rain_HW_recife$gamma)	
const2

# Estimativas finais (em t=n) nivel, tendência e componentes sazonais
coeffic2 = rain_hw_recife$coefficients	
coeffic2  
 
# Valores ajustados as componentes calculadas ao longo do tempo         
fitted2 = rain_hw_recife$fitted	
fitted2  

# Cálculo dos valores ajustados ao longo dos niveis          	
level2 = rain_hw_recife$fitted[1,2]
level2

# cálculo da componente tendência em t-12
trend2 = rain_hw_recife$fitted[1,3]
trend2

# cálculo do componente sazonal em t-12
season2 = rain_hw_recife$fitted[1,4]
season2

fit_hw_manaus = fitted1[,1]
fit_hw_recife = fitted2[,1]
ts_obs_manaus = timeseries_manaus[13:456]
ts_obs_recife = timeseries_recife[13:456]
length(fit_hw_manaus)
length(fit_hw_recife)
length(ts_obs_manaus) 
length(ts_obs_recife) 

# Plot obs time series vs. H-W_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_hw_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,1))
plot(rain_hw_manaus, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="A) Série Temporal de Precipitação OBS vs. Filtrada H-W de Manaus (AM)", cex=1, cex.main=1.3) 
legend(1980, 800, legend=c("Obs", "Sim_H-W"), col=c("black", "red"), lty=c(1,1))
plot(rain_hw_recife, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="B) Série Temporal de Precipitação OBS vs. Filtrada H-W de Recife (PE)", cex=1, cex.main=1.3) 
legend(1980, 800, legend=c("Obs", "Sim_H-W"), col=c("black", "red"), lty=c(1,1))
dev.off()

# Modeling and ploting with predict and forecast functions
fcst_hw_manaus = forecast(rain_hw_manaus, h=12)
fcst_hw_recife = forecast(rain_hw_recife, h=12)

# Plot residuals H-W_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/hw_hist_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,2))
hist(fcst_HW_manaus$residuals, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="A) Histograma de Resíduos - Manaus (AM)", cex=1, cex.main=1.3)
plot(fit_hw_manaus, ts_obs_manaus, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado H-W", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
hist(fcst_HW_recife$fitted, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="B) Histograma de Resíduos - Recife (PE)", cex=1, cex.main=1.3)
plot(fit_hw_recife, ts_obs_recife, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado H-W", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
dev.off()

fcst_hw_manaus_2018 = c(306.92765,306.57399,344.78358,329.38826,243.44783,133.56795,94.42690,66.99691,95.73239,150.32313,214.49619,300.67461)
fcst_hw_recife_2018 = c(89.67876,98.47989,199.60026,253.84155,341.40304,376.20863,334.17979,147.16178,82.75448,48.54470,26.70258,59.52825)
ts_obs_manaus_2018 = c(215.3,331.4,242.9,280.8,179.4,188.6,54.1,19.5,80.8,56.7,148.4,349.7)
ts_obs_recife_2018 = c(96.5,109.2,133,473.2,231,143,133.8,104.6,59.4,24,40,75.8)

# Calculate estatistic methods (Skill)
accuracy(fit_hw_manaus, ts_obs_manaus)
accuracy(fit_hw_recife, ts_obs_recife)
erros_manaus=gof(sim=fit_hw_manaus, obs=ts_obs_manaus, na.rm=TRUE)
erros_manaus
erros_recife=gof(sim=fit_hw_recife, obs=ts_obs_recife, na.rm=TRUE)
erros_recife
correl_manaus=cor(fit_hw_manaus, ts_obs_manaus)
correl_manaus
correl_recife=cor(fit_hw_recife, ts_obs_recife)
correl_recife

# Part III - Time series modeling per Box-Jenkins method (SARIMA)
# Aplicated ARIMA model with sazonalite (SARIMA)
sarima_manaus = arima(timeseries_manaus,order=c(1,0,0),seasonal=list(order=c(1,1,0),period=12))
sarima_recife = arima(timeseries_recife,order=c(3,0,1),seasonal=list(order=c(2,1,0),period=12))
sarima_manaus
sarima_recife

# Plot obs time series vs. SA_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_sa_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,1))
plot(timeseries_manaus, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="A) Série Temporal de Precipitação OBS vs. Filtrada SARIMA de Manaus (AM)", cex=1, cex.main=1.3) 
lines(sarima_manaus$residuals, col="red")
legend(1980, 800, legend=c("Obs", "Sim_SA"), col=c("black", "red"), lty=c(1,1))
plot(timeseries_recife, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="B) Série Temporal de Precipitação OBS vs. Filtrada SARIMA de Recife (PE)", cex=1, cex.main=1.3) 
lines(sarima_manaus$residuals, col="red")
legend(1980, 800, legend=c("Obs", "Sim_SA"), col=c("black", "red"), lty=c(1,1))
dev.off()

# Plot residuals SA_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/sa_hist_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,2))
hist(sarima_manaus$residuals, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="A) Histograma de Resíduos - Manaus (AM)", cex=1, cex.main=1.3)
plot(fitted(sarima_manaus), ts_obs_manaus, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado SARIMA", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
hist(sarima_recife$residuals, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="B) Histograma de Resíduos - Recife (PE)", cex=1, cex.main=1.3)
plot(fitted(sarima_recife), ts_obs_recife, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado SARIMA", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
dev.off()

fcst_arima_manaus = forecast(arima_manaus, h=12)
fcst_arima_recife = forecast(arima_recife, h=12)
fcst_arima_manaus
fcst_arima_recife 

fcst_sa_manaus_2018 = c(334.29653,331.02813,302.09790,329.96451,222.52318,103.51776,70.92496,50.75983,85.31719,106.7990,178.36179,240.87028)
fcst_sa_recife_2018 = c(67.75434,45.88424,243.38177,227.51674,363.76712,354.74934,334.29382,93.08259,59.86352,31.42813,20.47455,61.99232)

# Calculate estatistic methods (Skill)
accuracy(fitted(sarima_manaus), ts_obs_manaus)
accuracy(fitted(sarima_recife), ts_obs_recife)
erros_manaus=gof(sim=fitted(sarima_manaus), obs=ts_obs_manaus, na.rm=TRUE)
erros_manaus
erros_recife=gof(sim=fitted(sarima_recife), obs=ts_obs_recife, na.rm=TRUE)
erros_recife
correl_manaus=cor(fitted(sarima_recife), ts_obs_manaus)
correl_manaus
correl_recife=cor(fitted(sarima_recife), ts_obs_recife)
correl_recife

Part IV - Time series modeling per Box-Jenkins-Tcao method (SARIMAX)
# Extracting variable amm_sst
timeseries_ammsst = ts(rain$amm_sst, frequency=12, start=c(1980,1))
timeseries_ammsst
amm_sst = rain$amm_sst
amm_sst

sarimax_manaus = arima(timeseries_manaus, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12), xreg=timeseries_ammsst)
sarimax_recife = arima(timeseries_recife, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12), xreg=timeseries_ammsst)
sarimax_manaus
sarimax_recife

amm_sst2018 = c(-0.29,-0.06,0.15,0.29,0.39,0.38,0.16,-0.14,-0.44,-0.70,-0.88,-0.97)
fcst_sarimax_recife = forecast(sarimax_recife, h=12, xreg=amm_sst2018)
fcst_sarimax_manaus = forecast(sarimax_manaus, h=12, xreg=amm_sst2018)
fcst_sarimax_recife
fcst_sarimax_manaus

# Plot obs time series vs. SX_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_sx_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,1))
plot(timeseries_manaus, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="A) Série Temporal de Precipitação OBS vs. Filtrada SARIMAX de Manaus (AM)", cex=1, cex.main=1.3) 
lines(sarimax_manaus$residuals, col="red")
legend(1980, 800, legend=c("Obs", "Sim_SX"), col=c("black", "red"), lty=c(1,1))
plot(timeseries_recife, ylim=c(0,800), xlab="Anos", ylab="Precipitação (mm)", main="B) Série Temporal de Precipitação OBS vs. Filtrada SARIMAX de Recife (PE)", cex=1, cex.main=1.3) 
lines(sarimax_manaus$residuals, col="red")
legend(1980, 800, legend=c("Obs", "Sim_SX"), col=c("black", "red"), lty=c(1,1))
dev.off()

amm_sst2018 = c(-0.87,-0.76,-0.60,-0.41,-0.13,0.06,0.11,0.20,0.43,0.70,0.85,0.81)
fcst_sarimax_manaus = forecast(sarimax_manaus, h=12, xreg=amm_sst2018)
fcst_sarimax_recife = forecast(sarimax_recife, h=12, xreg=amm_sst2018)
fcst_sarimax_manaus
fcst_sarimax_recife

fcst_sx_manaus_2017 = c(299.80926,249.46101,274.04869,322.43950,128.61459,113.84608,88.65749,34.73098,145.55168,153.25356,199.56757,520.85542)
fcst_sx_recife_2017 = c(69.72265,44.74154,212.60580,290.59702,440.30115,315.85176,291.57397,85.11804,72.11993,39.69426,18.88547,52.36339)

# Plot residuals SX_fitted 
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/sx_hist_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,2))
hist(sarimax_manaus$residuals, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="A) Histograma de Resíduos - Manaus (AM)", cex=1, cex.main=1.3)
plot(fitted(sarimax_manaus), ts_obs_manaus, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado SARIMAX", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
hist(sarimax_recife$residuals, col=4, xlim=c(-400,400), ylim=c(0,400), xlab="Resíduos", ylab="Frequência", main="B) Histograma de Resíduos - Recife (PE)", cex=1, cex.main=1.3)
plot(fitted(sarimax_recife), ts_obs_recife, type="p", col=9, pch=3, xlim=c(0,600), ylim=c(0,600), xlab="Simulado SARIMAX", ylab="Observado", main="Dispersão entre Observação e Simulação H-W", cex=1, cex.main=1.3)  
dev.off()

# Calculate estatistic methods (Skill)
accuracy(fitted(sarimax_manaus), ts_obs_manaus)
accuracy(fitted(sarimax_recife), ts_obs_recife)
erros_manaus=gof(sim=fitted(sarimax_manaus), obs=ts_obs_manaus, na.rm=TRUE)
erros_manaus
erros_recife=gof(sim=fitted(sarimax_recife), obs=ts_obs_recife, na.rm=TRUE)
erros_recife
correl_manaus=cor(fitted(sarimax_recife), ts_obs_manaus)
correl_manaus
correl_recife=cor(fitted(sarimax_recife), ts_obs_recife)
correl_recife

# Plot forecast
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/obs_fcst_manaus_recife.png", width=25,height=25, units="cm", res=600) 
par(mfrow=c(2,1))
plot(ts_obs_manaus_2018, type="o", pch=20, ylim=c(0,800), xlab="Meses", ylab="Precipitação (mm)", main="A) Previsão de 12 meses - Manaus (AM) - 2018", xaxt="n", cex=1, cex.main=1.3)
lines(fcst_hw_manaus_2018, type="b", col="blue", pch=20)
lines(fcst_sa_manaus_2018, type="b", col="green", pch=20)
lines(fcst_sx_manaus_2018, type="b", col="orange", pch=20)
legend(1, 800, legend=c("Obs", "Fcst_H-W", "Fcst_SA", "Fcst_SX"), col=c("black", "blue", "green", "orange"), lty=c(1,1))
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)
plot(ts_obs_recife_2018, type="o", pch=20, ylim=c(0,800), xlab="Meses", ylab="Precipitação (mm)", main="B) Previsão de 12 meses - Recife (PE) - 2018", xaxt="n", cex=1, cex.main=1.3)
lines(fcst_hw_recife_2018, type="b", col="blue", pch=20)
lines(fcst_sa_recife_2018, type="b", col="green", pch=20)
lines(fcst_sx_recife_2018, type="b", col="orange", pch=20)
legend(1, 800, legend=c("Obs", "Fcst_H-W", "Fcst_SA", "Fcst_SX"), col=c("black", "blue", "green", "orange"), lty=c(1,1))
axis(at=1:12, labels=c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez"), side=1, line=0, cex=1)
dev.off()

# Teste de Shapiro-Wilk 
shapiro.test(fit_hw_manaus)
shapiro.test(fit_hw_recife)
shapiro.test(sarima_manaus$residuals)
shapiro.test(sarima_recife$residuals)
shapiro.test(sarimax_manaus$residuals)
shapiro.test(sarimax_recife$residuals)

# THE END # 

