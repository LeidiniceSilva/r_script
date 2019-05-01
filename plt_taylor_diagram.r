# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 12/10/2018
# Description = Plot precipitation Taylor Diagram


# INIT #

rm(list=ls(all=TRUE))

library("plotrix")
library("forecast")

# Open and read data
data = read.table("rain_inmet_1980_2017.csv", header=TRUE, dec=".", sep=";")
attach(data)
names(data)   
data[1:5,] 

# Creating time series
obs1 = ts(data[,4], frequency=12, start=c(1980,1))
obs2 = ts(data[,5], frequency=12, start=c(1980,1))
obs3 = ts(data[,6], frequency=12, start=c(1980,1))

obs_ts1 = obs1[13:456]
obs_ts2 = obs2[13:456]
obs_ts3 = obs3[13:456]

# Calculated Holt-Winters Fitted
sim1 = HoltWinters(obs1, seasonal="addit")
sim2 = HoltWinters(obs2, seasonal="addit")
sim3 = HoltWinters(obs3, seasonal="addit")

hw_ts1 = sim1$fitted[,1]
hw_ts2 = sim2$fitted[,1]
hw_ts3 = sim3$fitted[,1]

# Aplicated ARIMA model with sazonalite (SARIMA) Fitted
sarima1 = auto.arima(obs1)
sarima2 = auto.arima(obs2)
sarima3 = auto.arima(obs3)

sarima_ts1 = sarima1$fitted[13:456]
sarima_ts2 = sarima2$fitted[13:456]
sarima_ts3 = sarima3$fitted[13:456]

length(obs_ts3) 
length(sim_ts3)
length(sarima_ts3)

# Aplicated SARIMAX model
ammsst = ts(data[,2], frequency=12, start=c(1980,1))

amm_sst = rain$amm_sst
amm_sst

sarimax1 = arima(timeseries_station1, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12), xreg=timeseries_ammsst)
sarimax1

# Plot Taylor Diagram
png("C:/users/leidinice/desktop/ufrn/papers/time_series/results/taylor_diagra_test.png", width=35,height=15, units="cm", res=600) 

par(mfcol=c(1,3))

taylor.diagram(obs_ts1, hw_ts1, pos.cor=T, show.gamma=TRUE, gamma.col="black", ngamma=3, sd.arcs=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, ref.sd=TRUE, col="red", pch=19, main="Obs x Sim - Brasília/DF", ylab="", cex=1.5, cex.main=1.8) 
taylor.diagram(obs_ts1, sarima_ts1, add=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, col="green", pch=15)
legend("topright", c("H-W", "SARIMA", "SARIMAX"), col=c("red", "green", "purple"), pch=c(19,15), cex=1)

taylor.diagram(obs_ts2, hw_ts2, pos.cor=T, show.gamma=TRUE, gamma.col="black", ngamma=3, sd.arcs=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, ref.sd=TRUE, col="red", pch=19, main="Obs x Sim - Manaus/AM", ylab="", cex=1.5, cex.main=1.8)
taylor.diagram(obs_ts2, sarima_ts2, add=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, col="green", pch=15)
legend("topright", c("H-W", "SARIMA", "SARIMAX"), col=c("red", "green", "purple"), pch=c(19,15), cex=1)

taylor.diagram(obs_ts3, hw_ts3, pos.cor=T, show.gamma=TRUE, gamma.col="black", ngamma=3, sd.arcs=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, ref.sd=TRUE, col="red", pch=19, main="Obs x Sim - Recife/PE", ylab="", cex=1.5, cex.main=1.8)
taylor.diagram(obs_ts3, sarima_ts3, add=TRUE, pcex=1.8, normalize=TRUE, sd.method=TRUE, col="green", pch=15)
legend("topright", c("H-W", "SARIMA", "SARIMAX"), col=c("red", "green", "purple"), pch=c(19,15), cex=1)

dev.off()

# THE END # 

