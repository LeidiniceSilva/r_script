# Author      = Leidinice Silva
# Email       = leidinicesilva@gmail.com
# Date        = 04/30/2018
# Description = Plot time series from SST


# INIT #

rm(list=ls(all=TRUE))

# Open and read data
sst = read.table("dados _SST_anual.txt", header=T, dec=",", sep="")
attach(sst)
names(sst)
sst[,2]      

# Calculate indices from annual mean TSM
summary(sst[,2])

# Plot Boxplot annual mean TSM
png("C:/users/leidinice/desktop/sst_test2.png", width=30,height=15, units="cm", res=600) 

x=(1:length(sst[,2]))
plot(sst[,2]~sst[,1])
plot(x,sst[,2], type="o", lwd=2, main="Annual Mean Sea Surface Temperature (3°S-0°, 34°W-30°W) Period: 1958-2013", xlab="Years", xaxt="n", ylim=c(26,29), ylab="SST (°C)", cex=1)
axis(at=1:56, labels=c(1958:2013), side=1, line=0, cex=1)

# Add linear trend
lines(predict(lm(sst[,2]~x)), lwd=2, col='blue')

dev.off()


