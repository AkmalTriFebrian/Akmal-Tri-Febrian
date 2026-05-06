library(tseries)
library(TSA)
library(lmtest)
library(forecast)
library(MLmetrics)
library(readxl)
library(tsoutliers)

data1 = read_excel("C:/Users/akmal/Downloads/IRSX.xlsx")
head(data1)

Tanggal = as.Date(data1$Tanggal)
Harga = as.numeric(gsub(",", ".", data1$`Harga Saham Penutup`))
data.saham = data.frame(Tanggal, Harga)
head(data.saham)
plot(Tanggal, Harga, type = "l", col = "blue", lwd = 2,
     main = "Pergerakan IHSG")

summary(data.saham)
abline(v = Tanggal[304], col = "red", lty = 3, lwd = 1.5)

Harga.ujiragam = Harga - min(Harga)+1
BoxCox.lambda(Harga.ujiragam)
adf.test(Harga, k=1)
df_diff = diff(data.saham$Harga)
adf.test(df_diff)

acf(df_diff)
pacf(df_diff)

pola_fixed1 = c(0, NA,rep(0, 7),NA, rep(0, 8),NA)            
arima1 = arima(Harga, 
                 order = c(19, 1, 0), 
                 fixed = pola_fixed1, 
                 method = 'ML',
                include.mean = TRUE)
coeftest(arima1)

pola_fixed2 = c(0, NA,rep(0, 4),NA, 0, 0,NA,rep(0, 8),NA)            
arima2 = arima(Harga, 
                 order = c(0, 1, 19), 
                 fixed = pola_fixed2, 
                 method = 'ML',
                include.mean = TRUE)
coeftest(arima2)

pola_fixed3 = c(rep(0,6),NA, 0, 0,NA,rep(0, 8),NA)            
arima3 = arima(Harga, 
                 order = c(0, 1, 19), 
                 fixed = pola_fixed3, 
                 method = 'ML',
                include.mean = TRUE)
coeftest(arima3)

resarima1 = residuals(arima1)
resarima3 = residuals(arima3)
Box.test(resarima1, lag=2, type = c('Ljung-Box'))
Box.test(resarima1, lag=6, type = c('Ljung-Box'))
Box.test(resarima1, lag=20, type = c('Ljung-Box'))
Box.test(resarima3, lag=2, type = c('Ljung-Box'))
Box.test(resarima3, lag=6, type = c('Ljung-Box'))
Box.test(resarima3, lag=20, type = c('Ljung-Box'))

jarque.bera.test(resarima1)
jarque.bera.test(resarima3)

arima1$aic
arima3$aic

pred.arima = Harga-resarima1
outlier = tso(pred.arima, types = c('AO','IO','IS','TC'))
print(outlier)

plot(outlier$effects)

xr.outlier = outlier$effects

pola_fixedd = c(0, NA,rep(0, 7),NA, rep(0, 8),NA,NA)            
arimaout = arima(Harga, 
                order = c(19, 1, 0), 
                fixed = pola_fixedd,
                xreg=xr.outlier,
                method = 'ML',
                include.mean = TRUE)
coeftest(arimaout)


resarimaout = residuals(arimaout)

Box.test(resarimaout, lag=2, type = c('Ljung-Box'))
Box.test(resarimaout, lag=6, type = c('Ljung-Box'))
Box.test(resarimaout, lag=20, type = c('Ljung-Box'))

jarque.bera.test(resarimaout)

pred.arimaout = Harga-resarimaout
plot(Tanggal, Harga, type = "l", col = "blue", lwd = 2,main = "Pergerakan IHSG")
lines(Tanggal, pred.arima, type = "o", col = "red", lwd = 2)
lines(Tanggal, pred.arimatc, type = "o", col = "green", lwd = 2)

MAE(Harga, pred.arima)
MAPE(Harga, pred.arima)
RMSE(Harga, pred.arima)

MAE(Harga, pred.arimaout)
MAPE(Harga, pred.arimaout)
RMSE(Harga, pred.arimaout)

Harga.pra.intervensi = Harga[1:303]
Harga_plus1 = Harga.pra.intervensi - min(Harga.pra.intervensi) + 1
BoxCox.lambda(Harga_plus1)
adf.test(Harga.pra.intervensi, k=1)
df_diffe = diff(Harga.pra.intervensi)
adf.test(df_diffe)
acf(df_diffe)
pacf(df_diffe)

fixid = c(rep(0, 9),NA)            
arima.printer1 = arima(Harga.pra.intervensi, 
                 order = c(10, 1, 0), 
                 fixed = fixid,
                 method = 'ML',
                 include.mean = TRUE)
coeftest(arima.printer1)

fixid2 = c(rep(0, 9),NA)            
arima.printer2 = arima(Harga.pra.intervensi, 
                        order = c(0, 1, 10), 
                        fixed = fixid2,
                        method = 'ML',
                        include.mean = TRUE)
coeftest(arima.printer2)

resarima.printer = residuals(arima.printer1)
resarima.printer2 = residuals(arima.printer2)
Box.test(resarima.printer, lag=2, type = c('Ljung-Box'))
Box.test(resarima.printer, lag=6, type = c('Ljung-Box'))
Box.test(resarima.printer, lag=11, type = c('Ljung-Box'))
Box.test(resarima.printer2, lag=2, type = c('Ljung-Box'))
Box.test(resarima.printer2, lag=6, type = c('Ljung-Box'))
Box.test(resarima.printer2, lag=11, type = c('Ljung-Box'))
jarque.bera.test(resarima.printer)
jarque.bera.test(resarima.printer2)


arima.printer1$aic
arima.printer2$aic

peramalan = forecast(Harga.pra.intervensi, model = arima.printer2, h=10)
peramalan$mean
sisaan.ramalan = Harga[304:311] - peramalan$mean

errorintv = rep(0,311)
errorintv[1:303] = resarima1
errorintv[304:311] = sisaan.ramalan

plot(errorintv, type="h", xlab="Waktu (T)", ylab = "Residual", xaxt = "n")
abline(h=c(-3*sd(resarima.printer2), 3*sd(resarima.printer2)), col="blue", lty=2)
abline(v = 304, col = "red", lty = 3, lwd = 1.5)
n=311
AO1 = rep(0, n)
AO2 = rep(0, n)
TC  = rep(0, n)

AO1[292:n] = 1
AO2[311:n] = 1
TC[151:n]  = 1
interven = cbind(AO1, AO2, TC)
interven = as.matrix(interven)
model.intervensi = arimax(Harga, order = c(0, 1, 10), 
                  fixed = c(rep(0, 9),NA, NA,NA,NA),xreg = interven,
                  method = 'ML')
model.intervensi
coef(model.intervensi)

resinterven = residuals(model.intervensi)
jarque.bera.test(resinterven)

Box.test(resinterven, lag = 2, type = "Ljung-Box")
Box.test(resinterven, lag = 6, type = "Ljung-Box")
Box.test(resinterven, lag = 11, type = "Ljung-Box")

prediksi.arima.interven = Harga-resinterven
MAE(Harga, prediksi.arima.interven)
MAPE(Harga, prediksi.arima.interven)
RMSE(Harga, prediksi.arima.interven)
plot(Tanggal, Harga, type = "l", col = "blue", lwd = 2)
lines(Tanggal, prediksi.arima.interven, type = "l", col = "red", lwd = 2)
legend("topright",
       legend = c("Aktual", "Prediksi"),
       col = c("blue", "red"),
       lty = 1,
       cex = 0.5)
