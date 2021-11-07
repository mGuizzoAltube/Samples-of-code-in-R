###================================================###
###           R sample of code (Spanish)           ###
###            Time Series - Final exam            ###
###================================================###

# Author: Matías Güizzo Altube
# Date: August 2021

library(urca)
library(forecast)
library(vars)
library(murphydiagram)
library(dynlm)
library(rugarch)
library(xts)

setwd('~/Maestría en Economía/Series de Tiempo/Examen Final')

###================================================###
###               Pronosticando M1                 ###
###================================================###

# Carga de datos ----
data <- read.csv('basemensual.csv') #importamos los datos
full.sample <- data[13:222,] # Tomamos datos desde 2004 por disponibilidad del EMAE

full.sample$M1 <- ts(full.sample$M1, start=c(2003,1), frequency = 12) 
full.sample$tna <- ts(full.sample$tna, start=c(2003,1), frequency = 12)
full.sample$tcn <- ts(full.sample$tcn, start=c(2003,1), frequency = 12)
full.sample$inflacion <- ts(full.sample$inflacion, start=c(2003,1), frequency = 12)
full.sample$emae <- ts(full.sample$emae, start=c(2003,1), frequency = 12)

#====# Análisis gráfico #====# ------------------------------------------------

plot.ts(full.sample$M1, type='l', lwd=2, xlab='',ylab='M1', bty='n')
title('(a) Serie de M1')
plot.ts(full.sample$inflacion, type = 'l',lwd=2, bty = 'n', xlab = '', ylab = 'Inflación',bty='n')
title('(b) Serie de inflación')
plot.ts(full.sample$emae, type = 'l',lwd=2, bty = 'n', xlab = '', ylab = 'EMAE',bty='n')
title('(c) Serie de EMAE')
plot.ts(full.sample$tna, type = 'l',lwd=2, bty = 'n', xlab = '', ylab = 'TNA',bty='n')
title('(d) Serie de TNA')
plot.ts(full.sample$tcn, type = 'l',lwd=2, bty = 'n', xlab = '', ylab = 'TCN',bty='n')
title('(e) Serie de TCN')


#====# Pruebas de raíz unitaria #====# ------------------------------------------------

### M1
summary(ur.df(log(full.sample$M1), type='trend', selectlags='BIC'))
summary(ur.pp(log(full.sample$M1), type='Z-tau', model='trend', lags = 'short'))
summary(ur.kpss(log(full.sample$M1), type='tau', lags='short'))
summary(ur.df(diff(log(full.sample$M1)), type='drift', selectlags='BIC'))
summary(ur.pp(diff(log(full.sample$M1)), type='Z-tau', model='constant', lags = 'short'))
summary(ur.kpss(diff(log(full.sample$M1)), type='mu', lags='short'))
# Resultado: M1 ~ I(1). Los trest tests coinciden.

### Inlfación
summary(ur.df(full.sample$inflacion, type='trend', selectlags='BIC'))
summary(ur.pp(full.sample$inflacion, type='Z-tau', model='trend', lags = 'long'))
summary(ur.kpss(full.sample$inflacion, type='tau', lags='long'))
summary(ur.df(diff(full.sample$inflacion), type='drift', selectlags='BIC'))
summary(ur.pp(diff(full.sample$inflacion), type='Z-tau', model='constant', lags = 'long'))
summary(ur.kpss(diff(full.sample$inflacion), type='mu', lags='long'))
# Resultado: Inflación ~ I(1)

### EMAE (Estimador Mensual de Actividad Económica)
summary(ur.df(log(full.sample$emae), type='trend', selectlags='BIC'))
summary(ur.pp(log(full.sample$emae), type='Z-tau', model='trend', lags = 'short'))
summary(ur.kpss(log(full.sample$emae), type='tau', lags='short'))
summary(ur.df(diff(log(full.sample$emae)), type='drift', selectlags='BIC'))
summary(ur.pp(diff(log(full.sample$emae)), type='Z-tau', model='constant', lags = 'short'))
summary(ur.kpss(diff(log(full.sample$emae)), type='mu', lags='short'))
# Resultado: EMAE ~ I(1)

### TNA (Tasa Nominal Anual)
summary(ur.df(log(full.sample$tna), type='trend', selectlags='BIC'))
summary(ur.pp(log(full.sample$tna), type='Z-tau', model='trend', lags = 'short'))
summary(ur.kpss(log(full.sample$tna), type='tau', lags='short'))
summary(ur.df(diff(log(full.sample$tna)), type='drift', selectlags='BIC'))
summary(ur.pp(diff(log(full.sample$tna)), type='Z-tau', model='constant', lags = 'short'))
summary(ur.kpss(diff(log(full.sample$tna)), type='mu', lags='short'))
# Resultado: TNA ~ I(1)

### TCN (Tipo de Cambio Nominal)
summary(ur.df(log(full.sample$tcn), type='trend', selectlags='BIC'))
summary(ur.pp(log(full.sample$tcn), type='Z-tau', model='trend', lags = 'short'))
summary(ur.kpss(log(full.sample$tcn), type='tau', lags='short'))
summary(ur.df(diff(log(full.sample$tcn)), type='drift', selectlags='BIC'))
summary(ur.pp(diff(log(full.sample$tcn)), type='Z-tau', model='constant', lags = 'short'))
summary(ur.kpss(diff(log(full.sample$tcn)), type='mu', lags='short'))
summary(ur.df(diff(diff(log(full.sample$tcn))), type='drift', selectlags='BIC'))
summary(ur.pp(diff(diff(log(full.sample$tcn))), type='Z-tau', model='constant', lags = 'short'))
summary(ur.kpss(diff(diff(log(full.sample$tcn))), type='mu', lags='short'))
# Resultado: TCN ~ I(2)

# RESUMEN DE ORDENES DE INTEGRACIÓN
# M1 ~ I(1)
# Inflación ~ I(1)
# EMAE ~ I(1)
# TNA ~ I(1)
# TCN ~ I(2)

data_estacionaria <- data.frame(M1 = c(NA,diff(log(full.sample$M1))),
                                inf = c(NA,diff(full.sample$inflacion)),
                                emae = c(NA,diff(log(full.sample$emae))),
                                tna = c(NA,diff(log(full.sample$tna))),
                                tcn = c(NA,NA,diff(diff(log(full.sample$tcn)))))
data_estacionaria <- ts(data_estacionaria, start = c(2004,1), frequency = 12)

# Usamos la estrategia de 80-20. Total de meses (2004-2021): 2010
# training-sample: 168 observaciones (1/2004 - 12/2018) 
# test-sample: 42 observaciones (1/2019 - 6/2021)

in.sample <- data_estacionaria[1:168,]
in.sample <- ts(in.sample, start = c(2004,1), frequency = 12)
out.sample <- data_estacionaria[169:210,]
out.sample <- ts(out.sample, end = c(2021,6), frequency = 12)


#====# Modelos Uniecuacionales #====# ------------------------------------------------

#===#  . ARIMA   #===# ----
fit.arima <- auto.arima(in.sample[,'M1'])
summary(fit.arima)
# MODELO SARIMA(0,0,1)(0,1,1)[12]
checkresiduals(fit.arima)
Box.test(fit.arima$residuals, lag = 12, type = 'Ljung-Box')
# p-val = 0.4536 (no podemos rechazar la H0 de ruido blanco)

#===#  . ETS   #===# ----
fit.ets <- ets(in.sample[,'M1'])
summary(fit.ets)
# MODELO ETS(A,N,A)
checkresiduals(fit.ets)
Box.test(fit.ets$residuals, lag = 12, type = 'Ljung-Box')
# p-val = 0.0441 (se rechaza al 5% la H0 de ruido blanco)

#===#  . ARIMAX   #===# ----
fit.arimax <- auto.arima(in.sample[,'M1'], xreg = in.sample[,c('inf','emae','tna','tcn')])
summary(fit.arimax)
# MODELO SARIMAX(2,0,0)(1,1,0)[12] con X = (inlación, EMAE, TNA, TCN)
checkresiduals(fit.arimax)
Box.test(fit.arimax$residuals, lag = 12, type = 'Ljung-Box')
# p-val = 0.3433 (no podemos rechazar la H0 de ruido blanco)

#===#  . ADL   #===# ----
fit.adl <- dynlm(M1 ~ lag(M1) + lag(M1,2) + 
                   lag(inf) + lag(inf,2) +
                   lag(emae) + lag(emae,2) +
                   lag(tna) + lag(tna,2) +
                   lag(tcn) + lag(tcn,2), data = in.sample)
summary(fit.adl)
# MODELO ADL con dos lags en cada variable
checkresiduals(fit.adl)
Box.test(fit.adl$residuals, lag = 12, type = 'Ljung-Box')
# p-val = 1.579e-07 (se rechaza al 1% la H0 de ruido blanco)

#====# Modelos Multivariados #====# ------------------------------------------------

#===#  . VAR   #===# ----

# Solo usaremos al EMAE como regresor por su sifnigicatividad en el modelo ADL
# y para mantener los residuos lo más simétricos posible
in.sample.var <- ts(in.sample[2:168,c('M1','emae')], start = c(2004,2), frequency = 12)
VARselect(in.sample.var, lag.max = 18)
fit.var <- VAR(in.sample.var, p = 12, type = 'const', season = 12, ic = 'BIC')
summary(fit.var)
serial.test(fit.var, lags.pt = 12)
checkresiduals(fit.var$varresult$M1)
# Si bien el test de Portmanteau para la no autocorrelación de errores siempre se
# rechaza, el test de Breusch-Godfrey no rechaza la hipótesis de no correlación
# en errores de predicción de M1. Continuamos con el VAR(12) estacional.

# Búsqueda de algún VAR en que no se rechace la H0 del test de Portmanteau
pvals <- matrix(nrow = 12, ncol = 6)
for(v in 1:6){
  cols <- c('M1')
  if(v %in% 1:3){cols <- c(cols, 'emae')}
  if(v %in% 2:4){cols <- c(cols, 'inf')}
  if(v %in% 3:5){cols <- c(cols, 'tna')}
  if(v == 6){cols <- c(cols, 'emae', 'tna')}
  in.sample.var.prueba <- ts(in.sample[2:168,cols], start = c(2004,2), frequency = 12)
    for(r in 1:12){
      fit.var.prueba <- VAR(in.sample.var.prueba, p = r, season = 12, ic = 'BIC')
      summary(fit.var.prueba)
      test.var <- serial.test(fit.var.prueba, lags.pt = r)
      pvals[r,v] <- test.var$serial$p.value
    }
}
pvals # Todas las posibles combinaciones de regresores y lags rechanzan el test de Portamnteau

#===#  . VEC   #===# ----

# Volvemos a niveles. Solo usamos EMAE, al igual que en VAR
in.sample.niveles <- ts(cbind(log(data[13:180,'M1']),data[13:180,'emae']), start = c(2004,1), frequency = 12)
VARselect(in.sample.niveles, lag.max = 18, type = 'const')
# Escogemos 13 lags
fit.var13 <- VAR(in.sample.niveles, p = 13, type = 'const', season = 12, ic = 'BIC')
summary(fit.var13)
serial.test(fit.var13, lags.pt = 18)
normality.test(fit.var13, multivariate.only = TRUE)
# Se rechaza normalidad multivariada al 5% pero se debe solo a problemas de kurtosis
resid1 <- ts(fit.var13$varresult$Series.1$residuals, start=c(2004,1), frequency = 12)
resid2 <- ts(fit.var13$varresult$Series.2$residuals, start=c(2004,1), frequency = 12)
checkresiduals(fit.var13$varresult$Series.1)

# Test de cointegración via Johansen
summary(ca.jo(in.sample.niveles, type = 'trace', ecdet = 'trend', K=13))
summary(ca.jo(in.sample.niveles, type = 'eigen', ecdet = 'trend', K=13))
# En ambas especificaciones se rechaza la hipótesis de ausencua de cointegración al 5% 
# y no se rechaza la hipótesis de rango = 1 (una relación de cointegración)

# Test de tendencia lineal
lttest(ca.jo(in.sample.niveles, type = 'trace', ecdet = 'trend', K=13), r=1)
lttest(ca.jo(in.sample.niveles, type = 'eigen', ecdet = 'trend', K=13), r=1)
# En ambas especificaciones el p-valor = 0.01, por lo que la tendencia es significativa

# Estimación del VEC
cointest <- ca.jo(in.sample.niveles, type = 'eigen', ecdet = 'trend', K=13)
vec <- cajorls(cointest, r=1)
summary(vec$rlm)
# Coeficiente de ajuste no distinto de cero para M1 y cercano a -10 para EMAE
# Esto implica que M1 no ajusta a cambios en la actividad económica, sino que es la
# que tiene efectos sobre la actividad económica (M1 es pushing y EMAE es pulling)
vec2var <- vec2var(cointest, r=1)
checkresiduals(vec2var$resid[,1])


#====# Pronósticos recursivos y rolling #====# ---------------------------------------

#===#  . ARIMA   #===# ----
### RECURSIVO h=1
fcst1.arima <- matrix(0, nrow = 42, ncol = 1)
fcst1.arima <- ts(fcst1.arima, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  fcst1.arima[i] <- forecast::forecast(arima)$mean
}
### RECURSIVO h=6
fcst6.arima <- matrix(0, nrow = 37, ncol = 1) 
fcst6.arima <- ts(fcst6.arima, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  forecast <- forecast::forecast(arima, h=6)$mean
  fcst6.arima[i,] <- forecast[6]
}
# RECURSIVO h=12:
fcst12.arima <- matrix(0, nrow = 31, ncol = 1) 
fcst12.arima <- ts(fcst12.arima, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  forecast <- forecast::forecast(arima, h=12)$mean
  fcst12.arima[i,] <- forecast[12]
}
# EN DIFERENCIAS LOGARITMICAS
plot.ts(out.sample[,'M1'], type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.arima, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.arima, type='l', lwd=2, col = 'salmon')
lines(fcst12.arima, type='l', lwd=2, col = 'salmon4')
title('(a) ARIMA Recursivo - Dif log')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')
# EN NIVELES
M1.niveles <- ts(data$M1[181:222], end = c(2021,6), frequency = 12)
fcst1.arima_level <- exp(fcst1.arima + lag(log(M1.niveles),-1))
fcst6.arima_level <- exp(fcst6.arima + lag(log(M1.niveles),-6))
fcst12.arima_level <- exp(fcst12.arima + lag(log(M1.niveles),-12))

plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.arima_level, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.arima_level, type='l', lwd=2, col = 'salmon')
lines(fcst12.arima_level, type='l', lwd=2, col = 'salmon4')
title('(a) ARIMA Recursivo')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

### ROLLING h=1
fcst1.arima.rol <- matrix(0, nrow = 42, ncol = 1)
fcst1.arima.rol <- ts(fcst1.arima, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  fcst1.arima.rol[i] <- forecast::forecast(arima)$mean
}
### ROLLING h=6
fcst6.arima.rol <- matrix(0, nrow = 37, ncol = 1) 
fcst6.arima.rol <- ts(fcst6.arima, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  forecast <- forecast::forecast(arima, h=6)$mean
  fcst6.arima.rol[i,] <- forecast[6]
}
# ROLLING h=12:
fcst12.arima.rol <- matrix(0, nrow = 31, ncol = 1) 
fcst12.arima.rol <- ts(fcst12.arima, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  arima <- auto.arima(train)
  forecast <- forecast::forecast(arima, h=12)$mean
  fcst12.arima.rol[i,] <- forecast[12]
}
# EN DIFERENCIAS LOGARITMICAS
plot.ts(out.sample[,'M1'], type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.arima.rol, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.arima.rol, type='l', lwd=2, col = 'salmon')
lines(fcst12.arima.rol, type='l', lwd=2, col = 'salmon4')
title('(c) ARIMA Rolling - Dif log')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')
# EN NIVELES
fcst1.arima.rol_level <- exp(fcst1.arima.rol + lag(log(M1.niveles),-1))
fcst6.arima.rol_level <- exp(fcst6.arima.rol + lag(log(M1.niveles),-6))
fcst12.arima.rol_level <- exp(fcst12.arima.rol + lag(log(M1.niveles),-12))

plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.arima.rol_level, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.arima.rol_level, type='l', lwd=2, col = 'salmon')
lines(fcst12.arima.rol_level, type='l', lwd=2, col = 'salmon4')
title('(d) ARIMA Rolling')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

#===#  . ETS   #===# ----
### RECURSIVO h=1
fcst1.ets <- matrix(0, nrow = 42, ncol = 1)
fcst1.ets <- ts(fcst1.ets, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  fcst1.ets[i] <- forecast::forecast(fittd)$mean
}
### RECURSIVO h=6
fcst6.ets <- matrix(0, nrow = 37, ncol = 1) 
fcst6.ets <- ts(fcst6.ets, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  forecast <- forecast::forecast(fittd, h=6)$mean
  fcst6.ets[i,] <- forecast[6]
}
# RECURSIVO h=12:
fcst12.ets <- matrix(0, nrow = 31, ncol = 1) 
fcst12.ets <- ts(fcst12.ets, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(data_estacionaria[,'M1'], end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  forecast <- forecast::forecast(fittd, h=12)$mean
  fcst12.ets[i,] <- forecast[12]
}
# EN DIFERENCIAS LOGARITMICAS
plot.ts(out.sample[,'M1'], type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.ets, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.ets, type='l', lwd=2, col = 'salmon')
lines(fcst12.ets, type='l', lwd=2, col = 'salmon4')
title('(e) ETS Recursivo - Dif log')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')
# EN NIVELES
fcst1.ets_level <- exp(fcst1.ets + lag(log(M1.niveles),-1))
fcst6.ets_level <- exp(fcst6.ets + lag(log(M1.niveles),-6))
fcst12.ets_level <- exp(fcst12.ets + lag(log(M1.niveles),-12))

plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.ets_level, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.ets_level, type='l', lwd=2, col = 'salmon')
lines(fcst12.ets_level, type='l', lwd=2, col = 'salmon4')
title('(b) ETS Recursivo')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

### ROLLING h=1
fcst1.ets.rol <- matrix(0, nrow = 42, ncol = 1)
fcst1.ets.rol <- ts(fcst1.ets.rol, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  fcst1.ets.rol[i] <- forecast::forecast(fittd)$mean
}
### ROLLING h=6
fcst6.ets.rol <- matrix(0, nrow = 37, ncol = 1) 
fcst6.ets.rol <- ts(fcst6.ets.rol, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  forecast <- forecast::forecast(fittd, h=6)$mean
  fcst6.ets.rol[i,] <- forecast[6]
}
# ROLLING h=12:
fcst12.ets.rol <- matrix(0, nrow = 31, ncol = 1) 
fcst12.ets.rol <- ts(fcst12.ets.rol, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(data_estacionaria[,'M1'], start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  fittd <- ets(train)
  forecast <- forecast::forecast(fittd, h=12)$mean
  fcst12.ets.rol[i,] <- forecast[12]
}
# EN DIFERENCIAS LOGARITMICAS
plot.ts(out.sample[,'M1'], type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.ets.rol, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.ets.rol, type='l', lwd=2, col = 'salmon')
lines(fcst12.ets.rol, type='l', lwd=2, col = 'salmon4')
title('(g) ETS Rolling - Dif log')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')
# EN NIVELES
fcst1.ets.rol_level <- exp(fcst1.ets.rol + lag(log(M1.niveles),-1))
fcst6.ets.rol_level <- exp(fcst6.ets.rol + lag(log(M1.niveles),-6))
fcst12.ets.rol_level <- exp(fcst12.ets.rol + lag(log(M1.niveles),-12))

plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.ets.rol_level, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.ets.rol_level, type='l', lwd=2, col = 'salmon')
lines(fcst12.ets.rol_level, type='l', lwd=2, col = 'salmon4')
title('(e) ETS Rolling')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

#===#  . VEC   #===# ----
### RECURSIVO h=1
full.sample.niveles <- ts(cbind(log(data[13:222,'M1']),data[13:222,'emae']), start = c(2004,1), frequency = 12)
fcst1.vec <- matrix(0, nrow = 42, ncol = 1)
fcst1.vec <- ts(fcst1.vec, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(full.sample.niveles, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K = 13)
  vec2var <- vec2var(cointest, r = 1)
  fcst1.vec[i,] <- predict(vec2var, n.ahead = 1)$fcst$Series.1[1]
}
### RECURSIVO h=6
fcst6.vec <- matrix(0, nrow = 37, ncol = 1) 
fcst6.vec <- ts(fcst6.vec, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(full.sample.niveles, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K = 13)
  vec2var <- vec2var(cointest, r = 1)
  fcst6.vec[i,] <- predict(vec2var, n.ahead = 6)$fcst$Series.1[1]
}
### RECURSIVO h=12:
fcst12.vec <- matrix(0, nrow = 31, ncol = 1) 
fcst12.vec <- ts(fcst12.vec, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(full.sample.niveles, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K = 13)
  vec2var <- vec2var(cointest, r = 1)
  fcst12.vec[i,] <- predict(vec2var, n.ahead = 12)$fcst$Series.1[1]
}
# EN NIVELES
plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(exp(fcst1.vec), type='l', lwd=2, col = 'lightsalmon')
lines(exp(fcst6.vec), type='l', lwd=2, col = 'salmon')
lines(exp(fcst12.vec), type='l', lwd=2, col = 'salmon4')
title('(c) VEC Recursivo')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

### ROLLING h=1
fcst1.vec.rol <- matrix(0, nrow = 42, ncol = 1)
fcst1.vec.rol <- ts(fcst1.vec.rol, start=c(2018,1), frequency = 12)
for(i in 1:42){
  train <- window(full.sample.niveles, start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K=13)
  vec2var <- vec2var(cointest, r=1)
  fcst1.vec.rol[i,] <- predict(vec2var, n.ahead = 1)$fcst$Series.1[1]
}
### ROLLING h=6
fcst6.vec.rol <- matrix(0, nrow = 37, ncol = 1) 
fcst6.vec.rol <- ts(fcst6.vec.rol, start=c(2018,6), frequency = 12)
for(i in 1:37){
  train <- window(full.sample.niveles, start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K = 13)
  vec2var <- vec2var(cointest, r = 1)
  fcst6.vec.rol[i,] <- predict(vec2var, n.ahead = 6)$fcst$Series.1[1]
}
### ROLLING h=12:
fcst12.vec.rol <- matrix(0, nrow = 31, ncol = 1) 
fcst12.vec.rol <- ts(fcst12.vec.rol, start=c(2018,12), frequency = 12)
for(i in 1:31){
  train <- window(full.sample.niveles, start = 2004 + (i-1)/12, end = 2017.917 + (i-1)/12)
  cointest <- ca.jo(train, type = "eigen", ecdet = "trend", K = 13)
  vec2var <- vec2var(cointest, r = 1)
  fcst12.vec.rol[i,] <- predict(vec2var, n.ahead = 12)$fcst$Series.1[1]
}
# EN NIVELES
plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(exp(fcst1.vec.rol), type='l', lwd=2, col = 'lightsalmon')
lines(exp(fcst6.vec.rol), type='l', lwd=2, col = 'salmon')
lines(exp(fcst12.vec.rol), type='l', lwd=2, col = 'salmon4')
title('(f) VEC Rolling')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')


#====# Medidas de accuracy #====# ---------------------------------------------

# La serie out of sample es M1.niveles
### h = 1
accuracy(fcst1.arima_level,M1.niveles)
# RMSE 100229855 MAPE 4.09821
accuracy(fcst1.arima.rol_level,M1.niveles)
# RMSE 101748548 MAPE 4.06886
accuracy(fcst1.ets_level,M1.niveles)
# RMSE 87252430 MAPE 3.610325
accuracy(fcst1.ets.rol_level,M1.niveles)
# RMSE 90441493 MAPE 3.686993
accuracy(exp(fcst1.vec),M1.niveles)
# RMSE 124396721 MAPE 4.651902
accuracy(exp(fcst1.vec.rol),M1.niveles)
# RMSE 135008093 MAPE 4.872432

e.fcst1.arima <- M1.niveles-fcst1.arima_level
e.fcst1.arima.rol <- M1.niveles-fcst1.arima.rol_level
e.fcst1.ets <- M1.niveles-fcst1.ets_level
e.fcst1.ets.rol <- M1.niveles-fcst1.ets.rol_level
e.fcst1.vec <- M1.niveles-exp(fcst1.vec)
e.fcst1.vec.rol <- M1.niveles-exp(fcst1.vec.rol)

# Test de Diebold-Mariano
dm.test(e.fcst1.vec, e.fcst1.arima, alternative = "two.sided", power = 2)
# No se rechaza igualdad de desempeño entre ARIMA y VEC
dm.test(e.fcst1.ets, e.fcst1.arima, alternative = "two.sided", power = 2)
# Se rechaza al 5% igualdad de desempeño entre ARIMA y ETS (el ETS tiene menos error)
dm.test(e.fcst1.ets, e.fcst1.vec, alternative = "two.sided", power = 2)
# Se rechaza al 10% igualdad de desempeño entre VEC y ETS (el ETS tiene menos error)

# Test de Giacomini-Rossi
loss.arima <-  e.fcst1.arima^2
loss.ets <-  e.fcst1.ets^2
loss.vec <- e.fcst1.vec^2

fluctuation_test(loss.arima, loss.ets, mu = 0.2)
title('(a) Test de GR ARIMA vs. ETS con h=1')
fluctuation_test(loss.arima, loss.vec[-1], mu = 0.2)
title('(b) Test de GR ARIMA vs. VEC con h=1')
fluctuation_test(loss.vec[-1], loss.ets, mu = 0.2)
title('(c) Test de GR VEC vs. ETS con h=1')

### h = 6
accuracy(fcst6.arima_level,M1.niveles)
# RMSE 100229855 MAPE 4.09821
accuracy(fcst6.arima.rol_level,M1.niveles)
# RMSE 101748548 MAPE 4.06886
accuracy(fcst6.ets_level,M1.niveles)
# RMSE 87252430 MAPE 3.610325
accuracy(fcst6.ets.rol_level,M1.niveles)
# RMSE 90441493 MAPE 3.686993
accuracy(exp(fcst6.vec),M1.niveles)
# RMSE 124396721 MAPE 4.651902
accuracy(exp(fcst6.vec.rol),M1.niveles)
# RMSE 135008093 MAPE 4.872432

e.fcst6.arima <- M1.niveles-fcst6.arima_level
e.fcst6.arima.rol <- M1.niveles-fcst6.arima.rol_level
e.fcst6.ets <- M1.niveles-fcst6.ets_level
e.fcst6.ets.rol <- M1.niveles-fcst6.ets.rol_level
e.fcst6.vec <- M1.niveles-exp(fcst6.vec)
e.fcst6.vec.rol <- M1.niveles-exp(fcst6.vec.rol)

# Test de Diebold-Mariano
loss.arima6 <-  e.fcst6.arima^2
loss.ets6 <-  e.fcst6.ets^2
loss.vec6 <- e.fcst6.vec^2
dl.ets6 <- loss.ets6-loss.arima6
dl.vec6 <- loss.vec6-loss.arima6
NW.COV <- NeweyWest(lm(dl.ets6~1))
coeftest(lm(dl.ets6~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre ARIMA y ETS
NW.COV <- NeweyWest(lm(dl.vec6~1))
coeftest(lm(dl.vec6~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre ARIMA y VEC

# Test de Giacomini-Rossi
fluctuation_test(loss.arima6, loss.ets6, mu = 0.1)
title('(d) Test de GR ARIMA vs. ETS con h=6')
fluctuation_test(loss.arima6, loss.vec6[-1], mu = 0.1)
title('(e) Test de GR ARIMA vs. VEC con h=6')
fluctuation_test(loss.vec6[-1], loss.ets6, mu = 0.1)
title('(f) Test de GR VEC vs. ETS con h=6')

### h = 12
accuracy(fcst12.arima_level,M1.niveles)
# RMSE 930897685 MAE 772225980 MAPE 28.92001
accuracy(fcst12.arima.rol_level,M1.niveles)
# RMSE 932002627 MAE 773000320 MAPE 28.94498
accuracy(fcst12.ets_level,M1.niveles)
# RMSE 936570822 MAE 776468559 MAPE 28.97252
accuracy(fcst12.ets.rol_level,M1.niveles)
# RMSE 932642672 MAE 772929100 MAPE 28.8746
accuracy(exp(fcst12.vec),M1.niveles)
# RMSE 930425418 MAE 760422769 MAPE 28.58542
accuracy(exp(fcst12.vec.rol),M1.niveles)
# RMSE 937642560 MAE 765353916 MAPE 28.7489

e.fcst12.arima <- M1.niveles-fcst12.arima_level
e.fcst12.arima.rol <- M1.niveles-fcst12.arima.rol_level
e.fcst12.ets <- M1.niveles-fcst12.ets_level
e.fcst12.ets.rol <- M1.niveles-fcst12.ets.rol_level
e.fcst12.vec <- M1.niveles-exp(fcst12.vec)
e.fcst12.vec.rol <- M1.niveles-exp(fcst12.vec.rol)

# Test de Diebold-Mariano
loss.arima12 <-  e.fcst12.arima^2
loss.ets12 <-  e.fcst12.ets^2
loss.vec12 <- e.fcst12.vec^2
dl.ets12 <- loss.ets12-loss.arima12
dl.vec12 <- loss.vec12-loss.arima12
NW.COV <- NeweyWest(lm(dl.ets12~1))
coeftest(lm(dl.ets12~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre ARIMA y ETS
NW.COV <- NeweyWest(lm(dl.vec12~1))
coeftest(lm(dl.vec12~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre ARIMA y VEC

# Test de Giacomini-Rossi
fluctuation_test(loss.arima12, loss.ets12, mu = 0.2)
title('(g) Test de GR ARIMA vs. ETS con h=12')
fluctuation_test(loss.arima12, loss.vec12[-1], mu = 0.2)
title('(h) Test de GR ARIMA vs. VEC con h=12')
fluctuation_test(loss.vec12[-1], loss.ets12, mu = 0.2)
title('(i) Test de GR VEC vs. ETS con h=12')

#====# Pooling de modelos #====# ----------------------------------------------

#===#  . Regression-based   #===# ----

# Primero construimos un pronóstico pooling basado regression-based
# ante la posibilidad de sesgos en los métodos de predicción
# h = 1
fcst1.vec_level <- exp(fcst1.vec)
fcst1.vec.rol_level <- exp(fcst1.vec.rol)
pool1.rb <- lm(M1.niveles[-1] ~ fcst1.arima_level + fcst1.vec_level[-1] + fcst1.ets_level)
fcst1.pool.rb <- pool1.rb$fitted.values
e.fcst1.pool.rb <- pool1.rb$residuals
fcst1.pool.rb <- ts(fcst1.pool.rb, end = c(2021, 6), frequency = 12)
pool1.rb$coefficients
#        (Intercept)   fcst1.arima_level fcst1.vec_level[-1]     fcst1.ets_level 
#       3.089432e+07       -4.537306e-01        1.795126e-01        1.265084e+00 

# h = 6
fcst6.vec_level <- exp(fcst6.vec)
fcst6.vec.rol_level <- exp(fcst6.vec.rol)
pool6.rb <- lm(M1.niveles[-1:-6] ~ fcst6.arima_level + fcst6.vec_level[-1] + fcst6.ets_level)
fcst6.pool.rb <- pool6.rb$fitted.values
e.fcst6.pool.rb <- pool6.rb$residuals
fcst6.pool.rb <- ts(fcst6.pool.rb, end = c(2021, 6), frequency = 12)
pool6.rb$coefficients
#        (Intercept)   fcst6.arima_level fcst6.vec_level[-1]     fcst6.ets_level 
#       3.715103e+07       -1.517813e+00        1.877434e-01        2.497599e+00 

# h = 12
fcst12.vec_level <- exp(fcst12.vec)
fcst12.vec.rol_level <- exp(fcst12.vec.rol)
pool12.rb <- lm(M1.niveles[-1:-12] ~ fcst12.arima_level + fcst12.vec_level[-1] + fcst12.ets_level)
fcst12.pool.rb <- pool12.rb$fitted.values
e.fcst12.pool.rb <- pool12.rb$residuals
fcst12.pool.rb <- ts(fcst12.pool.rb, end = c(2021, 6), frequency = 12)
pool12.rb$coefficients
#         (Intercept)   fcst12.arima_level fcst12.vec_level[-1]     fcst12.ets_level 
#       -1.572433e+08        -1.598677e+00         1.452633e-01         3.045750e+00 

plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.pool.rb, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.pool.rb, type='l', lwd=2, col = 'salmon')
lines(fcst12.pool.rb, type='l', lwd=2, col = 'salmon4')
title('(a) Regression-based pooled forecast')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

# Accuracy
accuracy(fcst1.pool.rb,M1.niveles)
# RMSE 82009918 MAE 63924568 MAPE 3.56509
accuracy(fcst6.pool.rb,M1.niveles)
# RMSE 271641243 MAE 225017534 MAPE 11.58751
accuracy(fcst12.pool.rb,M1.niveles)
# RMSE 459137560 MAE 420838637 MAPE 20.39702

# Diebold-Mariano contra ETS
dm.test(e.fcst1.pool.rb, e.fcst1.ets, alternative = "two.sided", power = 2)
# No se rechaza H0 de igualdad de desempeño
loss.pool.rb <- e.fcst1.pool.rb^2
loss.pool6.rb <- e.fcst6.pool.rb^2
dl.pool6.rb <- loss.pool6.rb-loss.ets6
loss.pool12.rb <- e.fcst12.pool.rb^2
dl.pool12.rb <- loss.pool12.rb-loss.ets12

NW.COV <- NeweyWest(lm(dl.pool6.rb~1))
coeftest(lm(dl.pool6.rb~1), vcov. = NW.COV)
# Se rechaza al 10% la hipótesis de higualdad de desempeño entre Pool y ETS
NW.COV <- NeweyWest(lm(dl.pool12.rb~1))
coeftest(lm(dl.pool12.rb~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre Pool y ETS

# Test de Giacomini-Rossi
fluctuation_test(loss.pool.rb, loss.ets, mu = 0.1)
title('(a) Test de GR Pool Reg-based vs. ETS con h=1')
fluctuation_test(loss.pool6.rb, loss.ets6, mu = 0.1)
title('(b) Test de GR Pool Reg-based vs. ETS con h=6')
fluctuation_test(loss.pool12.rb, loss.ets12, mu = 0.1)
title('(c) Test de GR Pool Reg-based vs. ETS con h=12')


#===#  . Ponderación por RMSE   #===# ----

# Recuperamos el RMSE de cada modelo y construimos una ponderación
# h = 1
RMSE.arima1 <- accuracy(fcst1.arima_level,M1.niveles)[,'RMSE']
RMSE.ets1 <- accuracy(fcst1.ets_level,M1.niveles)[,'RMSE']
RMSE.vec1 <- accuracy(fcst1.vec_level,M1.niveles)[,'RMSE']
# Elección de nu para minimizar el RMSE del ponderado
RMSE <- vector(length = 25)
v <- seq(0.01,0.25,0.01)
for(i in 1:25){
  w_arima <- RMSE.arima1^(-v[i])/(RMSE.arima1^(-v[i])+RMSE.ets1^(-v[i])+RMSE.vec1^(-v[i]))
  w_ets <- RMSE.ets1^(-v[i])/(RMSE.arima1^(-v[i])+RMSE.ets1^(-v[i])+RMSE.vec1^(-v[i]))
  w_vec <- RMSE.vec1^(-v[i])/(RMSE.arima1^(-v[i])+RMSE.ets1^(-v[i])+RMSE.vec1^(-v[i]))
  fcst1.pool.rmse <- w_arima*fcst1.arima_level + w_ets*fcst1.ets_level + w_arima*fcst1.vec_level
  RMSE[i] <- accuracy(fcst1.pool.rmse, M1.niveles)[,'RMSE']
}
plot(v, RMSE, type='l', lwd=2, xlab= expression(nu),ylab='RMSE del pool', bty='n')
title(expression('(a) Elección de' ~ nu ~ 'para h=1'))
v_1 <- v[RMSE == min(RMSE)]
w_arima1 <- RMSE.arima1^(-v_1)/(RMSE.arima1^(-v_1)+RMSE.ets1^(-v_1)+RMSE.vec1^(-v_1))
w_ets1 <- RMSE.ets1^(-v_1)/(RMSE.arima1^(-v_1)+RMSE.ets1^(-v_1)+RMSE.vec1^(-v_1))
w_vec1 <- RMSE.vec1^(-v_1)/(RMSE.arima1^(-v_1)+RMSE.ets1^(-v_1)+RMSE.vec1^(-v_1))
fcst1.pool.rmse <- w_arima1*fcst1.arima_level + w_ets1*fcst1.ets_level + w_arima1*fcst1.vec_level
e.fcst1.pool.rmse <- M1.niveles[-1] - fcst1.pool.rmse

# h = 6
RMSE.arima6 <- accuracy(fcst6.arima_level,M1.niveles)[,'RMSE']
RMSE.ets6 <- accuracy(fcst6.ets_level,M1.niveles)[,'RMSE']
RMSE.vec6 <- accuracy(fcst6.vec_level,M1.niveles)[,'RMSE']
# Elección de nu para minimizar el RMSE del ponderado
RMSE <- vector(length = 25)
v <- seq(0.01,0.25,0.01)
for(i in 1:25){
  w_arima <- RMSE.arima6^(-v[i])/(RMSE.arima6^(-v[i])+RMSE.ets6^(-v[i])+RMSE.vec6^(-v[i]))
  w_ets <- RMSE.ets6^(-v[i])/(RMSE.arima6^(-v[i])+RMSE.ets6^(-v[i])+RMSE.vec6^(-v[i]))
  w_vec <- RMSE.vec6^(-v[i])/(RMSE.arima6^(-v[i])+RMSE.ets6^(-v[i])+RMSE.vec6^(-v[i]))
  fcst6.pool.rmse <- w_arima*fcst6.arima_level + w_ets*fcst6.ets_level + w_arima*fcst6.vec_level
  RMSE[i] <- accuracy(fcst6.pool.rmse, M1.niveles)[,'RMSE']
}
plot(v, RMSE, type='l', lwd=2, xlab= expression(nu),ylab='RMSE del pool', bty='n')
title(expression('(b) Elección de' ~ nu ~ 'para h=6'))
v_6 <- v[RMSE == min(RMSE)]
w_arima6 <- RMSE.arima6^(-v_6)/(RMSE.arima6^(-v_6)+RMSE.ets6^(-v_6)+RMSE.vec6^(-v_6))
w_ets6 <- RMSE.ets6^(-v_6)/(RMSE.arima6^(-v_6)+RMSE.ets6^(-v_6)+RMSE.vec6^(-v_6))
w_vec6 <- RMSE.vec6^(-v_6)/(RMSE.arima6^(-v_6)+RMSE.ets6^(-v_6)+RMSE.vec6^(-v_6))
fcst6.pool.rmse <- w_arima6*fcst6.arima_level + w_ets6*fcst6.ets_level + w_arima6*fcst6.vec_level
e.fcst6.pool.rmse <- M1.niveles[-1:-6] - fcst6.pool.rmse

# h = 12
RMSE.arima12 <- accuracy(fcst12.arima_level,M1.niveles)[,'RMSE']
RMSE.ets12 <- accuracy(fcst12.ets_level,M1.niveles)[,'RMSE']
RMSE.vec12 <- accuracy(fcst12.vec_level,M1.niveles)[,'RMSE']
# Elección de nu para minimizar el RMSE del ponderado
RMSE <- vector(length = 25)
v <- seq(0.01,0.25,0.01)
for(i in 1:25){
  w_arima <- RMSE.arima12^(-v[i])/(RMSE.arima12^(-v[i])+RMSE.ets12^(-v[i])+RMSE.vec12^(-v[i]))
  w_ets <- RMSE.ets12^(-v[i])/(RMSE.arima12^(-v[i])+RMSE.ets12^(-v[i])+RMSE.vec12^(-v[i]))
  w_vec <- RMSE.vec12^(-v[i])/(RMSE.arima12^(-v[i])+RMSE.ets12^(-v[i])+RMSE.vec12^(-v[i]))
  fcst12.pool.rmse <- w_arima*fcst12.arima_level + w_ets*fcst12.ets_level + w_arima*fcst12.vec_level
  RMSE[i] <- accuracy(fcst12.pool.rmse, M1.niveles)[,'RMSE']
}
plot(v, RMSE, type='l', lwd=2, xlab= expression(nu),ylab='RMSE del pool', bty='n')
title(expression('(c) Elección de' ~ nu ~ 'para h=12'))
v_12 <- v[RMSE == min(RMSE)]
w_arima12 <- RMSE.arima12^(-v_12)/(RMSE.arima12^(-v_12)+RMSE.ets12^(-v_12)+RMSE.vec12^(-v_12))
w_ets12 <- RMSE.ets12^(-v_12)/(RMSE.arima12^(-v_12)+RMSE.ets12^(-v_12)+RMSE.vec12^(-v_12))
w_vec12 <- RMSE.vec12^(-v_12)/(RMSE.arima12^(-v_12)+RMSE.ets12^(-v_12)+RMSE.vec12^(-v_12))
fcst12.pool.rmse <- w_arima12*fcst12.arima_level + w_ets12*fcst12.ets_level + w_arima12*fcst12.vec_level
e.fcst12.pool.rmse <- M1.niveles[-1:-12] - fcst12.pool.rmse


plot.ts(M1.niveles, type='l', lwd=2, xlab='',ylab='M1', bty='n')
lines(fcst1.pool.rmse, type='l', lwd=2, col = 'lightsalmon')
lines(fcst6.pool.rmse, type='l', lwd=2, col = 'salmon')
lines(fcst12.pool.rmse, type='l', lwd=2, col = 'salmon4')
title('(b) RMSE weighted pooled forecast')
legend('topleft', c('Serie M1','h = 1', 'h = 6', 'h = 12'), col = c('black', 'lightsalmon','salmon','salmon4'),
       cex = 0.8, lty = 1, lwd = 2, bty = 'n')

# Accuracy
accuracy(fcst1.pool.rmse,M1.niveles)
# RMSE 90330605 MAE 71486933 MAPE 3.803161
accuracy(fcst6.pool.rmse,M1.niveles)
# RMSE 446073656 MAE 349771929 MAPE 14.63605
accuracy(fcst12.pool.rmse,M1.niveles)
# RMSE 934316922 MAE 773886747 MAPE 28.85819

# Diebold-Mariano contra ETS
dm.test(e.fcst1.pool.rmse, e.fcst1.ets, alternative = "two.sided", power = 2)
# No se rechaza H0 de igualdad de desempeño
loss.pool.rmse <- e.fcst1.pool.rmse^2
loss.pool6.rmse <- e.fcst6.pool.rmse^2
dl.pool6.rmse <- loss.pool6.rmse-loss.ets6
loss.pool12.rmse <- e.fcst12.pool.rmse^2
dl.pool12.rmse <- loss.pool12.rmse-loss.ets12

NW.COV <- NeweyWest(lm(dl.pool6.rmse~1))
coeftest(lm(dl.pool6.rmse~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre Pool y ETS
NW.COV <- NeweyWest(lm(dl.pool12.rmse~1))
coeftest(lm(dl.pool12.rmse~1), vcov. = NW.COV)
# No se rechaza igualdad de desempeño entre Pool y ETS

# Test de Giacomini-Rossi
fluctuation_test(loss.pool.rmse, loss.ets, mu = 0.1)
title('(d) Test de GR Pool RMSEw vs. ETS con h=1')
fluctuation_test(loss.pool6.rmse, loss.ets6, mu = 0.1)
title('(e) Test de GR Pool RMSEw vs. ETS con h=6')
fluctuation_test(loss.pool12.rmse, loss.ets12, mu = 0.1)
title('(f) Test de GR Pool RMSEw vs. ETS con h=12')
