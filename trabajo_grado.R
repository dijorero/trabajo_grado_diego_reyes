#library(readxl)
#datos_IRA <- read_excel("C:/Users/djrey/Desktop/TRABAJO DE GRADO/datos IRA.xlsx")

datos_IRA = as.matrix(datos_IRA)
datos_IRA <- datos_IRA[, -1]
datos_IRA1 = c(datos_IRA[1,], datos_IRA[2,], datos_IRA[3,], datos_IRA[4,], datos_IRA[5,], 
               datos_IRA[6,], datos_IRA[7,], datos_IRA[8,], datos_IRA[9,], datos_IRA[10,], 
               datos_IRA[11,])
datos_IRA1 = as.vector(datos_IRA1)
plot(1:(52*10), datos_IRA1[1:520], lwd = 2, col = "blue",
     type = "l")
for (i in 1:11) {
  abline(v = 52*i, lwd = 2, col = "red")
}

install.packages("epitools")
library(epitools)
x <- seq(as.Date("2010-01-03"), as.Date("2022-07-29"), 
         by="day")
x
semanas_epidemicas = as.week(as.character(x), min.date = "2010-01-03",
                             sunday = TRUE)
semanas_epidemicas$firstday
semanas_epidemicas$week
semanas_epidemicas1 = semanas_epidemicas$cstratum2
datos_IRA1
casos_IRA = datos_IRA1 
año_epidemiologico = c(casos_IRA[1:(5*52)], NA,
                       casos_IRA[(5*52 + 1):(10*52)],
                       rep(NA, (2*52 + 1)),
                       casos_IRA[(10*52 + 1):(10*52 + 30)])

plot(semanas_epidemicas1[1:656], año_epidemiologico[1:656], type = "l", 
     lwd = 2, col = "blue")

serie_arima_ira = ts(año_epidemiologico[1:521])
install.packages("imputeTS")
library(imputeTS)
serie_arima_ira = na_interpolation(serie_arima_ira, "spline")
library(forecast)
ndiffs(serie_arima_ira)

acf(diff(serie_arima_ira)) # q = 5
pacf(diff(serie_arima_ira)) # p = 5
ajuste_arima_ira = Arima(serie_arima_ira, order = c(5, 1, 5))
plot(forecast(ajuste_arima_ira, h = 52*2))

library(TSA)
periodogram(serie_arima_ira, xlab = "Frecuencia", ylab = "Periodograma", 
            las =2, cex.axis = 0.8)
abline(v = 0.02, lwd = 2, col = "purple")
abline(v = 0.04, lwd = 2, col = "purple")
abline(v = 0.075, lwd = 2, col = "purple")
periodo = 1/0.02
periodo

?arima
ajuste_multi = arima(serie_arima_ira, order = c(1, 0, 1), 
                     seasonal = list(order = c(1, 1, 0), period = 50))

plot(ajuste_multi$residuals + serie_arima_ira, type = "l", 
     col = "cyan", lwd = 2)
points(serie_arima_ira, pch = 20)
pronostico_IRA = forecast(ajuste_multi, h = (52*2+1))
plot(pronostico_IRA)

plot(c(serie_arima_ira, data.frame(pronostico_IRA)[,1]), lwd = 2, type = "l")

casos_estimados <- c(serie_arima_ira, data.frame(pronostico_IRA)[,1], 
                     casos_IRA[(10*52 + 1):(10*52 + 30)])
plot(casos_estimados, type = "l", lwd = 2, col = "darkblue")

matriz_1 = matrix(0, ncol = 6, nrow = 6)
for (i in 0:5) {
  for (j in 0:5) {
    matriz_1[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(i, 0, j), 
                                  seasonal = list(order = c(0, 0, 0), 
                                                  period = 50)))}}
min(matriz_1[which(matriz_1 > 0)]) # p = 1, q = 5

matriz_2 = matrix(0, ncol = 6, nrow = 6)
for (i in 0:5) {
  for (j in 0:6) {
    matriz_2[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(i, 1, j), 
                                  seasonal = list(order = c(0, 0, 0), 
                                                  period = 50)))}}
matriz_2
min(matriz_2[which(matriz_2 > 0)]) # p = 5, q = 1

matriz_3 = matrix(0, ncol = 6, nrow = 6)
for (i in 0:5) {
  for (j in 0:5) {
    matriz_3[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(i, 2, j), 
                                  seasonal = list(order = c(0, 0, 0), 
                                                  period = 50)))}}
matriz_3
min(matriz_3[which(matriz_3 > 0)]) # p = 5, q = 2

matriz_3_1 = matrix(0, ncol = 6, nrow = 6)
for (i in 0:5) {
  for (j in 0:5) {
    matriz_3_1[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                    order = c(i, 3, j), 
                                    seasonal = list(order = c(0, 0, 0), 
                                                    period = 50)))}}
min(matriz_3_1[which(matriz_3_1 > 0)])

matriz_4 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_4[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(2, 1, 2), 
                                  seasonal = list(order = c(i, 0, j), 
                                                  period = 50)))}}
matriz_4
min(matriz_4[which(matriz_4 > 0)])

matriz_5 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_5[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(2, 1, 2), 
                                  seasonal = list(order = c(i, 1, j), 
                                                  period = 50)))}}

matriz_5
min(matriz_5[which(matriz_5 > 0)])

matriz_6 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_6[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                  order = c(2, 1, 2), 
                                  seasonal = list(order = c(i, 2, j), 
                                                  period = 50)))}}
matriz_6
min(matriz_6[which(matriz_6 > 0)])

matriz_6_1 = matrix(0, ncol = 4, nrow = 4)
for (i in 0:3) {
  for (j in 0:3) {
    matriz_6_1[i+1,j+1] = AIC(arima(serie_arima_ira, 
                                    order = c(2, 1, 2), 
                                    seasonal = list(order = c(i, 3, j), 
                                                    period = 50)))}}
matriz_6_1
min(matriz_6_1[which(matriz_6_1 > 0)])

ajuste_1 = arima(serie_arima_ira, order = c(3, 2, 3), 
                 seasonal = list(order = c(0, 3, 2),  period = 50))
library(forecast)
pronostico_IRA_1 = forecast(ajuste_1, h = (52*2+1))
plot(pronostico_IRA_1)

plot(c(serie_arima_ira, data.frame(pronostico_IRA_1)[,1]), 
     lwd = 2, col = "purple", type = "l")

casos_estimados_1 <- c(serie_arima_ira, data.frame(pronostico_IRA_1)[,1], 
                       casos_IRA[(10*52 + 1):(10*52 + 30)])

plot(semanas_epidemicas1[1:656], año_epidemiologico[1:656], type = "l", lwd = 2, col = "darkblue")
lines(semanas_epidemicas1[522:626], data.frame(pronostico_IRA_1)[,1], lwd =2, col = "cyan")
