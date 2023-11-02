install.packages("tidyverse")
install.packages("vars")

library(readxl)
library(epitools)
library(imputeTS)
library(forecast)
library(TSA)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(vars)
library(aTSA)



datos_IRA <- read_excel("F:\\TG\\datos IRA.xlsx")
datos_IRA = as.matrix(datos_IRA)
datos_IRA <- datos_IRA[, -1]

#cantidad de datos del 2010 y el 2011 = 52*2
#cantidad de datos del 2010,..., y 2019 = 52*10
#cantidad de datos desde 2020 hasta el 2021 = 52*2
#cantidad de datos desde 2020 hasta el 2022 = 52*3
#cantidad de datos desde 2010 hasta el 2022 = 52*13 = 676 datos


### GRAFICAR LA SERIE DE TIEMPO CON LOS DATOS IMPUTADOS EN EL 2020 Y 2021 USANDO GGPLOT

semana = c(1:572)
infectados = c(datos_IRA[1,], datos_IRA[2,], datos_IRA[3,], datos_IRA[4,], datos_IRA[5,], 
               datos_IRA[6,], datos_IRA[7,], datos_IRA[8,], datos_IRA[9,], datos_IRA[10,], 
               datos_IRA[11,]) #YA NO MIRE MAS ESTA!

length(infectados)
datos_IRA1df = data.frame(semana,infectados)
ggplot(datos_IRA1df, aes(x=semana, y=infectados)) + geom_line()

#install.packages("epitools")
# library(epitools)
#x <- seq(as.Date("2010-01-03"), as.Date("2023-01-01"), 
#         by="day")

#semanas_epidemicas = as.week(as.character(x), min.date = "2010-01-03",
#                             sunday = TRUE)
casos_IRA = infectados 
año_epidemiologico = c(casos_IRA[1:(5*52)], NA,
                       casos_IRA[(5*52 + 1):(10*52)],
                       rep(NA, (2*52 + 1)),
                       casos_IRA[(10*52 + 1):(10*52 + 30)],rep(NA, (24))) #676 datos con el arreglo final rep(NA, (20)

#año_epidemiologico[627:656] desde la 627 empiezan los datos del 2022 hasta 656 terminan los 30 datos

length(año_epidemiologico) #680
#semanas_epidemicas1 = semanas_epidemicas$cstratum2
#length(semanas_epidemicas1) #680

#st = data.frame(semanas_epidemicas1[1:680],año_epidemiologico[1:680])
st = data.frame(c(1:680),año_epidemiologico[1:680])
colnames(st) = c("semana","infectados")
ggplot(st, aes(x=semana , y=infectados)) + geom_line()


serie_arima_ira = ts(año_epidemiologico[1:521])
#install.packages("imputeTS")
# library(imputeTS)
serie_arima_ira = na_interpolation(serie_arima_ira, "spline")
# library(forecast)


#Prueba de Dickey-Fuller
adf.test(serie_arima_ira)


ndiffs(serie_arima_ira) # d=1
acf(diff(serie_arima_ira)) # q = 5
pacf(diff(serie_arima_ira)) # p = 5

ajuste_arima_ira = Arima(serie_arima_ira, order = c(5, 1, 5))
prediccion = data.frame(forecast(ajuste_arima_ira, h = 52*3))[,1]
length(prediccion)#156 
length(st$semana)#680
length(st$semana[520:680]) #161



df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2 = data.frame(semana = st$semana[520:624], infectados = prediccion[1:105] , isin = "Imputación 2020 y 2021")
df3 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion[105:161] , isin = "Predicción 2022")
df = rbind(df1, df2, df3)

## GRAFICA DE LA IMPUTACIÓN Y DE LA PREDICCIÓN CON EL MODELO ARIMA(5,1,5) NO ESTACIONAL

ggplot(df, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000')) 


library(TSA)
periodogram(serie_arima_ira, xlab = "Frecuencia", ylab = "Periodograma", 
            las =2, cex.axis = 0.8)
abline(v = 0.02, lwd = 2, col = "#00FF00")
periodo = 1/0.02
periodo

### ESCOGENCIA DE LOS MODELOS ARIMA MULTIPLICATIVOS Y SU AIC


###-----------------------------------------------------------------------------------
##ARIMA515_131
#ARIMA515_131_2 = Arima(serie_arima_ira, order = c(5, 1, 5), seasonal = list(order = c(1, 3, 1), period = 50))
#AIC(ARIMA515_131_2)
load("F:\\TG\\ARIMA515_131_2.rda")
prediccion_131 = data.frame(forecast(ARIMA515_131_2, h = 52*3))[,1]
#upper<-prediccion_131+2*sd(prediccion_131)
#lower<-prediccion_131-2*sd(prediccion_131)
df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2_131 = data.frame(semana = st$semana[520:624], infectados = prediccion_131[1:105] , isin = "Imputación 2020 y 2021")
df3_131 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion_131[105:161] , isin = "Predicción 2022")
#h = data.frame(semana = st$semana[(52*12):680], infectados = upper[105:161] ,isin = "upper")
#l = data.frame(semana = st$semana[(52*12):680], infectados = lower[105:161], isin = "lower")
df_131 = rbind(df1, df2_131, df3_131)
ggplot(df_131, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000'))
#save(ARIMA515_131_2, file = "D:\\TG\\ARIMA515_131_2.rda")

dif_131 = casos_IRA[(10*52 + 1):(10*52 + 30)]-prediccion_131[1:30]
cuad_131 = (dif_131)^2
sum_131 = sum(cuad_131)
MSE_131=(1/30)*(sum_131)
MSE_131


##Grafico con la banda de confianza. Se ve mal
#ggplot(df_131, aes(x = semana, y = infectados)) + geom_line() + geom_smooth(aes(x=semana, y=infectados, ymax=upper[105:161], ymin=lower[105:161]), colour='red', data=df3_131, stat='identity')

###-----------------------------------------------------------------------------------

##ARIMA515_231
#ARIMA515_231 = Arima(serie_arima_ira, order = c(5, 1, 5), seasonal = list(order = c(2, 3, 1), period = 50))
#AIC(ARIMA515_231)
load("F:\\TG\\ARIMA515_231.rda")
prediccion_231 = data.frame(forecast(ARIMA515_231, h = 52*3))[,1]
df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2_231 = data.frame(semana = st$semana[520:624], infectados = prediccion_231[1:105] , isin = "Imputación 2020 y 2021")
df3_231 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion_231[105:161] , isin = "Predicción 2022")
df_231 = rbind(df1, df2_231, df3_231)
ggplot(df_231, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000'))
#save(ARIMA515_231, file = "D:\\TG\\ARIMA515_231.rda")

dif_231 = casos_IRA[(10*52 + 1):(10*52 + 30)]-prediccion_231[1:30]
cuad_231 = (dif_231)^2
sum_231 = sum(cuad_231)
MSE_231=(1/30)*(sum_231)
MSE_231

###-----------------------------------------------------------------------------------
##ARIMA515_222
#ARIMA515_222 = Arima(serie_arima_ira, order = c(5, 1, 5), seasonal = list(order = c(2, 2, 2), period = 50))
#AIC(ARIMA515_222)
load("F:\\TG\\ARIMA515_222.rda")
prediccion_222 = data.frame(forecast(ARIMA515_222, h = 52*3))[,1]
df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2_222 = data.frame(semana = st$semana[520:624], infectados = prediccion_222[1:105] , isin = "Imputación 2020 y 2021")
df3_222 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion_222[105:161] , isin = "Predicción 2022")
df_222 = rbind(df1, df2_222, df3_222)
ggplot(df_222, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000'))
#save(ARIMA515_222, file = "D:\\TG\\ARIMA515_222.rda")

dif_222 = casos_IRA[(10*52 + 1):(10*52 + 30)]-prediccion_222[1:30]
cuad_222 = (dif_222)^2
sum_222 = sum(cuad_222)
MSE_222=(1/30)*(sum_222)
MSE_222



###-----------------------------------------------------------------------------------
##ARIMA515_424
#ARIMA515_424 = Arima(serie_arima_ira, order = c(5, 1, 5), seasonal = list(order = c(4, 2, 4), period = 50))
#AIC(ARIMA515_424)
load("F:\\TG\\ARIMA515_424.rda")
prediccion_424 = data.frame(forecast(ARIMA515_424, h = 52*3))[,1]
df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2_424 = data.frame(semana = st$semana[520:624], infectados = prediccion_424[1:105] , isin = "Imputación 2020 y 2021")
df3_424 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion_424[105:161] , isin = "Predicción 2022")
df_424 = rbind(df1, df2_424, df3_424)
ggplot(df_424, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000'))
#save(ARIMA515_424, file = "D:\\TG\\ARIMA515_424.rda")

dif_424 = casos_IRA[(10*52 + 1):(10*52 + 30)]-prediccion_424[1:30]
cuad_424 = (dif_424)^2
sum_424 = sum(cuad_424)
MSE_424=(1/30)*(sum_424)
MSE_424
###-----------------------------------------------------------------------------------
##ARIMA515_234
#ARIMA515_234 = Arima(serie_arima_ira, order = c(5, 1, 5), seasonal = list(order = c(2, 3, 4), period = 50))
#AIC(ARIMA515_234)
load("F:\\TG\\ARIMA515_234.rda")
prediccion_234 = data.frame(forecast(ARIMA515_234, h = 52*3))[,1]
df1 = data.frame(semana = st$semana[1:520], infectados = st$infectados[1:520], isin = "Observaciones")
df2_234 = data.frame(semana = st$semana[520:624], infectados = prediccion_234[1:105] , isin = "Imputación 2020 y 2021")
df3_234 = data.frame(semana = st$semana[(52*12):680], infectados = prediccion_234[105:161] , isin = "Predicción 2022")
df_234 = rbind(df1, df2_234, df3_234)
ggplot(df_234, aes(x = semana, y = infectados, color = isin)) + geom_line() + geom_vline(xintercept = (52*10), linetype = 3, color = 1) +   geom_vline(xintercept = (52*12),linetype = 3, color = 1)+   geom_vline(xintercept = (52*13),linetype = 3, color = 1) + scale_colour_manual(values=c(Observaciones='black', "Imputación 2020 y 2021"='#4F94CD', "Predicción 2022"='#EE0000'))
#save(ARIMA515_234, file = "D:\\TG\\ARIMA515_234.rda")

dif_234 = casos_IRA[(10*52 + 1):(10*52 + 30)]-prediccion_234[1:30]
cuad_234 = (dif_234)^2
sum_234 = sum(cuad_234)
MSE_234=(1/30)*(sum_234)
MSE_234
###---------------------------------------------------------
MSE_min = min(MSE_131,MSE_222,MSE_231,MSE_234,MSE_424)
MSE_min
