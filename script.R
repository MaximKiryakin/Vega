library(forecast)
library(tseries)
library(openxlsx)
library(ggplot2)
library(quantmod)
library(rusquant)
library(rugarch)
library(rmgarch)
library(HARModel)

install.packages('HARModel')
  


# plot(ts(rollapply(eps, width=50, by=1, function(x) var(x))))

# gazp <- getSymbols.Moex("GAZP")

# dd <- ts(exp(diff(log(gazp$close))))
# plot(dd, ylab="", xlab="")
# abline(h=0, col="dark grey", lty=3, lwd=3)

# Загружаем пакет tidyverse, если он еще не установлен



# Читаем CSV-файл
data <- read.csv("sber_data (4).csv")
data$DateTime <- as.POSIXct(data$DateTime)
data_xts <- xts(data$returns, order.by = data$DateTime)


data_xts["/2020-03"]


# Построение графика
plot(data_xts, main = "График доходности SBER", ylab = "Доходность")

plot(ts(data$returns))



#data <- getSymbols.Moex("GAZP")
#dd <- data$X.CLOSE.


# Просматриваем первые несколько строк загруженных данных
head(data)

#dd<-ts(exp(diff(log(dd))))

# sGARCH
# Adjusted Pearson Goodness-of-Fit Test: этот тест показал ноль - модель херня
s <- ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1,1)), 
                    mean.model = list(armaOrder=c(0, 0)), 
                distribution.model = 'norm')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")


f <- ugarchforecast(fitORspec = m, n.ahead = 250)
# сонстанта, так как мы заложили это в модели
plot(fitted(f))
# модель ожидает, что волатильность будет расти
plot(sigma(f))

# имея sigma(f) например можно получать вес активка в портфеле 
# в зависимости от его волатильности sqrt(252) * sigma(m)
# вес 0.05 делить на результат где 0.05 волатильность которую мы готовы принять



# sGARCH with snorm
# Adjusted Pearson Goodness-of-Fit Test: этот тест показал ноль - модель херня
s <- ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(0, 0)), 
                distribution.model = 'snorm')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")


# GARCH with sstd
# стали более нормальные хвосты и вырос информационный критерий
s <- ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(0, 0)), 
                distribution.model = 'sstd')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")


# GJR-GARCH
# модель показывает, что негативные новости влияют на прогноз 
# более существенно, чем позитивные
s <- ugarchspec(variance.model = list(model="gjrGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(0, 0)), 
                distribution.model = 'sstd')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")



forc <- ugarchforecast(m, n.ahead = 20)


plot(ugarchforecast(m))


# AR(1) GJR-GARCH
# ar1 окзаался значимым, качество стало получе но незначитльено
s <- ugarchspec(variance.model = list(model="gjrGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(1, 0)), 
                distribution.model = 'sstd')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")




# TGARCH
# ar1 окзаался значимым, качество стало получе но незначитльено
s <- ugarchspec(variance.model = list(model="fGARCH",submodel="TGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(1, 0)), 
                distribution.model = 'sstd')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")



# simulation
s <- ugarchspec(variance.model = list(model="sGARCH", garchOrder = c(1,1)), 
                mean.model = list(armaOrder=c(0, 0)), 
                distribution.model = 'sstd')
m <- ugarchfit(spec=s, data=ts(data$returns))
m
plot(m, which="all")

sfinal <- s
setfixed(sfinal) <- as.list(coef(m))


f2020 <- ugarchforecast(m, n.ahead = 20)

data$returns

data$date <- as.Date(data$DateTime)

tmp <- as.xts(data[, "Close"], order.by=data[, "date"])

tmp["/2022-12-29"]

plot(ugarchforecast(m))

data$DateTime1 <- as.POSIXct(data$DateTime)

################################

intradayData = getSymbols.Moex('SBER', period="5min", )







