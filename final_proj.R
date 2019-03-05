
library(tseries)
library(ggplot2)
library(forecast)
library(TTR)

bike <- read.csv("day.csv", TRUE, ",", stringsAsFactors = FALSE)
names(bike)
View(bike)

#missing values
missing_values <- sum(is.na(bike))/(ncol(bike)*nrow(bike))
missing_values
str(bike)
#converting the dteday char into date
bike$Date <-as.Date(bike$dteday, "%m/%d/%Y")

#visulazing the data
dev.off()
ggplot(bike, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")

#cleaning of data
count_ts = ts(bike[, c('cnt')])
bike$clean_cnt = tsclean(count_ts)
ggplot() + geom_line(data = bike, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')

#box plots
ggplot(bike, aes(x = weekday,y=cnt)) + geom_boxplot(aes(group=weekday))+ scale_x_continuous(breaks=seq(0,7,1))+ ggtitle("Bike Hiring Count by Day")

ggplot(bike, aes(y=cnt, x = mnth)) + geom_boxplot(aes(group=mnth)) + scale_x_continuous(breaks=seq(0,12,1), labels=c("","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) + ggtitle("Bike Hiring Count by Month")

boxplot(bike$cnt ~ bike$season,
        data = bike,
        main = "Total Bike Rentals Vs Season",
        xlab = "Season",ylab = "Total Bike Rentals")

boxplot(bike$cnt ~ bike$holiday,
        data = bike,
        main = "Total Bike Rentals Vs Holiday/Working Day",
        xlab = "Holiday/Working Day",
        ylab = "Total Bike Rentals")

boxplot(bike$cnt ~ bike$weathersit,
        data = bike,
        main = "Total Bike Rentals Vs Weather Situation",
        xlab = "Weather Situation",
        ylab = "Total Bike Rentals")

#windspeed
plot( bike$windspeed,bike$cnt,xlab="wind speed", ylab="count",main ="Windspeed vs Count" ,pch =19)
abline(lm(bike$cnt~bike$windspeed), col="red")

##############################################

#lm-model
lm_model1 <- lm(bike$clean_cnt ~ bike$atemp + bike$weathersit + bike$windspeed + bike$season + bike$hum +bike$temp)
summary(lm_model1)
lm_model2 <- lm(bike$clean_cnt ~ bike$atemp + bike$weathersit + bike$windspeed + bike$season + bike$hum)
summary(lm_model2)

#correlations
temp <- cor(bike$clean_cnt, bike$atemp)
weather <- cor(bike$clean_cnt, bike$weathersit)
ws <- cor(bike$clean_cnt, bike$windspeed)
season <- cor(bike$clean_cnt, bike$season)
humidity <- cor(bike$clean_cnt, bike$hum)
temp
weather
ws
season
humidity

##############################################
#time series
bike_ts <- ts(bike$clean_cnt, frequency = 30)
bike_ts_decomp <- decompose(bike_ts, type = "multiplicative")
plot(bike_ts_decomp)

#stationarity test
adf.test(bike_ts, alternative = "stationary")

#deseasonal
acf_bike <- Acf(bike_ts, main="")
pacf_bike <- Pacf(bike_ts, main="")

count_d <- diff(bike$clean_cnt, differences = 1)
plot(count_d)

adf.test(count_d1, alternative = "stationary")
acf_bike_d1 <- Acf(count_d1, main = "Acf for differenced series") #1
pacf_bike_d1 <- Pacf(count_d1, main = "Pacf for differenced series") #7

#arima model
fit_arima <- auto.arima(bike$clean_cnt, seasonal = F)
fit_arima
tsdisplay(residuals(fit_arima),lag.max=15, main='Residuals')
fcast_arima <- forecast(fit_arima, h=30)
plot(fcast_arima)

fit1 <- arima(bike$clean_cnt, order = c(1,1,7))
fit1
fcast <- forecast(fit1, h = 30)
plot(fcast)

fit2 <- auto.arima(bike$clean_cnt, seasonal = T)
fit2
fcast2 <- forecast(fit2, h =30)
plot(fcast2)
tsdisplay(residuals(fit_arima),lag.max=15, main='Residuals')

