# install and load packages
#install.packages("fpp")
library(fpp)

#install.packages("data.table")
library(data.table)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("xts")
library(xts)

# clear global environment (if necessary)
rm(list = ls())

# read in GOM data 
gom <- read.csv("~/Desktop/Metro/Gom_jobs.csv", header = TRUE, sep = ",", na.strings= c("$null$", " "))

# convert data frame to data table
setDT(gom)
str(gom)
View(gom)
tables()

### subset data to only keep relevant columns
gom <- subset(gom, select = c(Data_source:created_yearmonth))
str(gom)
View(gom)


### subset data to only keep relevant rows
# sum number of requisitions per month
gom$one <- 1
gom_months <- gom[, sum(one), by = created_yearmonth]
setnames(gom_months, c('created_yearmonth', 'counts'))
str(gom_months)

# created dates from factor variable
gom_months$date <- as.Date(gom_months$created_yearmonth, "%m/%d/%Y")

# remove empty row 
gom_months <- gom_months[complete.cases(gom_months),]

# create year and month variable from date
gom_months$year <- as.numeric(format(gom_months$date, "%Y"))
gom_months$month <- as.numeric(format(gom_months$date, "%m"))

# subset to only include years from 2010 - 2014
gom_months <- subset(gom_months, year %in% 2010:2014)

# order rows by date
gom_months <- gom_months[order(date), ]

# remove data from July 2014 (last row)
gom_months <- gom_months[-55, ]
View(gom_months)

# add excel forecast
gom_months_excel <- c(4420, 4507, 4560, 4651, 4703, 4601, 4486, 4455, 4371, 4351, 4358, 4284, 4194, 4211, 4240, 4321, 4369, 4430)

# removing the effect of the number of days
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),5)
monthdays <- monthdays[-55:-60]
View(monthdays)

monthdays_excel <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),2)
monthdays_excel <- monthdays_excel[-19:-24]
View(monthdays_excel)

gom_months <- gom_months/monthdays
gom_months_excel <- gom_months_excel/monthdays_excel
View(gom_months)
View(gom_months_excel)

### plot data
ggplot(data = gom_months2, aes(x = date, y = counts)) + geom_line()


### Quick-R: Time Series and Forecasting
# create vector for counts
counts <- gom_months


# create ts for data from 2010 - 2014
gom_months <- ts(counts, start = c(2010, 1), end = c(2014, 6), frequency = 12)
gom_months_excel <- ts(gom_months_excel, start = c(2013, 1), end = c(2014, 6), frequency = 12)
View(gom_months)

# subset ts from 2010 - 2012 to create training data
gom_train <- window(gom_months, start = c(2010, 1), end = c(2012, 12))
gom_test <- window(gom_months, start = c(2013, 1), end = c(2014, 6))

### forecast with mean, naive, and seasonal naive models
# mean model
gom_mean <- meanf(gom_train, h = 18)

# naive model
gom_naive <- naive(gom_train, h = 18)

# seasonal naive model
gom_sn <- snaive(gom_train, h = 18)
plot(gom_sn, xlab = 'Time', ylab = 'Average No. of Reqs / Month')
lines(gom_test, lwd = 2, col = 'red')
legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Seasonal Naive Forecast","Actual Counts"))

View(gom_months_excel)
### plots of actual data and models
plot(gom_months)
lines(gom_mean$mean, col = 'blue', lwd = 2)
lines(gom_months_excel, col = 'blue', lwd = 2)
lines(gom_naive$mean, col = 'red', lwd = 2)
lines(gom_sn$mean, col = 'green', lwd = 2)
legend("topleft",lty=1, lwd = 2 ,col=c(2,3,4), legend=c("Mean method","Naive method","Seasonal naive method"))


### seasonal decomposition
# breaking down the data into seasons and trend
gom_stl <- stl(gom_months, s.window = 'periodic', t.window = 12, robust = T)
plot(gom_stl)
summary(gom_stl)

plot(elecequip, col="grey",
     main="Electrical equipment manufacturing",
     xlab="", ylab="New orders index")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted")

dev.off()
plot(gom_months, col = 'grey', ylab = 'Average No. of Reqs / Month', main = 'Seasonally Adjusted Average No. of Reqs / Month')
lines(seasadj(gom_stl),col="red",ylab="Seasonally adjusted")
legend("topleft",lty=1, lwd = 2 ,col=c('grey', 'red'), legend=c("Original data","Seasonally-adjusted data"))

seasonal <- gom_stl$time.series[,1]
trend <- gom_stl$time.series[,2]
remainder <- gom_stl$time.series[,3]

plot(trend)
plot(seasonal)
plot(remainder)
plot(trend+remainder, ylab = 'Average No. of Reqs / Month', main = 'Seasonally Adjusted Average No. of Reqs / Month')


monthplot(gom_months, xlab = 'Months', ylab = 'Average No. of Reqs / Month', main = 'Average No. of Reqs in each month')
seasonplot(gom_months, year.labels.left = T, xlab = 'Time', ylab = 'Average No. of Reqs / Month', main = 'Seasonality of Average No. of Reqs in each year')

# stl model 
gom_stltrain <- stl(gom_train, s.window = 'periodic', t.window = 12, robust = T)
plot(gom_stltrain)
gom_stlf <- forecast(gom_stltrain, method = 'naive', h = 18)
summary(gom_stltrain)

plot(gom_stlf, xlab = 'Time', ylab = 'Average No. of Reqs / Month')
lines(gom_test, lwd = 2, col = 'red')
legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("STL Forecast","Actual Counts"))


### holt-winters
gom_hwf <- hw(gom_train, seasonal = 'multiplicative', damped = T, initial = 'optimal', h = 18)
lines(gom_hwf$mean, col = 'green', lwd = 2)
plot(gom_hwf)

gom_hwf2 <- hw(gom_train, seasonal = 'additive', damped = T, initial = 'optimal', h = 18)

dev.off()
plot(gom_hwf2, xlab = 'Time', ylab = 'Average No. of Reqs / Month')
lines(gom_test, lwd = 2, col = 'red')
legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Holt-Winters Forecast","Actual Counts"))

### arima
gom_arima <- auto.arima(gom_train, stepwise = F, approximation = F)
gom_arimaf <- forecast(gom_arima, h = 18)
lines(gom_arimafore$mean, col = 'purple', lwd = 2)
summary(gom_arima)
plot(gom_arimaf)

# regression
gom_reg <- tslm(gom_train ~ trend + season)
summary(gom_reg)
gom_regf <- forecast(gom_reg, h = 18)
lines(gom_regf$mean, col = 'blue', lwd = 2)

plot(gom_regf, xlab = 'Time', ylab = 'Average No. of Reqs / Month')
lines(gom_test, lwd = 2, col = 'red')
legend("topleft",lty=1, lwd = 2 ,col=c('blue', 'red'), legend=c("Regression Forecast","Actual Counts"))


### comparison plot and legend
dev.off()
par(oma = c(2, 1, 1, 1))
plot(gom_months, xlab = 'Time', ylab = 'Average No. of Reqs / Month', main = 'Comparison of Forecasts to actual data')
lines(gom_months_excel, col = 'orange', lwd = 2)
lines(gom_sn$mean, col = 'purple', lwd = 2)
lines(gom_stlf$mean, col = 'red', lwd = 2)
lines(gom_hwf2$mean, col = 'green', lwd = 2)
lines(gom_regf$mean, col = 'blue', lwd = 2)
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('bottom','groups',c('Linear Regression', 'Seasonal Naive', "STL","Holt-Winters","Regression"), lty = 1, lwd = 2, col = c('orange', 'purple','red', 'green', 'blue'), ncol = 3, bty = 'n')

### checking accuracy
accuracy(gom_sn, gom_test)[, 'MASE']
accuracy(gom_stlf, gom_test)[, 'MASE']
accuracy(gom_hwf, gom_test)[, 'MASE']
accuracy(gom_hwf2, gom_test)[, 'MASE']
accuracy(gom_regf, gom_test)[, 'MASE']
accuracy(gom_months_excel, gom_test)

accuracy(gom_sn, gom_test)[, 'RMSE']
accuracy(gom_stlf, gom_test)[, 'RMSE']
accuracy(gom_hwf2, gom_test)[, 'RMSE']
accuracy(gom_regf, gom_test)[, 'RMSE']

### auto exponential
gom_ets <- ets(gom_train)
plot(gom_ets)

