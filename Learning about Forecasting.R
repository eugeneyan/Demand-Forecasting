# install and load packages
install.packages("fpp")
library(fpp)

# load data on australian beer production
data(ausbeer)

# sample simple forecasting techniques on Australian beer productino
beer2 <- window(ausbeer,start=1992,end=2006-.1)
beerfit1 <- meanf(beer2, h=11)
beerfit2 <- naive(beer2, h=11)
beerfit3 <- snaive(beer2, h=11)
View(beerfit3)
View(beer2)
View(beerfit1)
View(beerfit2)

plot(beerfit1, plot.conf=FALSE, 
     main="Forecasts for quarterly beer production")
lines(beerfit2$mean,col=2)
lines(beerfit3$mean,col=3)
legend("topright",lty=1,col=c(4,2,3),
       legend=c("Mean method","Naive method","Seasonal naive method"))

beer2 <- window(ausbeer,start=1992,end=2006-.1)
fit <- tslm(beer2 ~ trend + season)
summary(fit)

str(beer2)
View(beer2)

plot(beer2, xlab="Year", ylab="", main="Quarterly Beer Production")
lines(fitted(fit), col=2)
legend("topright", lty=1, col=c(1,2), legend = c("Actual", "Predicted"))

# transformations on the data
plot(log(elec), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)

# The BoxCox.lambda() function will choose a value of lambda for you.
lambda <- BoxCox.lambda(elec) # = 0.27
plot(BoxCox(elec,lambda))

# removing the effect of the number of days in a month (calendar adjustment)
monthdays <- rep(c(31,28,31,30,31,30,31,31,30,31,30,31),14)
monthdays[26 + (4*12)*(0:2)] <- 29
par(mfrow=c(2,1))
plot(milk, main="Monthly milk production per cow",
     ylab="Pounds",xlab="Years")
plot(milk/monthdays, main="Average milk production per cow per day", 
     ylab="Pounds", xlab="Years")

par(mfrow=c(1,1))
plot(Mwh ~ temp, data=econsumption)
fit  <- lm(Mwh ~ temp, data=econsumption)
plot(residuals(fit) ~ temp, data=econsumption)
forecast(fit, newdata=data.frame(temp=c(10,35)))


# non-linear regression models
Cityp <- pmax(fuel$City-25,0)
fit2 <- lm(Carbon ~ City + Cityp, data=fuel)
x <- 15:50; z <- pmax(x-25,0)
fcast2 <- forecast(fit2, newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast2$mean,col="red")

# cubic regression spline
fit3 <- lm(Carbon ~ City + I(City^2) + I(City^3) + I(Cityp^3), data=fuel)
fcast3 <- forecast(fit3,newdata=data.frame(City=x,Cityp=z))
plot(jitter(Carbon) ~ jitter(City), data=fuel)
lines(x, fcast3$mean,col="red")




x <- xts(1:10, Sys.Date()+1:10)
dimnames(x) <- c("date", "time")
rownames(x)
rownames(x) <- 1:10
rownames(x)
str(x)
View(x)
