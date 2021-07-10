data <- read.csv("mlb.csv")
names(data)
library(astsa)

par(mfrow = c(2,1))
tsplot(x = data$Year, y=data$ERA, col = 3)
xt.kernal = ksmooth(data$Year, data$ERA, "normal", bandwidth = 5)
lines(xt.kernal, lwd = 2, col = 4)
tsplot(x = data$Year, y=data$BA, col = 3)
xt.kernal = ksmooth(data$Year, data$BA, "normal", bandwidth = 5)
lines(xt.kernal, lwd = 2, col = 4)
abline(v = c(1901,1919.5,1941.5,1960.5,1976.5,1993.5,2005.5))

acf1(data$ERA, max.lag = 50)
acf1(data$BA, max.lag = 50)

hist(data$ERA, breaks = 15)
hist(data$BA, breaks = 15)

ccf2(data$ERA,data$BA, max.lag = 50)

par(mfrow = c(3,1))
tsplot(x = data$Year, y= log(data$ERA), col = 3)
acf1(log(data$ERA), max.lag = 50)
hist(log(data$ERA), breaks = 15)

par(mfrow = c(3,1))
diff = diff(log(data$ERA))
tsplot(x = data$Year[-1], y= diff, col = 3)
acf1(diff, max.lag = 50)
hist(diff, breaks = 15)

par(mfrow = c(3,1))
diff = diff(data$ERA)
tsplot(x = data$Year[-1], y= diff, col = 3)
acf1(diff, max.lag = 50)
hist(diff, breaks = 15)

par(mfrow = c(1,1))
plot(data$ERA, data$BA)

par(mfrow = c(1,1))
lag1.plot(data$ERA, 12)

par(mfrow = c(1,1))
lag2.plot(data$ERA, data$BA, 12)