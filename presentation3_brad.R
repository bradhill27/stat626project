library(astsa)

## Read in the data ##
raw <- read.csv("mlb.csv")
## Including years before dead ball era to possibly model variance using G/ARCH ##

## Adding period factor variable ##
raw$period <- as.factor(ifelse(raw$Year < 1901, "Pre-Dead Ball",
              ifelse(raw$Year >= 1901 & raw$Year < 1920, "Dead Ball",
              ifelse(raw$Year >= 1920 & raw$Year < 1942, "Live Ball",
              ifelse(raw$Year >= 1942 & raw$Year < 1961, "Integration",
              ifelse(raw$Year >= 1961 & raw$Year < 1977, "Expansion",
              ifelse(raw$Year >= 1977 & raw$Year < 1994, "Free Agency",
              ifelse(raw$Year >= 1994 & raw$Year < 2006, "Steroid",
              ifelse(raw$Year >= 2006, "Post-Steroid", "ERROR")))))))))

## Period ##
period <- rev(raw$period)

## ERA ##
era <- ts(rev(raw$ERA), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(era, main = 'Earned Run Average Time Series', xlab = 'Year', ylab = 'ERA')
xt.kernal = ksmooth(raw$Year, raw$ERA, "normal", bandwidth = 5)
lines(xt.kernal, lwd = 2, col = "red")

## Batting Average ##
ba <- ts(rev(raw$BA), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(ba, main = 'Batting Average Time Series', xlab = 'Year', ylab = 'BA')

## Hits ##
hits <- ts(rev(raw$H), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(hits, main = 'Hits Time Series', xlab = 'Year', ylab = 'Hits')

## Home Runs ##
hrs <- ts(rev(raw$HR), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(hrs, main = 'Home Runs Time Series', xlab = 'Year', ylab = 'HR')

## Strikeouts ##
so <- ts(rev(raw$SO), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(so, main = 'Strikeouts Time Series', xlab = 'Year', ylab = 'SO')

## Look for trends between covariates and response and check for multicollinearity ##
panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), 2)
  text(0.5, 0.5, r, cex = 1.75)
}

## Batting Average and Hits are highly correlated. Strikeouts and Home Runs are highly correlated ##
## Keep only ERA, Batting Average, Home Runs, and Period ##
pairs(cbind(ERA = era, BattingAvg = ba, HomeRuns = hrs, Period = period), lower.panel = panel.cor)

## Fitting models ##
hrs_2 <- hrs^2
trend <- time(era)
fit_e <- lm(era ~ trend + hrs + ba)
fit_f <- lm(era ~ trend + hrs + ba + period)

summary(fit_e)
summary(fit_f)

AIC(fit_e)
AIC(fit_f)

BIC(fit_e)
BIC(fit_f)
# fit_f (including period) beats out fit_e ##

## ACF and PACF of detrended ERA data using Model F from above ##
## Stationarity questionable ##
## PACF plot indicates an AR(1) model would be a good fit ##
detera <- resid(fit_f)
acf2(detera, main = "Detrended ERA")
tsplot(detera, main = "Detrended ERA Time Series", ylab = "Detrended ERA")

## Fit AR(1) model to detrended ERA data (using Model E) ##
arfit <- sarima(detera, p=1, d=0, q=0, no.constant = TRUE)
arfit

## Investigate residuals and residuals^2 (r2) ##
r <- resid(arfit$fit)
acf2(r, main = "AR(1) Residuals")
tsplot(r, main = "AR(1) Residuals Time Series")
r2 <- r^2
acf2(r2, main = expression("AR(1)" ~ Residuals^2))
## PACF of r2 cuts off after lag-1 and ACF seems to tail off, indicating an ARCH model ##

## Fitting an AR(1)-ARCH(1) model on the detrended data ##
library(fGarch)
ararchfit <- garchFit(~arma(1,0) + garch(1,0), data = detera)
summary(ararchfit)

## Fitting an AR(1)-ARCH(2) model on the detrended data ##
ararch2fit <- garchFit(~arma(1,0) + garch(2,0), data = detera)
summary(ararch2fit)
## Based on Information Criteria, AR(1)-ARCH(2) fit is better, but alpha_2 is not significant at .05 level ##
## Continue with AR(1)-ARCH(1) model for model simplicity ##

preds <- predict(ararchfit, plot = TRUE)
y_t <- predict.lm(fit_f, newdata = data.frame(trend = 2022:2031, hrs = rep(hrs[151], 10), 
                                       ba = rep(ba[151], 10), period = as.factor(rep("Post-Steroid", 10))))

prediction <- preds$meanForecast + y_t
lower_bound <- preds$lowerInterval + y_t
upper_bound <- preds$upperInterval + y_t

tsplot(era, col = "gray22", type = "b", main = 'Prediction and 95% Interval for Earned Run Average', ylab = "ERA",
       xlim = c(1871, 2031))
lines(x = 2022:2031, y = prediction, col = "red", type = "b")
lines(x = 2022:2031, y = upper_bound, type = "l", lwd = 2)
lines(x = 2022:2031, y = lower_bound, type = "l", lwd = 2)
legend(1983, 2.75, legend=c("Observed Data", "Prediction", "95% Confidence Bound"), 
       col = c("gray22", "red", "black"), pch = c(1, 1, NA), lty = c(NA, NA, 1), lwd = c(1, 1, 2))

