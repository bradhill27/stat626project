library(astsa)

## Read in the data ##
raw <- read.csv("mlb.csv")

## ERA ##
era <- ts(rev(raw$ERA), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(era, main = 'Earned Run Average Time Series', xlab = 'Year', ylab = 'ERA')

## Batting Average ##
ba <- ts(rev(raw$BA), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(ba, main = 'Batting Average Time Series', xlab = 'Year', ylab = 'BA')

## Hits ##
hits <- ts(rev(raw$H), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(hits, main = 'Hits Time Series', xlab = 'Year', ylab = 'Hits')

## Home Runs ##
hrs <- ts(rev(raw$HR), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(hrs, main = 'Home Runs Time Series', xlab = 'Year', ylab = 'HRs')

## Strikeouts ##
so <- ts(rev(raw$SO), start = raw$Year[length(raw$Year)], end = raw$Year[1])
tsplot(so, main = 'Strikeouts Time Series', xlab = 'Year', ylab = 'SOs')

## Look for trends between covariates and response and check for multicollinearity ##
panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), 2)
  text(0.5, 0.5, r, cex = 1.75)
}
pairs(cbind(ERA = era, BattingAvg = ba, Hits = hits, Strikeouts = so, HomeRuns = hrs), lower.panel = panel.cor)

## Batting Average and Hits are highly correlated. Strikeouts and Home Runs are highly correlated ##
## Keep only ERA, Batting Average, and Home Runs ##
pairs(cbind(ERA = era, BattingAvg = ba, HomeRuns = hrs), lower.panel = panel.cor)

## Fitting nested models, include squared HR term because of curved trend between HR and ERA ##
hrs_2 <- hrs^2
trend <- time(era)
fit_a <- lm(era ~ trend)
fit_b <- lm(era ~ trend + hrs)
fit_c <- lm(era ~ trend + hrs + hrs_2)
fit_d <- lm(era ~ trend + hrs + hrs_2 + ba)
fit_e <- lm(era ~ trend + hrs + ba)

summary(fit_a)
summary(fit_b)
summary(fit_c)
summary(fit_d)
summary(fit_e)

AIC(fit_a)
AIC(fit_b)
AIC(fit_c)
AIC(fit_d)
AIC(fit_e)

BIC(fit_a)
BIC(fit_b)
BIC(fit_c)
BIC(fit_d)
BIC(fit_e)
## fit_d (full model) is the best ##

## ACF and PACF of original ERA time series ##
acf2(era)
tsplot(era)

## ACF and PACF of log(ERA) ###
lera <- log(era)
acf2(lera)
tsplot(lera)

## ACF and PACF of detrended ERA data using Model D from above ##
## Looks pretty stationary except for first 25 years ##
## PACF plot indicates an AR(1) model would be a good fit ##
detera <- resid(fit_d)
acf2(detera)
tsplot(detera)

## Trying a Box-Cox transformation ##
library(MASS)
bc <- boxcox(era ~ time(era))
(lambda <- bc$x[which.max(bc$y)])
bcera <- (era^lambda-1)/lambda
acf2(bcera)
tsplot(bcera)

## ACF and PACF of differenced ERA data ##
## Appears stationary ##
## No clear winner between AR(p) and MA(q) based on ACF/PACF plots ##
dera <- diff(era)
acf2(dera)
tsplot(dera)

## Lag plots of differenced ERA data ##
lag1.plot(dera, 12)
lag2.plot(dera, ba, 12)
lag2.plot(dera, hrs, 12)

## Fit AR(1) model to differenced ERA data ##
arfit <- sarima(dera, p=1, q=0, d=0, no.constant = TRUE)
arfit

## Fit MA(1) model to differenced ERA data ##
mafit <- sarima(dera, p=0, q=1, d=0, no.constant = TRUE)
mafit

## Fit ARMA(1,1) model to differenced ERA data ##
## Lowest AIC/BIC ##
## Phi and theta estimates significantly different from zero ##
## Residuals appear to be white noise ##
armafit <- sarima(dera, p=1, q=1, d=0, no.constant = TRUE)
armafit

## Including covariates (don't think this is the right way to do it) ##
## armafit_reg <- sarima(dera, p=1, q=1, d=0, no.constant = TRUE, xreg = as.matrix(cbind(hrs, hrs_2, ba))[-1,])
## armafit_reg

## Forecasting 5 years with ARMA(1,1) model for differenced data ##
sarima.for(dera, p=1, q=1, d=0, no.constant = TRUE, n.ahead=5)
## sarima.for(dera, p=1, q=1, d=0, no.constant = TRUE, xreg = as.matrix(cbind(hrs, hrs_2, ba))[-1,], n.ahead = 5,
           ## newxreg = as.matrix(cbind(rep(mean(hrs), 5), rep(mean(hrs_2), 5), rep(mean(ba), 5))))

## Fit AR(1) model to detrended ERA data (using Model D) ##
arfit <- sarima(detera, p=1, q=0, d=0, no.constant = TRUE)
arfit

## Forecasting 20 years with AR(1) model for detrended data ##
sarima.for(detera, p=1, q=0, d=0, no.constant = TRUE, n.ahead=20)
