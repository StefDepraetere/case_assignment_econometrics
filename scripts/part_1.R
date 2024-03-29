
rm(list = ls())

## Install and load required packages
if(!require(pastecs)){install.packages("pastecs")}
if(!require(psych)){install.packages("psych")}
if(!require(moments)){install.packages("moments")}
if(!require(lmtest)){install.packages("lmtest")}
if(!require(sandwich)){install.packages("sandwich")}
if(!require(AER)){install.packages("AER")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(nlme)){install.packages("nlme")}
if(!require(orcutt)){install.packages("orcutt")}
if(!require(orcutt)){install.packages("tinytex")}
if(!require(orcutt)){install.packages("urca")}


library(pastecs)     ## Descriptive Statistics
library(psych)       ## Correlation plot
library(moments)     ## Test for Normality
library(lmtest)      ## Test for heteroscedasticity
library(sandwich)    ## Newey-West HAC estimators
library(AER)         ## 2SLS
library(stargazer)   ## Stargazer
library(orcutt)      ## Cochrane-Orcutt EGLS
library(nlme)        ## Linear models with autocorrelated error terms
library(openxlsx)    ## To read an excel xlsx data file
library(fastDummies) ## Create dummies based on different categories in a variable
library(datawizard)  ## Data transformations (e.g. demeaning by group)
#library(EconometricsUGent)  ## Additional functions
library(ggplot2)
library(tinytex)
library(urca)
source(".\\scripts\\TimeSeriesFunctions_v3.R")

output=".\\visuals"
#read and collect right data
data <- read.csv((".\\data.csv"), header=TRUE, sep = ",")

data.ts <- ts(data, start = c(1974, 1), end = c(2017, 12), frequency = 12)
US_IP <- data.ts[,"US_IP"]
US_IP_diff <- diff(US_IP, differences = 1)


##QUESTION 1
#plot US_IP
pdf(file=".\\visuals\\question_1.pdf")
#par(mar = c(2,2,0,2))                                                                             # Adjust margins
plot(US_IP, main = "Time series lnYt", ylab = "US_IP_log", xlab = "Time", lwd = 2, col = "darkblue", bty = "l", col.axis = "black")
abline(h = c(100,200,300,400), col = "darkgrey", lty = 2)
#dev.off()

#plot US_IP_diff
#pdf(file=".\\visuals\\US_IP_log_diff.pdf")
#par(mar = c(2,2,0,2))                                                                             # Adjust margins
plot(US_IP_diff, main="Time series diff lnYt",ylab = "Diff lnYt", xlab = "Time", lwd = 2, col = "darkblue", bty = "l", col.axis = "black")
abline(h = c(100,200,300,400), col = "darkgrey", lty = 2)
dev.off()

#reduced number of observations
reduced.ts <- ts(data, start = c(1974, 1), end = c(2015, 12), frequency = 12)
US_IP <- reduced.ts[,"US_IP"]
US_IP_diff <- diff(US_IP, differences = 1)

####################################################################
###   ACF, PACF, Box-Pierce and Ljung_box statistics             ###
####################################################################

#question 2
#acf_levels
pdf(file=".\\visuals\\question_2.pdf")
US_IP_diff_acf <- acf(US_IP_diff)
#par(mar = c(2,2,0,0)) 
plot(US_IP_diff_acf,main = "ACF US IP diff", ylab = "Autocorrelation", xlab = "Lag", lwd = 7.5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")
#dev.off()

#pacf_levels
#pdf(file=".\\visuals\\pacf_US_IP_diff.pdf")
US_IP_diff_pacf <- pacf(US_IP_diff)
#par(mar = c(2,2,0,0)) 
plot(US_IP_diff_pacf,main = "PACF US IP diff", ylab = "Partial autocorrelation", xlab = "Lag", lwd = 7.5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")


#same graph
par(mfrow = c(1,2)) 
plot(US_IP_diff_acf,main = "", ylab = NULL, xlab = "Lag", lwd = 5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")
plot(US_IP_diff_pacf, main = "", ylab = NULL, xlab = "Lag", lwd = 5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")
dev.off()

#ljung-box statistics
QstatDiff = LjungBox(US_IP_diff,10,0)
stargazer(QstatDiff,type = "text", summary = FALSE)

#question 3 
AIC_US_IP_diff<- aic_table_css(US_IP_diff,7,7)
AIC_US_IP_diff

# ARMA(6,3); AIC 2638,86
# ARMA(3,6); AIC 2643,42
# 
# ARMA(7,4); AIC 2633,68	choose this one for 11 parameters
# ARMA(6,5); AIC 2640,74
# ARMA(5,6); AIC 2641,41

BIC_US_IP_diff<- bic_table_css(US_IP_diff,7,7)
BIC_US_IP_diff

# ARMA(1,1); BIC 2680,29
# ARMA(2,2); BIC 2673,42
# ARMA(6,3); BIC 2680,92

#fit the chosen models

#ARMA63, most important one
ARMA63 <- arima(US_IP_diff,order=c(6,0,3), method = "CSS-ML", include.mean = TRUE)
ARMA63AIC = round(AIC(ARMA63),digits=4)
ARMA63BIC = round(AIC(ARMA63,k = log(length(US_IP_diff))),digits=4)
ARMA63RSS = round(sum(residuals(ARMA63)^2),digits=4)

stargazer(ARMA63,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, 
          digits = 4, digits.extra = 0, 
          df = T, report="vcs*t",  
          dep.var.labels = "D(US_IP)",
          add.lines = list(c("AIC",ARMA63AIC),c("BIC",ARMA63BIC),c("RSS",ARMA63RSS)), 
          omit.stat = c("aic","sigma2"),
          output=".\\visuals\\arma63.txt")
                             

writeLines(try, ".\\visuals\\arma63.txt")


#using the css_ml method aic = 1017,2339 and bic = 1063,66 and rss 211,8416

#ARMA36, not really necessary because ARMA63 is better with same number of parameters
ARMA36 <- arima(US_IP_diff,order=c(3,0,6), method = "CSS-ML", include.mean = TRUE)
ARMA36AIC = round(AIC(ARMA36),digits=4)
ARMA36BIC = round(AIC(ARMA36,k = log(length(US_IP_diff))),digits=4)
ARMA36RSS = round(sum(residuals(ARMA36)^2),digits=4)

stargazer(ARMA36,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, 
          digits = 4, digits.extra = 0, 
          df = T, report="vcs*t",  
          dep.var.labels = "D(US_IP)",
          add.lines = list(c("AIC",ARMA36AIC),c("BIC",ARMA36BIC),c("RSS",ARMA36RSS)), 
          omit.stat = c("aic","sigma2"))

#ARMA74
ARMA74 <- arima(US_IP_diff,order=c(7,0,4), method = "CSS-ML", include.mean = TRUE)
ARMA74AIC = round(AIC(ARMA74),digits=4)
ARMA74BIC = round(AIC(ARMA74,k = log(length(US_IP_diff))),digits=4)
ARMA74RSS = round(sum(residuals(ARMA74)^2),digits=4)

stargazer(ARMA74,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, 
          digits = 4, digits.extra = 0, 
          df = T, report="vcs*t",  
          dep.var.labels = "D(US_IP)",
          add.lines = list(c("AIC",ARMA74AIC),c("BIC",ARMA74BIC),c("RSS",ARMA74RSS)), 
          omit.stat = c("aic","sigma2"))

#ARMA11
ARMA11 <- arima(US_IP_diff,order=c(1,0,1), method = "CSS-ML", include.mean = TRUE)
ARMA11AIC = round(AIC(ARMA11),digits=4)
ARMA11BIC = round(AIC(ARMA11,k = log(length(US_IP_diff))),digits=4)
ARMA11RSS = round(sum(residuals(ARMA11)^2),digits=4)

stargazer(ARMA11,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, 
          digits = 4, digits.extra = 0, 
          df = T, report="vcs*t",  
          dep.var.labels = "D(US_IP)",
          add.lines = list(c("AIC",ARMA11AIC),c("BIC",ARMA11BIC),c("RSS",ARMA11RSS)), 
          omit.stat = c("aic","sigma2"))

#ARMA22
ARMA22 <- arima(US_IP_diff,order=c(2,0,2), method = "CSS-ML", include.mean = TRUE)
ARMA22AIC = round(AIC(ARMA22),digits=4)
ARMA22BIC = round(AIC(ARMA22,k = log(length(US_IP_diff))),digits=4)
ARMA22RSS = round(sum(residuals(ARMA22)^2),digits=4)

stargazer(ARMA22,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, 
          digits = 4, digits.extra = 0, 
          df = T, report="vcs*t",  
          dep.var.labels = "D(US_IP)",
          add.lines = list(c("AIC",ARMA22AIC),c("BIC",ARMA22BIC),c("RSS",ARMA22RSS)), 
          omit.stat = c("aic","sigma2"))

#look at impulse response function
IRFARMA63    = IRFarma(ARMA63, 30)
IRFARMA36    = IRFarma(ARMA36, 30)
IRFARMA74    = IRFarma(ARMA74, 30)
IRFARMA11    = IRFarma(ARMA11, 30)
IRFARMA22    = IRFarma(ARMA22, 30)

pdf(".\\visuals\\impulse_response_function_2.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
plot(IRFARMA63, ylab = "Impulse Response", xlab = "T",lwd = 0.5, col="darkblue", bty = 'l', col.axis = "black", type = "l", 
     ylim = c(min(IRFARMA63, IRFARMA36,IRFARMA74, IRFARMA11, IRFARMA22),
              max(IRFARMA63, IRFARMA36,IRFARMA74, IRFARMA11, IRFARMA22)),
     panel.first = abline(h = c(0.6,0.8,1,1.2,1.4,1.6), col = "darkgrey", lty = 2))
lines(IRFARMA36, col = "springgreen3", lwd = 0.5)
lines(IRFARMA74, col = "red", lwd = 0.5)
lines(IRFARMA11, col = "yellow", lwd = 0.5)
lines(IRFARMA22, col = "purple", lwd = 0.5)
legend("topright",legend = c("ARMA(6,3)","ARMA(3,6)","ARMA(7,4)", "ARMA(1,1)", "ARMA(2,2)"), lty = c(1,1), lwd = c(3,3)
       ,col = c("darkblue","springgreen3","red", "yellow", "purple"), horiz = TRUE, cex = 1.25, bty = "n")
dev.off()

# check for joint significance

#nulhypothese is dat meer parameters slechter is
#dus boven nulhypothese ga je voor de meer parameters

# Compare ARMA(6,3) to ARMA(7,4) 

Fstat = ( (ARMA63RSS - ARMA74RSS)/2 ) / ( ARMA74RSS/(ARMA74$nobs-12))
Fstat
qf(0.95, 2, ARMA74$nobs-12)
pf(Fstat, 2, ARMA74$nobs-12, lower.tail = FALSE)
#Fstat 8,7, critical value 3,01

ARMA74$loglik
ARMA63$loglik
LRtest = 2*(ARMA74$loglik-ARMA63$loglik)
LRtest
qchisq(0.95, 2)
pchisq(LRtest, 2, lower.tail = FALSE)
#LRtest 15,24; CHIsquared 5.99

# Compare ARMA(2,2) to ARMA(6,3) 

Fstat = ( (ARMA22RSS - ARMA63RSS)/5 ) / ( ARMA63RSS/(ARMA63$nobs-9))
Fstat
qf(0.95, 5, ARMA63$nobs-9)
pf(Fstat, 5, ARMA63$nobs-9, lower.tail = FALSE)
#Fstat 2.53, critical value 2.23

LRtest = 2*(ARMA63$loglik-ARMA22$loglik)
LRtest
qchisq(0.95, 5)
pchisq(LRtest, 5, lower.tail = FALSE)
#LRtest 10,28; CHIsquared 11,07

# Compare ARMA(1,1) to ARMA(2,2) 

Fstat = ( (ARMA11RSS - ARMA22RSS)/2 ) / ( ARMA22RSS/(ARMA22$nobs-4))
Fstat
qf(0.95, 2, ARMA22$nobs-4)
pf(Fstat, 2, ARMA22$nobs-4, lower.tail = FALSE)
#Fstat 0.399, critical value 3,01

LRtest = 2*(ARMA22$loglik-ARMA11$loglik)
LRtest
qchisq(0.95, 2) # 5% critical value
pchisq(LRtest, 2, lower.tail = FALSE) 
#LRtest 0.79; CHIsquared 5.99

# Compare ARMA(2,2) to ARMA(7,4) 

Fstat = ( (ARMA22RSS - ARMA74RSS)/7 ) / ( ARMA74RSS/(ARMA74$nobs-12))
Fstat
qf(0.95, 7, ARMA74$nobs-12) 
pf(Fstat, 7, ARMA74$nobs-12, lower.tail = FALSE)
#Fstat 4,35, critical value 2.02

LRtest = 2*(ARMA74$loglik-ARMA22$loglik)
LRtest
qchisq(0.95, 7) # 5% critical value
pchisq(LRtest, 7, lower.tail = FALSE) # p-value
#LRtest 25.52; CHIsquared 14.06

# Compare ARMA(1,1) to ARMA(7,4) 

Fstat = ( (ARMA11RSS - ARMA74RSS)/9 ) / ( ARMA74RSS/(ARMA74$nobs-12))
Fstat
qf(0.95, 9, ARMA74$nobs-12) 
pf(Fstat, 9, ARMA74$nobs-12, lower.tail = FALSE)
#Fstat 3,48, critical value 1.90

LRtest = 2*(ARMA74$loglik-ARMA11$loglik)
LRtest
qchisq(0.95, 9) # 5% critical value
pchisq(LRtest, 9, lower.tail = FALSE) # p-value
#LRtest 26,32; CHIsquared 16,91



#inspect residuals
resARMA74 <- ARMA74$residuals

QstatresARMA74 = LjungBox(resARMA74,50,11)
stargazer(QstatresARMA74,type = "text", summary = FALSE)

#graph residuals
res.ts <- ts(resARMA74, start = c(1974, 1), end = c(2015, 12), frequency = 12)
#US_IP_diff
IRFARMA74 = IRFarma(ARMA74, 504)


#acf & pacf for model residuals
resARMA74_ACF = acf(resARMA74)
resARMA74_PACF = pacf(resARMA74)
pdf(".\\visuals\\acf_pacf_residuals_arma74.pdf")
par( mfrow = c(1,2)) 
plot(resARMA74_ACF,main="Autocorrelation residuals",ylab="ACF",xlab="Lag",lwd=3,mar=c(2,2,0,0),col="darkblue",bty='l',col.axis="black")
plot(resARMA74_PACF,main="Partial autocorrelation residuals",ylab="PACF",xlab="Lag",lwd=3,mar=c(2,2,0,0),col="darkblue",bty='l',col.axis="black")
dev.off()

#plot data + fitted and residuals arma(7,4)
# Plot data + fitted and residuals from MA(4) model
pdf(".\\visuals\\model_ARMA74_resid.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
fitVal = US_IP_diff - resARMA74
par(mfrow = c(1,1))
plot(US_IP_diff, type = "l",col="red3", lwd = 2, xlab = NA, ylab = NA,ylim = c(-6,3),
     las = 1, bty = "u")
lines(fitVal, col = "green3", lwd = 2)
par(new = T)
plot(resARMA74, main= "ARMA(7,4)", type = "l", axes = F, xlab = NA, ylab = NA, col = "blue3", lwd = 2, lty = 3)
axis(side = 4)
par(new = T, xpd = NA, mar = c(par()$mar + c(-5,+3,0,0)))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab=NA,ylab=NA)
legend("bottom", legend = c("Actual","Fitted","Residuals (right axis)"), lty = c(1,1,3), lwd = c(2,2,2)
       ,col = c("red3","green3","blue3"), horiz = TRUE,cex=0.9,bty="n")
dev.off()

# CUSUM stability test on ARMA74
res_ARMA74 <- residuals(ARMA74)-mean(residuals(ARMA74))
sigma_ARMA74 <- sqrt(ARMA74$sigma2)
cs_ARMA74 <- cumsum(res_ARMA74) / (sqrt(length(US_IP_diff))*sigma_ARMA74)
cs_ARMA74.ts = ts(cs_ARMA74,start = c(1974, 1), end = c(2015, 12), frequency = 12)

pdf(".\\visuals\\CUSUM_ARMA74.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par()
plot(cs_ARMA74.ts, main = "CUSUM ARMA(7,4)", ylab = NULL, xlab = NULL,lwd = 3, col="darkblue", bty = 'l', col.axis = "black", type = "l",
     ylim = c(min(cs_ARMA74, -1.5), max(cs_ARMA74,1.5)))
abline(h = c(-1.36,1.36), col = "darkgrey", lty = 2,lwd = 2)
abline(h = c(0), col = "black", lwd = 2)
dev.off()

#forecasting
if(!require(forecast)){install.packages("forecast")}
library(forecast)

data.ts <- ts(data, start = c(1974, 1), end = c(2017, 12), frequency = 12)
US_IP <- data.ts[,"US_IP"]
US_IP_diff <- diff(US_IP, differences = 1)

noholdout1 <- window(US_IP_diff,start=c(1974,1),end=c(2015,12))
holdout1 <- window(US_IP_diff, start=c(2016, 1), end=c(2016, 12))
noholdout2 <- window(US_IP_diff, start=c(1974,1), end=c(2016,12))
holdout2 <- window(US_IP_diff, start=c(2017, 1), end=c(2017, 12))

# Estimate ARMA(7,4) over period 1974:1-2015:12 and forecast over holdout period 2016:1-2016:12 
ARMA74_noholdout1 = arima(noholdout1, order=c(7,0,4), method = "CSS-ML", include.mean = TRUE)
Fcast_ARMA74a <- forecast(ARMA74_noholdout1,h=12)                                
accuracy(Fcast_ARMA74a,holdout1) 

# Plot forecast + 80 and 95 % prediction interval
pdf(".\\visuals\\Fcast_ARMA74a_CI.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par()
plot(Fcast_ARMA74a, main="forecast ARMA(7,4) 2016", col="black")
lines(US_IP_diff)
abline(h=c(ARMA74_noholdout1$coef[5]),col="black",lty=2)       #Draws a horizontal line at the coordinates of the intercept of this model (the fifth coefficient of an MA(4) model
dev.off()   

# Estimate ARMA(7,4) over period 1974:1-2016:12 and forecast over holdout period 2017:1-2017:12 
ARMA74_noholdout2 = arima(noholdout2, order=c(7,0,4), method = "CSS-ML", include.mean = TRUE)
Fcast_ARMA74b <- forecast(ARMA74_noholdout2,h=12) 
accuracy(Fcast_ARMA74b,holdout2) 

# Plot forecast + 80 and 95 % prediction interval
pdf(".\\visuals\\Fcast_ARMA74b_CI.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par()
plot(Fcast_ARMA74b, main="forecast ARMA(7,4) 2017")
lines(US_IP_diff)
abline(h=c(ARMA74_noholdout2$coef[5]),col="black",lty=2)       #Draws a horizontal line at the coordinates of the intercept of this model (the fifth coefficient of an MA(4) model
dev.off()  

#QUESTION 7

#acf of the whole series
pdf(file=".\\visuals\\acf_diff_full.pdf")
US_IP_diff_acf <- acf(US_IP_diff)
#par(mar = c(2,2,0,0)) 
plot(US_IP_diff_acf,main = "ACF US IP diff", ylab = "Autocorrelation", xlab = "Lag", lwd = 7.5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")
dev.off()

estimate_p_DF <- nrow(data.ts)^(1/3)
estimate_p_DF
    
AR8 <- arima(US_IP_diff, order = c(8,0,0), include.mean = TRUE)

AR8_no_mean
LjungBox(AR8$residuals, 25, 8)

US_IP_diff_trend_drift <- ur.df(US_IP_diff, lags = 7, type = "trend")
summary(US_IP_diff_trend_drift)

#Question 8

pdf(file=".\\visuals\\acf_lnYt_full.pdf")
US_IP_acf <- acf(US_IP)
#par(mar = c(2,2,0,0)) 
plot(US_IP_acf,main = "ACF US IP", ylab = "Autocorrelation", xlab = "Lag", lwd = 7.5, mar = c(2,2,0,0), col = "darkblue", bty = 'l', col.axis = "black")
dev.off()

AR8 <- arima(US_IP, order = c(8,0,0), include.mean = T)
LjungBox(AR8$residuals, 25, 8)

US_IP_trend_drift <- ur.df(US_IP, lags = 7, type="trend")
summary(US_IP_trend_drift)

US_IP_drift <- ur.df(US_IP, lags = 7, type="drift")
summary(US_IP_drift)

US_IP_none <- ur.df(US_IP, lags = 7, type = "none")
summary(US_IP_none)

AR5_GDP_diff = arima(US_IP_diff, order = c(8,0,0), include.mean = TRUE)
stargazer(AR5_GDP_diff,type = "text", omit.table.layout = "n", intercept.bottom = FALSE, digits = 4, digits.extra = 0, 
          df = T, report="vcs*t", dep.var.labels = "D(GDP)", omit.stat = c("aic") )







