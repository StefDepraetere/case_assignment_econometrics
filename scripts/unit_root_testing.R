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
library(urca)
library(stargazer)
library(xtable)
library(knitr)
library(tools)
library(rmarkdown)
source("TimeSeriesFunctions_v3.R")
install.packages(ur)

output="C:\\Users\\Tim\\Documents\\school\\econometrics_time\\first_assignment\\case_assignment_econometrics"
#read and collect right data
data <- read.csv(("data.csv"), header=TRUE, sep = ",")

data.ts <- ts(data, start = c(1974, 1), end = c(2017, 12), frequency = 12)
US_IP <- data.ts[,"US_IP"]
US_IP_diff <- diff(US_IP, differences = 1)

#our chosen arma model is ARMA(7,4)
# T^(1/3) ~= 8
# we can see that our quick rule suggests one more p parameter
#this is in line with our own model who has 7 p parameters but on 
# top also has 4 q parameters
#the residuals are not correlated anymore

resAR8 <- arima(US_IP_diff,order=c(8,0,0), method = "CSS-ML", include.mean = TRUE)$residuals

QstatDiff = LjungBox(resAR8,50,8)
stargazer(QstatDiff,type = "text", summary = FALSE)

#we can take ADF(7), so we include 7 lags
#we look at dynamic testing
#start with full model

ADF_USIP_diff = ur.df(US_IP_diff, lags=7, type="trend")
summary(ADF_USIP_diff)

#tvalue of gamma is -6.233 which is lower than critical value
#we say with a certain certainty that we do no have a unit root
#this means that we can look at the intercept and the trend parameters
#under the normal hypothesis
#we can conclude that there is no trend and the intercept is zero
#this what we expected because we couldnt see a visual trend and the mean
#was around 0

# Test regression trend 
# 
# 
# Call:
#   lm(formula = z.diff ~ z.lag.1 + 1 + tt + z.diff.lag)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -4.0081 -0.3538 -0.0154  0.3535  2.7614 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.377e-02  6.028e-02   1.556   0.1204    
# z.lag.1     -4.224e-01  6.787e-02  -6.223 1.02e-09 ***
#   tt          -9.027e-05  1.918e-04  -0.471   0.6380    
# z.diff.lag1 -3.332e-01  6.943e-02  -4.799 2.10e-06 ***
#   z.diff.lag2 -1.749e-01  6.868e-02  -2.546   0.0112 *  
#   z.diff.lag3 -3.950e-03  6.631e-02  -0.060   0.9525    
# z.diff.lag4  6.115e-02  6.445e-02   0.949   0.3432    
# z.diff.lag5 -2.387e-02  6.161e-02  -0.388   0.6985    
# z.diff.lag6  1.247e-02  5.570e-02   0.224   0.8229    
# z.diff.lag7 -5.078e-02  4.434e-02  -1.145   0.2526    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.6528 on 509 degrees of freedom
# Multiple R-squared:  0.3817,	Adjusted R-squared:  0.3708 
# F-statistic: 34.92 on 9 and 509 DF,  p-value: < 2.2e-16
# 
# 
# Value of test-statistic is: -6.2233 12.916 19.3652 
# 
# Critical values for test statistics: 
#   1pct  5pct 10pct
# tau3 -3.96 -3.41 -3.12
# phi2  6.09  4.68  4.03
# phi3  8.27  6.25  5.34

resAR8_no_diff <- arima(US_IP,order=c(8,0,0), method = "CSS-ML", include.mean = TRUE)$residuals

QstatDiff = LjungBox(resAR8_no_diff,50,8)
stargazer(QstatDiff,type = "text", summary = FALSE)

ADF_USIP = ur.df(US_IP, lags=7, type="trend")
summary(ADF_USIP)

#tvalue of gamma is -2.066 this is not below crit value of -3.41
#phi 3 is not above the critical value of 6.25 so we dont conclude
#this in our ADF

ADF_USIP_1 = ur.df(US_IP, lags=7, type = "drift")
summary(ADF_USIP_1)

#gamma value is not signigicant different from zero
#we look at phi1, we can suggest that intercept is zero and gamma zero
#we estimate without intercept

ADF_USIP_2 = ur.df(US_IP, lags=7, type = "none")
summary(ADF_USIP_2)













