######################################################################################################
## Load required packages: this needs to be done every time we close and reopen an R session
######################################################################################################
library(urca)            
library(stargazer)
library(tools)
library(Hmisc)
library(dynlm)
library(lmtest)  
library(vars)
library(VAR.etp)

# library(forecast) CAUTION : only load forecast() when necessary: accuracy() function of forecast clashes with stargazer (you need to detach forecast to solve this)

source("TimeSeriesFunctions.R") # includes a number of additional functions not available online (download from Ufora and save in your input directory)

######################################################################################################
## Import dataset
######################################################################################################
data = read.table("data.csv", header=TRUE, sep=",")


US.IP_vec <- data$US_IP

#question 1
US.IP <- ts(US.IP_vec, start = c(1974, 1), end = c(2017, 12), frequency = 12)


US.IP_change<- diff(US.IP, differences = 1)

plot((US.IP),
     col = 'red',
     xlab = 'time',
     ylab = 'production',
     main='United States Industrial Production')

plot((US.IP_change),
     col = 'blue',
     xlab = 'time',
     ylab = 'production',
     main='United States Industrial Production change')

#question 2
US.IP <- ts(US.IP_vec, start = c(1974, 1), end = c(2015, 12), frequency = 12)

US.IP_change <- diff(US.IP, differences = 1)
acf(US.IP_change)
pacf(US.IP_change)