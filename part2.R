## Install required packages (code is only executed if the packages are not yet installed)
if(!require(tseries)){install.packages("tseries")}
if(!require(urca)){install.packages("urca")}
if(!require(knitr)){install.packages("knitr")}
if(!require(tools)){install.packages("tools")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(xtable)){install.packages("xtable")}
if(!require(rmarkdown)){install.packages("rmarkdown")}
if(!require(dynlm)){install.packages("dynlm")}
install.packages("lmtest")

## Load required packages
library(urca)
library(stargazer)
library(xtable)
library(knitr)
library(tools)
library(rmarkdown)
library(dynlm)
library(urca)
library(lmtest)
source(".\\scripts\\TimeSeriesFunctions_v3.R")

output=".\\visuals"

data <- read.csv((".\\data.csv"), header=TRUE, sep = ",")
data.ts <- ts(data, start = c(1974, 1), end = c(2017, 12), frequency = 12)

nrow(data)

colnames(data)

#dependent variable
US_IP <- data.ts[,"US_IP"]
US_IP_diff <- diff(US_IP, differences = 1)

#independent variables
OIL_P <- data.ts[,"OIL_P"]
OIL_P_diff <- diff(OIL_P, differences = 1)

OIL_PROD <- data.ts[,"OIL_PROD"]
OIL_PROD_diff <- diff(OIL_PROD, differences = 1)

OIL_STOCKS <- data.ts[,"OIL_STOCKS"]
OIL_STOCKS_diff <- diff(OIL_STOCKS, differences = 1)

WORLD_IP <- data.ts[,"WORLD_IP"]
WORLD_IP_diff <- diff(WORLD_IP, differences = 1)

US_CPI <- data.ts[,"US_CPI"]
US_CPI_diff <- diff(US_CPI, differences = 1)

pdf(".\\visuals\\levels.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par() 
plot(US_IP,main="variables in levels", type = "l",col="red3", lwd = 0.5, xlab = "T", ylab = NA, las = 1, bty = "l", ylim = c(0,800))
lines(OIL_P, col = "green3", lwd = 0.5)
lines(OIL_PROD, col = "black", lwd = 0.5)
lines(OIL_STOCKS, col = "purple", lwd = 0.5)
lines(WORLD_IP, col = "orange", lwd = 0.5)
lines(US_CPI, col = "blue", lwd = 0.5)
abline(h = seq(6, 13, .5), col = "darkgrey", lty = 2)
legend("bottomright", legend = c("US_IP","OIL_P","OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_CPI"), lty = c(1,1,1), lwd = c(2,2,2)
       ,col = c("red3","green3","black", "purple", "orange", "blue"), horiz = FALSE,cex = 1.5,bty="n")
dev.off()

pdf(".\\visuals\\differences.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par() 
plot(US_IP_diff,main="variables in diferences", type = "l",col="red3", lwd = 0.5, xlab = "T", ylab = NA, las = 1, bty = "l", ylim = c(-10,10))
lines(OIL_P_diff, col = "green3", lwd = 0.5)
lines(OIL_PROD_diff, col = "black", lwd = 0.5)
lines(OIL_STOCKS_diff, col = "purple", lwd = 0.5)
lines(WORLD_IP_diff, col = "orange", lwd = 0.5)
lines(US_CPI_diff, col = "blue", lwd = 0.5)
abline(h = seq(6, 13, .5), col = "darkgrey", lty = 2)
legend("bottomright", legend = c("US_IP","OIL_P","OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_CPI"), lty = c(1,1,1), lwd = c(2,2,2)
       ,col = c("red3","green3","black", "purple", "orange", "blue"), horiz = FALSE,cex = 1.5,bty="n")
dev.off()

#question 1

#ADL(0,0,0,0,0,0)
ADL0 <- dynlm(US_IP_diff ~ OIL_P_diff + OIL_PROD_diff + OIL_STOCKS_diff + WORLD_IP_diff + US_CPI_diff)
stargazer(ADL0, type="text")
LjungBox(ADL0$residuals, 25, 0)

#ADL(1,1,1,1,1,1)
ADL1 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) 
              + OIL_P_diff + L(OIL_P_diff, 1) 
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) 
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) 
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) 
              + US_CPI_diff + L(US_CPI_diff, 1))

stargazer(ADL1, type="text")
LjungBox(ADL1$residuals, 25, 1)

#ADL(2,2,2,2,2,2)
ADL2 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2))

stargazer(ADL2, type="text")
LjungBox(ADL2$residuals, 25, 2)

#ADL(3,3,3,3,3,3)
ADL3 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2) + L(US_IP_diff, 3)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2) + L(OIL_P_diff, 3)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2) + L(OIL_PROD_diff, 3)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2) + L(OIL_STOCKS_diff, 3)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2) + L(WORLD_IP_diff, 3)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2) + L(US_CPI_diff, 3))

stargazer(ADL3, type="text")
LjungBox(ADL3$residuals, 25, 3)

#ADL(3,3,3,3,3,3)
ADL4 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2) + L(US_IP_diff, 3) + L(US_IP_diff, 4)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2) + L(OIL_P_diff, 3) + L(OIL_P_diff, 4)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2) + L(OIL_PROD_diff, 3) + L(OIL_PROD_diff, 4)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2) + L(OIL_STOCKS_diff, 3) + L(OIL_STOCKS_diff, 4)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2) + L(WORLD_IP_diff, 3) + L(WORLD_IP_diff, 4)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2) + L(US_CPI_diff, 3) + L(US_CPI_diff, 4))

stargazer(ADL4, type="text")
LjungBox(ADL4$residuals, 25, 4)

ADL5 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2) + L(US_IP_diff, 3) + L(US_IP_diff, 4) + L(US_IP_diff, 5)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2) + L(OIL_P_diff, 3) + L(OIL_P_diff, 4) + L(OIL_P_diff, 5)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2) + L(OIL_PROD_diff, 3) + L(OIL_PROD_diff, 4) + L(OIL_PROD_diff, 5)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2) + L(OIL_STOCKS_diff, 3) + L(OIL_STOCKS_diff, 4) + L(OIL_STOCKS_diff, 5)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2) + L(WORLD_IP_diff, 3) + L(WORLD_IP_diff, 4) + L(WORLD_IP_diff, 5)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2) + L(US_CPI_diff, 3) + L(US_CPI_diff, 4) + L(US_CPI_diff, 5))

stargazer(ADL5, type="text")
LjungBox(ADL5$residuals, 25, 5)

ADL6 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2) + L(US_IP_diff, 3) + L(US_IP_diff, 4) + L(US_IP_diff, 5) + L(US_IP_diff, 6)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2) + L(OIL_P_diff, 3) + L(OIL_P_diff, 4) + L(OIL_P_diff, 5) + L(OIL_P_diff, 6)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2) + L(OIL_PROD_diff, 3) + L(OIL_PROD_diff, 4) + L(OIL_PROD_diff, 5) + L(OIL_PROD_diff, 6)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2) + L(OIL_STOCKS_diff, 3) + L(OIL_STOCKS_diff, 4) + L(OIL_STOCKS_diff, 5) + L(OIL_STOCKS_diff, 6)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2) + L(WORLD_IP_diff, 3) + L(WORLD_IP_diff, 4) + L(WORLD_IP_diff, 5) + + L(WORLD_IP_diff, 6)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2) + L(US_CPI_diff, 3) + L(US_CPI_diff, 4) + L(US_CPI_diff, 5) + L(US_CPI_diff, 6))

stargazer(ADL6, type="text")
LjungBox(ADL6$residuals, 20, 6)

ADL7 <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + L(US_IP_diff, 2) + L(US_IP_diff, 3) + L(US_IP_diff, 4) + L(US_IP_diff, 5) + L(US_IP_diff, 6) + L(US_IP_diff, 7)
              + OIL_P_diff + L(OIL_P_diff, 1) + L(OIL_P_diff, 2) + L(OIL_P_diff, 3) + L(OIL_P_diff, 4) + L(OIL_P_diff, 5) + L(OIL_P_diff, 6) +  L(OIL_P_diff, 7)
              + OIL_PROD_diff + L(OIL_PROD_diff, 1) + L(OIL_PROD_diff, 2) + L(OIL_PROD_diff, 3) + L(OIL_PROD_diff, 4) + L(OIL_PROD_diff, 5) + L(OIL_PROD_diff, 6) +  L(OIL_PROD_diff, 7)
              + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + L(OIL_STOCKS_diff, 2) + L(OIL_STOCKS_diff, 3) + L(OIL_STOCKS_diff, 4) + L(OIL_STOCKS_diff, 5) + L(OIL_STOCKS_diff, 6) +  L(OIL_STOCKS_diff, 7)
              + WORLD_IP_diff + L(WORLD_IP_diff, 1) + L(WORLD_IP_diff, 2) + L(WORLD_IP_diff, 3) + L(WORLD_IP_diff, 4) + L(WORLD_IP_diff, 5) +  L(WORLD_IP_diff, 6) +  L(WORLD_IP_diff, 7)
              + US_CPI_diff + L(US_CPI_diff, 1) + L(US_CPI_diff, 2) + L(US_CPI_diff, 3) + L(US_CPI_diff, 4) + L(US_CPI_diff, 5) + L(US_CPI_diff, 6) + L(US_CPI_diff, 7))

stargazer(ADL7, type="text")
LjungBox(ADL7$residuals, 20, 7)

#tmp = selectADL(US_IP_diff,OIL_P_diff,OIL_PROD_diff,OIL_STOCKS_diff,WORLD_IP_diff, US_CPI_diff,10,10,10,10,10,10)

#question 2

#check intigration levels of variables
#from part 1 we now that US_IP has a unit root but diff does not so I(1)

#OIL_P
OIL_P_trend_drift <- ur.df(OIL_P, lags = 7, type="trend")
summary(OIL_P_trend_drift)

OIL_P_drift <- ur.df(OIL_P, lags = 7, type="drift")
summary(OIL_P_drift)

OIL_P_none <- ur.df(OIL_P, lags = 7, type="none")
summary(OIL_P_none)

OIL_P_diff_trend_drift <- ur.df(OIL_P_diff, lags = 7, type="trend")
summary(OIL_P_diff_trend_drift)

#OIL_PROD
OIL_PROD_trend_drift <- ur.df(OIL_PROD, lags = 7, type="trend")
summary(OIL_PROD_trend_drift)

OIL_PROD_drift <- ur.df(OIL_PROD, lags = 7, type="drift")
summary(OIL_PROD_drift)

OIL_PROD_none <- ur.df(OIL_PROD, lags = 7, type="none")
summary(OIL_PROD_none)

OIL_PROD_diff_trend_drift <- ur.df(OIL_PROD_diff, lags = 7, type="trend")
summary(OIL_PROD_diff_trend_drift)

#OIL_STOCKS
OIL_STOCKS_trend_drift <- ur.df(OIL_STOCKS, lags = 7, type="trend")
summary(OIL_STOCKS_trend_drift)

OIL_STOCKS_trend <- seq_along(OIL_STOCKS)

xreg_oil <- cbind(OIL_STOCKS_trend)

arima_oil <- arima(OIL_STOCKS, order = c(8, 0, 0), xreg = xreg_oil, include.mean=T)
stargazer(arima_oil, type="text")

OIL_STOCKS_drift <- ur.df(OIL_STOCKS, lags = 7, type="drift")
summary(OIL_STOCKS_drift)

OIL_STOCKS_diff_trend_drift <- ur.df(OIL_STOCKS_diff, lags = 7, type="trend")
summary(OIL_STOCKS_diff_trend_drift)

#WORLD_IP
WORLD_IP_trend_drift <- ur.df(WORLD_IP, lags = 7, type="trend")
summary(WORLD_IP_trend_drift)

#US_CPI
US_CPI_trend_drift <- ur.df(US_CPI, lags=7, type="trend")
summary(US_CPI_trend_drift)

trend_variable <- seq_along(US_CPI)

xreg_matrix <- cbind(trend_variable)

arima_model <- arima(US_CPI, order = c(8, 0, 0), xreg = xreg_matrix, include.mean=T)
stargazer(arima_model, type="text")

#static model
staticModel <- lm(US_IP ~ OIL_P + OIL_PROD + OIL_STOCKS + WORLD_IP + US_CPI)
stargazer(staticModel, type="text")

staticResid  = ts(staticModel$residuals, start = 1974, frequency = 12)
staticFitted = ts(staticModel$fitted.values, start = 1974, frequency = 12)

pdf(".\\visuals\\static_fitted_resid_3.pdf", onefile = T, paper = 'A4r',width = 0,height = 0)
par(mar=c(5,2,2,3))
plot(US_IP, type = "l",col="red3", lwd = 2, xlab = NA, ylab = NA,ylim = c(min(US_IP,staticResid,staticFitted),max(US_IP,staticResid,staticFitted)),
     las = 1, bty = "u")
lines(staticFitted, col = "green3", lwd = 2)
par(new = T)
plot(staticResid, type = "l", axes = F, xlab = NA, ylab = NA, col = "blue3", lwd = 2, lty = 3)
axis(side = 4,las = 1, at = seq(round(min(staticResid),1), round(max(staticResid),1), by = 0.1))
abline(h = 0, col = "darkgrey", lwd = 2)
par(new = T, xpd = NA, mar = c(par()$mar + c(-5,+3,0,0)))
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n",xlab=NA,ylab=NA)
legend("bottom", legend = c("Actual","Fitted","Residuals (right axis)"), lty = c(1,1,3), lwd = c(2,2,2)
       ,col = c("red3","green3","blue3"), horiz = TRUE,cex=0.9,bty="n")
dev.off()

DW_static = dwtest(staticModel)
DW_static

selectARIMA(staticResid, 10, 0, 0)
ARmodel  = arima(staticResid, order = c(4,0,0), include.mean = T)
LjungBox(ARmodel$residuals,25,4)

ADFresid = ur.df(staticResid, lags = 3, type = "none")
summary(ADFresid)

#question 3
ECM_model <- dynlm(US_IP_diff ~ L(US_IP_diff, 1) + OIL_P_diff + L(OIL_P_diff, 1) + OIL_PROD_diff + L(OIL_PROD_diff, 1) 
                   + OIL_STOCKS_diff + L(OIL_STOCKS_diff, 1) + WORLD_IP_diff + L(WORLD_IP_diff, 1) 
                   + US_CPI_diff + L(US_CPI_diff, 1) + L(staticResid,1))
stargazer(ECM_model, type="text")
summary(ECM_model)

#question 4
selectADL(US_IP, OIL_P, OIL_PROD, OIL_STOCKS, WORLD_IP, US_CPI, 3, 3, 3, 3, 3, 3)
#adl(1,0,0,0,2,0)

adl_model_levels <- dynlm(US_IP ~ L(US_IP, 1) + OIL_P + OIL_PROD + OIL_STOCKS + WORLD_IP + L(WORLD_IP, 1) + L(WORLD_IP, 2) + US_CPI)
summary(adl_model_levels)

staticResid  = ts(adl_model_levels$residuals, start = 1974, frequency = 12)
staticFitted = ts(adl_model_levels$fitted.values, start = 1974, frequency = 12)

dwtest(adl_model_levels)

selectARIMA(staticResid, 10, 0, 0)
ARmodel <- arima(staticResid, order = c(8, 0, 0), include.mean = T)
LjungBox(ARmodel$residuals, 25, 8)

ADFresid <- ur.df(staticResid, lags = 7, type="none")
summary(ADFresid)

#question 5

alphainit <- coef(ECM_model)[[13]]
gamma0init <- coef(ECM_model)[[1]]
gamma1init <- coef(ECM_model)[[2]]
gamma2init <- coef(ECM_model)[[3]]
gamma3init <- coef(ECM_model)[[4]]
gamma4init <- coef(ECM_model)[[5]]
gamma5init <- coef(ECM_model)[[6]]
gamma6init <- coef(ECM_model)[[7]]
gamma7init <- coef(ECM_model)[[8]]
gamma8init <- coef(ECM_model)[[9]]
gamma9init <- coef(ECM_model)[[10]]
gamma10init <- coef(ECM_model)[[11]]

beta0init <- coef(staticModel)[[1]]
beta1init <- coef(staticModel)[[2]]
beta2init <- coef(staticModel)[[3]]
beta3init <- coef(staticModel)[[4]]
beta4init <- coef(staticModel)[[5]]
beta5init <- coef(staticModel)[[6]]

US_IP_diff_1    = lag(US_IP_diff,-1)
OIL_P_diff_1    = lag(OIL_P_diff,-1)
OIL_PROD_diff_1    = lag(OIL_PROD_diff,-1)
OIL_STOCKS_diff_1 = lag(OIL_STOCKS_diff,-1)
WORLD_IP_diff_1   = lag(WORLD_IP_diff,-1)
US_CPI_diff_1   = lag(US_CPI_diff,-1)

US_IP_1 = lag(US_IP, -1)
OIL_P_1 = lag(OIL_P, -1)
OIL_PROD_1 = lag(OIL_PROD, -1)
OIL_STOCKS_1 = lag(OIL_STOCKS, -1)
WORLD_IP_1 = lag(WORLD_IP, -1)
US_CPI_1 = lag(US_CPI, -1)


formula <- US_IP_diff ~ gamma0*US_IP_diff_1 + gamma1*OIL_P_diff + gamma2 *OIL_P_diff_1  + gamma3 * OIL_PROD_diff + gamma4 * OIL_PROD_diff_1 + gamma5 * OIL_STOCKS_diff + gamma6 * OIL_STOCKS_diff_1+ gamma7 * WORLD_IP_diff + gamma8 * WORLD_IP_diff_1 + gamma9 * US_CPI_diff + gamma10 * US_CPI_diff_1 + alpha * (US_IP_1 - beta0 - beta1 * OIL_P_1 - beta2 * OIL_PROD_1 - beta3 * OIL_STOCKS_1- beta4 * WORLD_IP_1 - beta5 * US_CPI_1)

ECMdata = data.frame(ts.union(US_IP, OIL_P, OIL_PROD, OIL_STOCKS, WORLD_IP, US_CPI,
                              US_IP_1, OIL_P_1, OIL_PROD_1, OIL_STOCKS_1, WORLD_IP_1, US_CPI_1,
                              US_IP_diff, OIL_P_diff, OIL_PROD_diff, OIL_STOCKS_diff, WORLD_IP_diff, US_CPI_diff,
                              US_IP_diff_1, OIL_P_diff_1, OIL_PROD_diff_1, OIL_STOCKS_diff_1, WORLD_IP_diff_1, US_CPI_diff_1) )

ECMcoint = nls(formula, data = ECMdata, start = list(alpha=alphainit, 
                                                     gamma0 = gamma0init, gamma1 = gamma1init, gamma2=gamma2init,gamma3=gamma3init,gamma4=gamma4init,
                                                     gamma5 = gamma5init, gamma6 = gamma6init, gamma7=gamma7init,gamma8=gamma8init,gamma9=gamma9init,
                                                     gamma10=gamma10init,
                                                     beta0=beta0init, beta1=beta1init, beta2=beta2init,
                                                     beta3=beta3init, beta4=beta4init, beta5=beta5init))

summary(ECMcoint)





















