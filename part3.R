## Install required packages (code is only executed if the packages are not yet installed)
if(!require(tseries)){install.packages("tseries")}
if(!require(urca)){install.packages("urca")}
if(!require(knitr)){install.packages("knitr")}
if(!require(tools)){install.packages("tools")}
if(!require(stargazer)){install.packages("stargazer")}
if(!require(xtable)){install.packages("xtable")}
if(!require(rmarkdown)){install.packages("rmarkdown")}
if(!require(vars)){install.packages("vars")}
if(!require(VAR.etp)){install.packages("VAR.etp")}

## Load required packages
library(urca) 
library(stargazer)
library(xtable)
library(knitr)
library(tools)
library(rmarkdown)
library(vars)
library(VAR.etp)
source(".\\scripts\\TimeSeriesFunctions_V3.R")

data <- read.csv((".\\data.csv"), header=TRUE, sep = ",")
data.ts <- ts(data, start = c(1974, 1), end = c(2017, 12), frequency = 12)

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

#Question 1
question_1_data = cbind(US_IP, OIL_P, OIL_PROD, OIL_STOCKS, WORLD_IP, US_CPI)
colnames(question_1_data) = c("US_IP","OIL_P","OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_CPI")

#all variables lag length 12
VAR_12 = VAR(question_1_data, p = 12, type = "const")

#check lag length
lagSelect = VARselect(question_1_data, lag.max = 36, type = "const")
stargazer(lagSelect$criteria, type = "text")

#all variables lag length 2
VAR_2 <- VAR(question_1_data, p=2, type="const")

#residuals
VAR12_residuals <- resid(VAR_12)
acf12_US_IP = acf(VAR12_residuals[,1])
pacf12_US_IP = pacf(VAR12_residuals[,1])
acf12_OIL_P = acf(VAR12_residuals[,2])
pacf12_OIL_P = pacf(VAR12_residuals[,2])
acf12_OIL_PROD = acf(VAR12_residuals[,3])
pacf12_OIL_PROD = pacf(VAR12_residuals[,3])
acf12_OIL_STOCKS = acf(VAR12_residuals[,4])
pacf12_OIL_STOCKS = pacf(VAR12_residuals[,4])
acf12_WORLD_IP = acf(VAR12_residuals[,5])
pacf12_WORLD_IP = pacf(VAR12_residuals[,5])
acf12_US_CPI = acf(VAR12_residuals[,6])
pacf12_US_CPI = pacf(VAR12_residuals[,6])



VAR2_residuals <- resid(VAR_2)
acf2_US_IP = acf(VAR2_residuals[,1])
pacf2_US_IP = pacf(VAR2_residuals[,1])
acf2_OIL_P = acf(VAR2_residuals[,2])
pacf2_OIL_P = pacf(VAR2_residuals[,2])
acf2_OIL_PROD = acf(VAR2_residuals[,3])
pacf2_OIL_PROD = pacf(VAR2_residuals[,3])
acf2_OIL_STOCKS = acf(VAR2_residuals[,4])
pacf2_OIL_STOCKS = pacf(VAR2_residuals[,4])
acf2_WORLD_IP = acf(VAR2_residuals[,5])
pacf2_WORLD_IP = pacf(VAR2_residuals[,5])
acf2_US_CPI = acf(VAR2_residuals[,6])
pacf2_US_CPI = pacf(VAR2_residuals[,6])

#cumsum
var12.stab = stability(VAR_12, type = "OLS-CUSUM")
var2.stab = stability(VAR_2, type = "OLS-CUSUM")

pdf(file=".\\visuals\\var_12_acf.pdf")
par(mfrow = c(2, 3))
plot(acf12_US_IP, main="acf US_IP")
plot(acf12_OIL_P, main="acf OIL_P")
plot(acf12_OIL_PROD, main="acf OIL_PROD")
plot(acf12_OIL_STOCKS, main="acf OIL_STOCKS")
plot(acf12_WORLD_IP, main="acf WORLD_IP")
plot(acf12_US_CPI, main="acf US_CPI")
dev.off()

pdf(file=".\\visuals\\var_12_pacf.pdf")
par(mfrow = c(2, 3))
plot(pacf12_US_IP, main="pacf US_IP")
plot(pacf12_OIL_P, main="pacf OIL_P")
plot(pacf12_OIL_PROD, main="pacf OIL_PROD")
plot(pacf12_OIL_STOCKS, main="pacf OIL_STOCKS")
plot(pacf12_WORLD_IP, main="pacf WORLD_IP")
plot(pacf12_US_CPI, main="pacf US_CPI")
dev.off()

pdf(file=".\\visuals\\var_2_acf.pdf")
par(mfrow = c(2, 3))
plot(acf2_US_IP, main="acf US_IP")
plot(acf2_OIL_P, main="acf OIL_P")
plot(acf2_OIL_PROD, main="acf OIL_PROD")
plot(acf2_OIL_STOCKS, main="acf OIL_STOCKS")
plot(acf2_WORLD_IP, main="acf WORLD_IP")
plot(acf2_US_CPI, main="acf US_CPI")
dev.off()

pdf(file=".\\visuals\\var_2_pacf.pdf")
par(mfrow = c(2, 3))
plot(pacf2_US_IP, main="pacf US_IP")
plot(pacf2_OIL_P, main="pacf OIL_P")
plot(pacf2_OIL_PROD, main="pacf OIL_PROD")
plot(pacf2_OIL_STOCKS, main="pacf OIL_STOCKS")
plot(pacf2_WORLD_IP, main="pacf WORLD_IP")
plot(pacf2_US_CPI, main="pacf US_CPI")
dev.off()

residDiag12 = serialcorrelation(VARmodel = VAR_12, nlag = 50)
stargazer(residDiag12,type = "text", summary = FALSE, flip = FALSE, rownames = FALSE)

pdf(file=".\\visuals\\var_2_resid.pdf")
plot(acf2)
plot(pacf2)
plot(var2.stab,alpha = 0.05)
dev.off()

residDiag2 = serialcorrelation(VARmodel = VAR_2, nlag = 50)
stargazer(residDiag2,type = "text", summary = FALSE, flip = FALSE, rownames = FALSE)

#Question 2
jointGranger_oil_price= causality(VAR_12, cause = "OIL_P")$Granger
jointGranger_oil_price

jointGranger_on = causality(VAR_12, cause = c("US_IP","OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_CPI"))$Granger
jointGranger_on

#Question 3
summary(VAR_12)$corres

question3_ord = cbind(OIL_P, OIL_PROD, OIL_STOCKS, WORLD_IP, US_IP, US_CPI)
VARord = VAR(question3_ord, p=12, type='const')
IRFord = irf(VARord, n.ahead = 20, boot='TRUE', runs=100)

IRFord_OIL_PROD <- irf(VARord, impulse = "OIL_PROD",n.ahead = 20)
IRFord_OIL_P  <- irf(VARord, impulse = "OIL_P", n.ahead = 20)
IRFord_OIL_STOCKS <- irf(VARord, impulse = "OIL_STOCKS", n.ahead = 20)
IRFord_WORLD_IP <- irf(VARord, impulse = "WORLD_IP",n.ahead = 20)
IRFord_US_IP <- irf(VARord, impulse = "US_IP", n.ahead = 20)
IRFord_US_CPI <- irf(VARord, impulse = "US_CPI", n.ahead = 20)

horizons = 40
IRF_OIL_PROD_1  <- irf(VARord, impulse = "OIL_PROD",response = "OIL_PROD", n.ahead = horizons)
IRF_OIL_PROD_2  <- irf(VARord, impulse = "OIL_PROD",response = "OIL_P", n.ahead = horizons)
IRF_OIL_PROD_3  <- irf(VARord, impulse = "OIL_PROD",response = "OIL_STOCKS", n.ahead = horizons)
IRF_OIL_PROD_4  <- irf(VARord, impulse = "OIL_PROD",response = "WORLD_IP", n.ahead = horizons)
IRF_OIL_PROD_5  <- irf(VARord, impulse = "OIL_PROD",response = "US_IP", n.ahead = horizons)
IRF_OIL_PROD_6  <- irf(VARord, impulse = "OIL_PROD",response = "US_CPI", n.ahead = horizons)

IRF_OIL_P_1  <- irf(VARord, impulse = "OIL_P",response = "OIL_PROD", n.ahead = horizons)
IRF_OIL_P_2  <- irf(VARord, impulse = "OIL_P",response = "OIL_P", n.ahead = horizons)
IRF_OIL_P_3  <- irf(VARord, impulse = "OIL_P",response = "OIL_STOCKS", n.ahead = horizons)
IRF_OIL_P_4  <- irf(VARord, impulse = "OIL_P",response = "WORLD_IP", n.ahead = horizons)
IRF_OIL_P_5  <- irf(VARord, impulse = "OIL_P",response = "US_IP", n.ahead = horizons)
IRF_OIL_P_6  <- irf(VARord, impulse = "OIL_P",response = "US_CPI", n.ahead = horizons)

IRF_OIL_STOCKS_1  <- irf(VARord, impulse = "OIL_STOCKS",response = "OIL_PROD", n.ahead = horizons)
IRF_OIL_STOCKS_2  <- irf(VARord, impulse = "OIL_STOCKS",response = "OIL_P", n.ahead = horizons)
IRF_OIL_STOCKS_3  <- irf(VARord, impulse = "OIL_STOCKS",response = "OIL_STOCKS", n.ahead = horizons)
IRF_OIL_STOCKS_4  <- irf(VARord, impulse = "OIL_STOCKS",response = "WORLD_IP", n.ahead = horizons)
IRF_OIL_STOCKS_5  <- irf(VARord, impulse = "OIL_STOCKS",response = "US_IP", n.ahead = horizons)
IRF_OIL_STOCKS_6  <- irf(VARord, impulse = "OIL_STOCKS",response = "US_CPI", n.ahead = horizons)

IRF_WORLD_IP_1  <- irf(VARord, impulse = "WORLD_IP",response = "OIL_PROD", n.ahead = horizons)
IRF_WORLD_IP_2  <- irf(VARord, impulse = "WORLD_IP",response = "OIL_P", n.ahead = horizons)
IRF_WORLD_IP_3  <- irf(VARord, impulse = "WORLD_IP",response = "OIL_STOCKS", n.ahead = horizons)
IRF_WORLD_IP_4  <- irf(VARord, impulse = "WORLD_IP",response = "WORLD_IP", n.ahead = horizons)
IRF_WORLD_IP_5  <- irf(VARord, impulse = "WORLD_IP",response = "US_IP", n.ahead = horizons)
IRF_WORLD_IP_6  <- irf(VARord, impulse = "WORLD_IP",response = "US_CPI", n.ahead = horizons)

IRF_US_IP_1  <- irf(VARord, impulse = "US_IP",response = "OIL_PROD", n.ahead = horizons)
IRF_US_IP_2  <- irf(VARord, impulse = "US_IP",response = "OIL_P", n.ahead = horizons)
IRF_US_IP_3  <- irf(VARord, impulse = "US_IP",response = "OIL_STOCKS", n.ahead = horizons)
IRF_US_IP_4  <- irf(VARord, impulse = "US_IP",response = "WORLD_IP", n.ahead = horizons)
IRF_US_IP_5  <- irf(VARord, impulse = "US_IP",response = "US_IP", n.ahead = horizons)
IRF_US_IP_6  <- irf(VARord, impulse = "US_IP",response = "US_CPI", n.ahead = horizons)

IRF_US_CPI_1  <- irf(VARord, impulse = "US_CPI",response = "OIL_PROD", n.ahead = horizons)
IRF_US_CPI_2  <- irf(VARord, impulse = "US_CPI",response = "OIL_P", n.ahead = horizons)
IRF_US_CPI_3  <- irf(VARord, impulse = "US_CPI",response = "OIL_STOCKS", n.ahead = horizons)
IRF_US_CPI_4  <- irf(VARord, impulse = "US_CPI",response = "WORLD_IP", n.ahead = horizons)
IRF_US_CPI_5  <- irf(VARord, impulse = "US_CPI",response = "US_IP", n.ahead = horizons)
IRF_US_CPI_6  <- irf(VARord, impulse = "US_CPI",response = "US_CPI", n.ahead = horizons)

pdf(file=".\\visuals\\irf_OIL_PROD.pdf") # Adjust the layout as needed
plot(IRF_OIL_PROD_1)
plot(IRF_OIL_PROD_2)
plot(IRF_OIL_PROD_3)
plot(IRF_OIL_PROD_4)
plot(IRF_OIL_PROD_5)
plot(IRF_OIL_PROD_6)
dev.off()

pdf(file=".\\visuals\\irf_OIL_P.pdf")
plot(IRF_OIL_P_1)
plot(IRF_OIL_P_2)
plot(IRF_OIL_P_3)
plot(IRF_OIL_P_4)
plot(IRF_OIL_P_5)
plot(IRF_OIL_P_6)
dev.off()

pdf(file=".\\visuals\\irf_OIL_STOCKS.pdf")
plot(IRF_OIL_STOCKS_1)
plot(IRF_OIL_STOCKS_2)
plot(IRF_OIL_STOCKS_3)
plot(IRF_OIL_STOCKS_4)
plot(IRF_OIL_STOCKS_5)
plot(IRF_OIL_STOCKS_6)
dev.off()

pdf(file=".\\visuals\\irf_WORLD_IP.pdf")
plot(IRF_WORLD_IP_1)
plot(IRF_WORLD_IP_2)
plot(IRF_WORLD_IP_3)
plot(IRF_WORLD_IP_4)
plot(IRF_WORLD_IP_5)
plot(IRF_WORLD_IP_6)
dev.off()

pdf(file=".\\visuals\\irf_US_IP.pdf")

plot(IRF_US_IP_1)
plot(IRF_US_IP_2)
plot(IRF_US_IP_3)
plot(IRF_US_IP_4)
plot(IRF_US_IP_5)
plot(IRF_US_IP_6)
dev.off()

pdf(file=".\\visuals\\irf_US_CPI.pdf")
plot(IRF_US_CPI_1)
plot(IRF_US_CPI_2)
plot(IRF_US_CPI_3)
plot(IRF_US_CPI_4)
plot(IRF_US_CPI_5)
plot(IRF_US_CPI_6)
dev.off()

VDord1 = fevd(VARord, n.ahead =20)
pdf(".\\visuals\\VDord1.pdf", onefile = T, paper = "A4r",width = 0,height = 0)
plot(VDord1, names = c("OIL_PROD", "OIL_P", "OIL_STOCKS", "WORLD_IP", "US_IP", "US_CPI"), 
     col = gray(seq(0.2, 0.8, length = 6)))
dev.off()

#Question 4
install.packages("rlang")
remove.packages("htmltools")
install.packages("htmltools")
if(!require(vars)){install.packages("vars")}
library(vars)

if(!require(tidyr)){install.packages("tidyr")}
library(tidyr)

if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

if(!require(devtools)){install.packages("devtools")}
library(devtools)

if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)

install_github("martinbaumgaertner/varexternal")
library(varexternal)

## Specify variables included in VAR and instrument

## Specify variables included in VAR and instrument
y <- cbind(data$OIL_P,data$OIL_PROD,data$OIL_STOCKS,data$WORLD_IP, data$US_IP, data$US_CPI) # order instrumented variable first
colnames(y) <- c("OIL_P", "OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_IP", "US_CPI")
z <- cbind(data$Surprises)
colnames(z) <- c("SURPRISES")


## Specify VAR and Impulse Response Function (IRF) options
p           = 12          # Number of lags in the VAR model
NWlags      = 0;          # Newey-West lags (if it is necessary to account for time series autocorrelation)
norm        = 1;          # Variable used for normalization
scale       = 0.2;        # Scale of the shock (determined by the size of the response of the norm variable)
horizons    = 50;         # Number of horizons for the IRFs
confidence  = c(0.95);    # Confidence levels for confidence interval around IRFs

## Estimate VAR using IV method
VAR <- SVARIV(y,data$Surprises,p,confidence,NWlags,norm,scale,horizons,instrument_name="test")

## Plot IRFs
pdf(file=".//visuals//irf_external_instrument.pdf")
sh.col        <- c("#E41A1C") # set color
names(sh.col) <- c("test")
pretty_irf(data=list(VAR$irfs),shock_names="test",pretty_names=c("OIL_P", "OIL_PROD", "OIL_STOCKS", "WORLD_IP", "US_IP", "US_CPI"),manual_color=sh.col)
dev.off()

#question 5
if(!require(lpirfs)){install.packages("lpirfs")}
library(lpirfs)

y_lp <- as.data.frame(y)

## Estimate LP with OLS
LP = lp_lin(y_lp, lags_endog_lin = 12, shock_type = 1, trend = 0, confint = 1.96, hor = 50)
plot(LP) # plotted is the response of each of the four variables to a unit shock in each of the four variables

## Estimate LP with 2SLS estimator (using ff4_tc as instrument for gs1)
shock <- as.data.frame(data$OIL_P) # one-column data.frame with shock variable (instrumented when using 2SLS)
instrum <- as.data.frame(data$Surprises) # instrument used by 2SLS
LP_iv = lp_lin_iv(y_lp,lags_endog_lin=12,shock=shock,instrum=instrum,use_twosls=TRUE,trend=0,confint=1.96,hor=50)
plot(LP_iv) # plotted is the response of each of the four variables to a unit (exogenous) shock in gs1

## Compare IRs from VAR and LP
IR_VAR <- matrix(VAR$irfs$value[1:204],nrow=4)*5
IR_LP  <- LP_iv$irf_lin_mean
plot(IR_LP[1,],type="l",col="red",xlab="", ylab="")
lines(IR_VAR[1,],type="l",col="black")
legend("topright", legend=c("IR from LP", "IR from VAR"), col=c("red", "black"), lty=1)




