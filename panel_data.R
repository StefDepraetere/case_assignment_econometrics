invisible(lapply(paste0('package:', names(sessionInfo()$otherPkgs)), detach, character.only=TRUE, unload=TRUE))
library(readr)
library(plm)
library(lpirfs)

str(data_panel)
#read in data
data_panel <- read_csv("data_panel/DataPanel.csv")

#change to paneldata
panel_data <- pdata.frame(data_panel, index=c("CountryID", "TimeID"))
plotmeans(lnGDP ~ TimeID, data = panel_data,bars = FALSE,
          n.label = FALSE)

#compute economic growth
panel_data$lnGDP_diff<- diff(panel_data$lnGDP)
panel_data$D_diff <- diff(panel_data$D)

#lags
lag(panel_data$lnGDP, 1:4)
panel_data$lnGDP_diff_lags <- lag(panel_data$lnGDP_diff, 1)
panel_data$lnGDP_lag <- lag(panel_data$lnGDP, 1)


#first equation
#lny it = alfa i + beta * Dit + error it

POLS_1 <- plm(lnGDP~D, data=panel_data, model="pooling")
summary(POLS_1)

FE_1 <- plm(lnGDP~D, data=panel_data, model="within")
summary(FE_1)

RE_1 <- plm(lnGDP~D, data=panel_data, model="random")
summary(RE_1)

pFtest(FE_1, POLS_1)

phtest(FE_1, RE_1)

pbgtest(FE_1)

#second equation
POLS_2 <- plm(lnGDP_diff~D, data=panel_data, model="pooling")
summary(POLS_2)

FE_2 <- plm(lnGDP_diff~D, data=panel_data, model="within")
summary(FE_2)

RE_2 <- plm(lnGDP_diff~D, data=panel_data, model="random")
summary(POLS_2)

pFtest(FE_2, POLS_2)

phtest(FE_2,RE_2)

pbgtest(FE_2)

#third equation

#check number of lags to induce no autocorrelation in errors
FE_lags_16 <- plm(lnGDP~D + lag(panel_data$lnGDP, 1:16), data=panel_data, model="within")
summary(FE_lags_16)

pbgtest(FE_lags_16)

POLS_lags_16 <- plm(lnGDP~D+lag(panel_data$lnGDP, 1:16), data=panel_data, model="pooling")
summary(POLS_lags_16)

RE_lags_16 <- plm(lnGDP~D+lag(panel_data$lnGDP,1:16), data=panel_data, model='random')
summary(RE_lags_16)

pFtest(FE_lags_16, POLS_lags_16)

phtest(FE_lags_16, RE_lags_16)

#fourth equation
FE_lags_8 <- plm(lnGDP_diff~D + lag(panel_data$lnGDP_diff, 1:8), data=panel_data, model="within")
pbgtest(FE_lags_8)

summary(FE_lags_8)

POLS_lags_8 <- plm(lnGDP_diff~D + lag(panel_data$lnGDP_diff, 1:8), data=panel_data, model="pooling")
summary(POLS_lags_8)

RE_lags_8 <- plm(lnGDP_diff~D + lag(panel_data$lnGDP_diff, 1:8), data=panel_data, model="random")
summary(RE_lags_8)

#local projection

panel_data$lnGDP_with_lag <- panel_data$lnGDP - lag(panel_data$lnGDP, 1)

local_projection = lp_lin_panel(data_set=panel_data, endog_data= "lnGDP_diff", 
                                cumul_mult=T, shock="D", diff_shock=T,panel_model="within",
                                panel_effect="twoways", confint=1.96, hor=30, 
                                l_fd_exog_data=c("lnGDP_diff"), lags_fd_exog_data=8)
plot(local_projection)
summary(local_projection)
# Use the purtest function for unit root testing


myPanelDataClean = subset(panel_data, CountryID %in% c(1:75,77:89,91:93,95:132,134:175))

myPanelDataUnbal=myPanelDataClean[which(!is.na(myPanelDataClean$lnGDP)),]
myPanelDataBal=make.pbalanced(myPanelDataUnbal,balance.type="shared.individuals")

purtest(myPanelDataBal$lnGDP, test = "madwu")
purtest(myPanelDataBal$lnGDP, test = "levinlin")
purtest(myPanelDataBal$lnGDP, exo="intercept", test = "ips")

pcdtest(panel_data)

myPanelDataUnbal=myPanelDataClean[which(!is.na(myPanelDataClean$lnGDP_diff)),]
myPanelDataBal=make.pbalanced(myPanelDataUnbal,balance.type="shared.individuals")

purtest(myPanelDataBal$lnGDP_diff, test = "madwu")
purtest(myPanelDataBal$lnGDP_diff, test = "levinlin")
purtest(myPanelDataBal$lnGDP_diff, exo="intercept", test = "ips")

myPanelDataUnbal=myPanelDataClean[which(!is.na(myPanelDataClean$D)),]
myPanelDataBal=make.pbalanced(myPanelDataUnbal,balance.type="shared.individuals")


purtest(myPanelDataUnbal$D, test = "madwu")
purtest(myPanelDataBal$D, test = "levinlin")
purtest(myPanelDataBal$D, exo="intercept", test = "ips")

myPanelDataUnbal=myPanelDataClean[which(!is.na(myPanelDataClean$D_diff)),]
myPanelDataBal=make.pbalanced(myPanelDataUnbal,balance.type="shared.individuals")

length(myPanelDataBal$D)
purtest(myPanelDataBal$D, test = "madwu")
purtest(myPanelDataBal$D, test = "levinlin")
purtest(myPanelDataBal$D, exo="intercept", test = "ips")

#cross_sectional dependence test
pcdtest(myPanelDataBal$D)

#cointegration test
FE_1 <- plm(myPanelDataBal$lnGDP~myPanelDataBal$D, data=panel_data, model="within")
res <- FE_1$residuals
res
cleaned_vector <- na.omit(res)
class(res)
purtest(cleaned_vector, test = "madwu")







