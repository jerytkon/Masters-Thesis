library("rjson")
library(ggplot2)
library(urca)
library(stats)
library(dynlm)
library(lmtest)
library(sandwich)
library(vars)
library(tseries)
library(splitstackshape)
library(tidyverse)
library(zoo)
library(readxl)
library(egcm)
library(dplyr)
library(ecm)
library(TSA)


TIMEPERIOD = c(1993:2018)

#DATA
ABMI_290920 <- read_excel("./ABMI-290920.xls", col_types = c("numeric", "numeric"))
gdp <- ABMI_290920[ which(ABMI_290920$period %in% TIMEPERIOD), ]
gdp
series_290920 <- read_excel("./series-301020.xls", col_types = c("numeric", "numeric"))
cpi <- series_290920[ which(series_290920$period %in% TIMEPERIOD), ]
cpi


s1 <- get.Comtrade(r="826", p="372", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s2 <- get.Comtrade(r="826", p="528", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s3 <- get.Comtrade(r="826", p="0", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")

s4 <- get.Comtrade(r="826", p="616", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s5 <- get.Comtrade(r="826", p="276", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s6 <- get.Comtrade(r="826", p="381", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s7 <- get.Comtrade(r="826", p="40", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")

s8 <- get.Comtrade(r="826", p="251", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s9 <- get.Comtrade(r="826", p="620", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s10 <- get.Comtrade(r="826", p="208", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s11 <- get.Comtrade(r="826", p="56", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")

s12 <- get.Comtrade(r="826", p="76", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s13 <- get.Comtrade(r="826", p="32", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")
s14 <- get.Comtrade(r="826", p="858", ps="All", freq="A", rg="1", px="HS", cc="0201, 0202")

df <- s1$data
IRE <- df
df1 <- s2$data
NL <- df1
df2 <- s3$data
RoW <- df2

df3 <- s4$data
PL <- df3
df4 <- s5$data
GER <- df4
df5 <- s6$data
ITA <- df5
df6 <- s7$data
AUS <- df6

df7 <- s8$data
FRA <- df7
df8 <- s9$data
POR <- df8 
df9 <- s10$data
DEN <- df9
df10 <- s11$data
BEL <- df10

df11 <- s12$data
BRA <- df11 
df12 <- s13$data
AR <- df12
df13 <- s14$data
URU <- df13

#IRE
IRE <- IRE[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
IRE$NetWeight
IRE$NetWeight <- as.numeric(IRE$NetWeight)
IRE$TradeValue <- as.numeric(IRE$TradeValue)
IRE <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = IRE, sum)
IRE$NetWeight
IRE <- IRE[ which(IRE$period %in% TIMEPERIOD), ]
IRE <- merge(IRE, gdp, by = "period")
IRE <- merge(IRE, cpi, by = "period")
IRE$unitprice <- (IRE$TradeValue / IRE$NetWeight) / (IRE$CPI /100)


#NL
NL <- NL[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
NL$NetWeight <- as.numeric(NL$NetWeight)
NL$TradeValue <- as.numeric(NL$TradeValue)
NL <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = NL, sum)
NL <- NL[ which(NL$period %in% TIMEPERIOD), ]
NL <- merge(NL, gdp, by = "period")
NL <- merge(NL, cpi, by = "period")
NL$unitprice <- (NL$TradeValue / NL$NetWeight) / (NL$CPI /100)

RoW <- RoW[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
RoW$NetWeight <- as.numeric(RoW$NetWeight)
RoW$TradeValue <- as.numeric(RoW$TradeValue)
RoW <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = RoW, sum)
RoW <- RoW[ which(RoW$period %in% TIMEPERIOD), ]
RoW <- merge(RoW, gdp, by = "period")
RoW <- merge(RoW, cpi, by = "period")
RoW$unitprice <- (RoW$TradeValue / RoW$NetWeight) / (RoW$CPI /100)

PL <- PL[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
PL$NetWeight <- as.numeric(PL$NetWeight)
PL$TradeValue <- as.numeric(PL$TradeValue)
PL <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = PL, sum)
PL <- PL[ which(PL$period %in% TIMEPERIOD), ]
PL <- merge(PL, gdp, by = "period")
PL <- merge(PL, cpi, by = "period")
PL$unitprice <- (PL$TradeValue / PL$NetWeight) / (PL$CPI /100)

GER <- GER[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
GER$NetWeight <- as.numeric(GER$NetWeight)
GER$TradeValue <- as.numeric(GER$TradeValue)
GER <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = GER, sum)
GER <- GER[ which(GER$period %in% TIMEPERIOD), ]
GER <- merge(GER, gdp, by = "period")
GER <- merge(GER, cpi, by = "period")
GER$unitprice <- (GER$TradeValue / GER$NetWeight) / (GER$CPI /100)

ITA <- ITA[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
ITA$NetWeight <- as.numeric(ITA$NetWeight)
ITA$TradeValue <- as.numeric(ITA$TradeValue)
ITA <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = ITA, sum)
ITA <- ITA[ which(ITA$period %in% TIMEPERIOD), ]
ITA <- merge(ITA, gdp, by = "period")
ITA <- merge(ITA, cpi, by = "period")
ITA$unitprice <- (ITA$TradeValue / ITA$NetWeight) / (ITA$CPI /100)

AUS <- AUS[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
AUS$NetWeight <- as.numeric(AUS$NetWeight)
AUS$TradeValue <- as.numeric(AUS$TradeValue)
AUS <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = AUS, sum)
AUS <- AUS[ which(AUS$period %in% TIMEPERIOD), ]
AUS <- merge(AUS, gdp, by = "period")
AUS <- merge(AUS, cpi, by = "period")
AUS$unitprice <- (AUS$TradeValue / AUS$NetWeight) / (AUS$CPI /100)

FRA <- FRA[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
FRA$NetWeight <- as.numeric(FRA$NetWeight)
FRA$TradeValue <- as.numeric(FRA$TradeValue)
FRA <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = FRA, sum)
FRA <- FRA[ which(FRA$period %in% TIMEPERIOD), ]
FRA <- merge(FRA, gdp, by = "period")
FRA <- merge(FRA, cpi, by = "period")
FRA$unitprice <- (FRA$TradeValue / FRA$NetWeight) / (FRA$CPI /100)

POR <- POR[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
POR$NetWeight <- as.numeric(POR$NetWeight)
POR$TradeValue <- as.numeric(POR$TradeValue)
POR <- aggregate(cbind(TradeValue, NetWeight) ~ period, POR = RoW, sum)
POR <- POR[ which(POR$period %in% TIMEPERIOD), ]
POR <- merge(POR, gdp, by = "period")
POR <- merge(POR, cpi, by = "period")
POR$unitprice <- (POR$TradeValue / POR$NetWeight) / (POR$CPI /100)

DEN <- DEN[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
DEN$NetWeight <- as.numeric(DEN$NetWeight)
DEN$TradeValue <- as.numeric(DEN$TradeValue)
DEN <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = DEN, sum)
DEN <- DEN[ which(DEN$period %in% TIMEPERIOD), ]
DEN <- merge(DEN, gdp, by = "period")
DEN <- merge(DEN, cpi, by = "period")
DEN$unitprice <- (DEN$TradeValue / DEN$NetWeight) / (DEN$CPI /100)

BEL <- BEL[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
BEL$NetWeight <- as.numeric(BEL$NetWeight)
BEL$TradeValue <- as.numeric(BEL$TradeValue)
BEL <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = BEL, sum)
BEL <- BEL[ which(BEL$period %in% TIMEPERIOD), ]
BEL <- merge(BEL, gdp, by = "period")
BEL <- merge(BEL, cpi, by = "period")
BEL$unitprice <- (BEL$TradeValue / BEL$NetWeight) / (BEL$CPI /100)

BRA <- BRA[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
BRA$NetWeight <- as.numeric(BRA$NetWeight)
BRA$TradeValue <- as.numeric(BRA$TradeValue)
BRA <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = BRA, sum)
BRA <- BRA[ which(BRA$period %in% TIMEPERIOD), ]
BRA <- merge(BRA, gdp, by = "period")
BRA <- merge(BRA, cpi, by = "period")
BRA$unitprice <- (BRA$TradeValue / BRA$NetWeight) / (BRA$CPI /100)

AR <- AR[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
AR$NetWeight <- as.numeric(AR$NetWeight)
AR$TradeValue <- as.numeric(AR$TradeValue)
AR <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = AR, sum)
AR <- AR[ which(AR$period %in% TIMEPERIOD), ]
AR <- merge(AR, gdp, by = "period")
AR <- merge(AR, cpi, by = "period")
AR$unitprice <- (AR$TradeValue / AR$NetWeight) / (AR$CPI /100)

URU <- URU[c("period", "rtTitle", "ptTitle", "NetWeight", "TradeValue", "cmdCode")]
URU$NetWeight <- as.numeric(URU$NetWeight)
URU$TradeValue <- as.numeric(URU$TradeValue)
URU <- aggregate(cbind(TradeValue, NetWeight) ~ period, data = URU, sum)
URU <- URU[ which(URU$period %in% TIMEPERIOD), ]
URU <- merge(URU, gdp, by = "period")
URU <- merge(URU, cpi, by = "period")
URU$unitprice <- (URU$TradeValue / URU$NetWeight) / (URU$CPI /100)

