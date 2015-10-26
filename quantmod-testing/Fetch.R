#!/usr/bin/Rscript

# EXtensible Time Series
library(xts)

# S3 Infrastructure for Regular and Irregular Time Series 
library(zoo)

# Quantitative Financial Modelling Framework
library(quantmod)

# Time Series Analysis and Computational Finance
library(tseries)

# Technical Trading Rules
library(TTR)

QUOTE_NAME <- "AAPL"
START_DATE <- "2010-01-01"
END_DATE   <- "2015-12-31"
TRAINING_END_DATE   <- "2014-12-31"
TRAINING_PERIOD  <- c(START_DATE, TRAINING_END_DATE)

# Fetch market data 
historic.quote.data <- tseries::get.hist.quote(
  QUOTE_NAME,
  start = START_DATE,
  end = END_DATE,
  quote = c("Open", "High", "Low", "Close", "Volume", "AdjClose")
)

# Remove volume since it might not exist
historic.quote.data$Volume <- NULL

# Convert to xts format
quote.data <- xts::as.xts(historic.quote.data)

# Change AdjClose to Adjusted so that quantmod is not confused 
quote.data <- historic.quote.data
colnames(quote.data) <- c("Open", "High", "Low", "Close", "Adjusted")

# Technical trading rules
TA.BB  <- function(x) TTR::BBands(HLC(x))[,'pctB']
TA.ATR <- function(x) TTR::ATR(HLC(x))[,'atr']
TA.SMI <- function(x) TTR::SMI(HLC(x))[,'SMI']
TA.ADX <- function(x) TTR::ADX(HLC(x))[,'ADX']
TA.CMO <- function(x) TTR::CMO(Cl(x))
TA.EMA <- function(x) TTR::EMA(Delt(Cl(x)))
TA.RSI <- function(x) TTR::RSI(Cl(x))
TA.MACD  <- function(x) TTR::MACD(Cl(x))[,2]
TA.runSD <- function(x) TTR::runSD(Cl(x))
TA.runMean <- function(x) TTR::runMean(Cl(x))

model = quantmod::specifyModel(
  quantmod::Next(quantmod::OpCl(quote.data)) ~ 
    quantmod::OpCl(quote.data) + 
    TA.BB(quote.data)  +
    TA.ATR(quote.data) +
    TA.SMI(quote.data) +
    TA.ADX(quote.data) +
    TA.CMO(quote.data) +
    TA.EMA(quote.data) +
    TA.RSI(quote.data) +
    TA.MACD(quote.data) +
    TA.runMean(quote.data) +
    TA.runSD(quote.data)
)

built.model <- buildModel(
  model, 
  training.per = TRAINING_PERIOD, 
  size = 10, 
  decay = 0.01, 
  maxit = 1000, 
  linout = T, 
  trace = F,
  method = 'nnet' 
)

print(quantmod::fittedModel(built.model))
print(quantmod::tradeModel(built.model))