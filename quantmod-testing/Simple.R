#!/usr/bin/Rscript

# Quantitative Financial Modelling Framework
library(quantmod)

# Technical Trading Rules
library(TTR)

START_DATE <- "2014-01-01"
TRAINING_END_DATE <- "2014-12-31"
TRAINING_PERIOD   <- c(START_DATE, TRAINING_END_DATE)

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

built.model <- quantmod::buildModel(
  model,
  training.per = TRAINING_PERIOD,
  size = 10, 
  decay = 0.01, 
  maxit = 1000, 
  trace = F,
  linout = T, 
  method = 'nnet'
)
print(quantmod::fittedModel(built.model))
print(quantmod::tradeModel(built.model))