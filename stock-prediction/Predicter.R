#!/usr/bin/Rscript

library(tseries, quietly = T)
library(forecast, quietly = T)
library(quantmod, quietly = T)
library(lubridate, quietly = T)
library(tcltk, quietly = T)  

QUOTE_NAME <- "^OMX"

QUOTE_START_DATE <- "2015-01-01"
QUOTE_END_DATE   <- "2015-10-16"

# Fetch market data 
historic.quote.data <- tseries::get.hist.quote(
  QUOTE_NAME,
  start = QUOTE_START_DATE,
  end = QUOTE_END_DATE,
  quote = c("Open", "High", "Low", "Close", "Volume", "AdjClose")
)

# Remove volume since it might not exist
historic.quote.data$Volume <- NULL

# Convert to xts format
quote.data.xts <- xts::as.xts(historic.quote.data)

# Change AdjClose to Adjusted so that quantmod is not confused 
colnames(quote.data.xts) <- c("Open", "High", "Low", "Close", 
  "Adjusted")

# Evaluation data
#SLIDING_WINDOW <- 10
#SLIDING_WINDOW <- 20
SLIDING_WINDOW <- 30
#SLIDING_WINDOW <- 50

HIDDEN_NODES <- 10

# All dates except sliding window
start.dates <- index(
    head(quote.data.xts, ndays(quote.data.xts) - SLIDING_WINDOW)
  )

# All dates from sliding window
end.dates <- index(
    tail(quote.data.xts, ndays(quote.data.xts) - SLIDING_WINDOW)
  )

if (length(start.dates) != length(end.dates)) {
  stop("Sliding window will not work with these dates")
}
days <- length(start.dates)

forecast.data <- c()
for (i in 1:days) {
  start.date <- start.dates[i]
  end.date <- end.dates[i]
  print("Training dates:")
  training.period <- paste(c(start.date, end.date), collapse="/")
  print(training.period)
  training.data.xts <- quote.data.xts[training.period,]
  nn.forecast <- forecast::nnetar(
      quantmod::Cl(training.data.xts),
      p=1,
      size=HIDDEN_NODES
    )
  fcast <- forecast::forecast(nn.forecast, h=1)
  forecast.data <- c(forecast.data, fcast$mean)
}

print("Data:")
cat(forecast.data)
cat(length(forecast.data))

print("")
print("Next:")

# We have forecasted one more than we have actual data on
next.forecast.point <- tail(forecast.data, 1)
forecast.data <- head(forecast.data, length(forecast.data)-1)

print(next.forecast.point)

next.week.day <- function(date) {
  date <- lubridate::ymd(date)
  day  <- lubridate::wday(date)
  while (day != 6 && day != 7) {
    date <- date + lubridate::days(1)
    day  <- lubridate::wday(date)
  }
  return (date)
}

next.forecast.point.xts <- as.xts(
    read.zoo(
      data.frame(
        Date  = next.week.day(index(tail(quote.data.xts, 1))),
        Close = next.forecast.point
      )
    )
  )

# End.dates[2] is the first predicted date
predicted.period <- paste(end.dates[2], "/", sep="")

# This will be the actual data to test prediction on
evaluation.data.xts <- quote.data.xts[predicted.period]

# For every date in evaluation.data.xts this will add 
# a Forecast column
data <- transform(evaluation.data.xts, Forecast=forecast.data)

# This will append the last predicted point all other points
# having one more date then actual evaluation data
foo <- merge(Cl(quote.data.xts), data$Forecast)
foo[index(data$Forecast)] <- data$Forecast
foo <- c(next.forecast.point.xts, Cl(foo))

# Renaming so that chart series print correct legend 
OMXS30 <- Cl(quote.data.xts)
Forecast <- foo
OMXS30Evaluated <- quantmod::Cl(data)

# Print chart
x11()
quantmod::chartSeries(
  Forecast,
  theme = chartTheme("white", up.col='blue'),
  name  = paste(
    c('Predicting OMXS30 with a ', 
      SLIDING_WINDOW, 
      ' days sliding window'
    ), 
    collapse=''),
  legend = 'OMXS30', 
  TA = c(
    addTA(OMXS30, on=1, col="black"),
    addTA(OMXS30Evaluated, on=1, col="red")
  )
) 

next.week.day(index(tail(Forecast, 1)))


actual.values <- as.data.frame(quantmod::Cl(evaluation.data.xts))[,]

# Residual sum of squares
ssr <- sum((actual.values - forecast.data)^2) 
mse <- ssr / length(actual.values)
rmse <- sqrt(mse)
nrmse <- sqrt(mse) / (
    max(max(actual.values), max(forecast.data)) 
    - 
    min(min(actual.values), min(forecast.data))
  )
accuracy.measures <- forecast::accuracy(forecast.data, 
  quantmod::Cl(evaluation.data.xts))

# SSR 
# ME: Mean Error
# RMSE: Root Mean Squared Error
# MAE: Mean Absolute Error
# MPE: Mean Percentage Error
# MAPE: Mean Absolute Percentage Error
print(accuracy.measures)

# SSR
print("SSR")
print(ssr)

prompt  <- "close"
capture <- tk_messageBox(message = prompt)