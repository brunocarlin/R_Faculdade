library(forecast)
library(forecastHybrid)
library(tidyverse)
library(tsibble)
library(Mcomp)
library(rlist)


SubsetM3 <- list.filter(M3, "MONTHLY" %in% period & n > 48)

SubsetM31 <- list.filter(M3, "MONTHLY" %in% period & n > 48 & "N1402" %in% sn)

y <- SubsetM31$N1402$x

h =18

# Use Forecast Functions --------------------------------------------------


Auto1 <- function(x,h){
  forecast(auto.arima(x), h=h)
}

Auto2 <- function(x,h){
  forecast(tbats(x), h=h)
}

Auto3 <- function(x,h){
  forecast(ets(x), h=h)
}


# Create List with Functions ----------------------------------------------


AutoFunctiona <- c(Auto1,Auto2,Auto3)


# Cross Calculate for h and save error ------------------------------------
MatrixErrors1 <- tsCV(TimeSeriesX,Auto1, h =18, window = 18)
MatrixErrors2 <- tsCV(TimeSeriesX,Auto2, h =18, window = 18)

MatrixErrors <- as.tsibble(MatrixErrors)


# Calculate Accuracy per h ------------------------------------------------


# Create an object  with the Time Series, CV Accuracy, Forecast   --------
MatrixCalculateErrors1 <- lapply(MatrixErrors1, function(x,y) mase(x,TimeSeriesX,d = 1,D = 1))
MatrixCalculateErrors2 <- sapply(MatrixErrors2, function(x,y) accuracy(x,TimeSeriesX))

percenterror<-100*MatrixErrors/y

MatrixCalculateErrors1[5,]
Matrix3 <- as.tibble(MatrixErrors2)

#"ME,RMSE,MAE,MPE,MAPE,MASE,ACF1"
MAPE1<- MatrixCalculateErrors1[5,]
MAPE2 <- MatrixCalculateErrors1[5,]

MAPE1
MAPE2

Combinations <- rbind(Auto1 = MAPE1, Auto2 = MAPE2)
CombinationsInv <- 1/Combinations
CombinationsInv <- CombinationsInv/sum(CombinationsInv)
# Compute combination weights
combinations <- sweep(CombinationsInv, 2, colSums(CombinationsInv), FUN = "/")

inverseErrors <- 1/combinations

modelResults<- inverseErrors/sum(inverseErrors)
# Compute combinations of forecasts
return(combinations %*% fcasts)


fcasts <- rbind(
  A = auto.arima(y, h)$mean,
  T = thetaf(y, h)$mean)
