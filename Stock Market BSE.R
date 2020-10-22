library(lubridate)
library(forecast)
library(zoo)
library(tibbletime)
library(dplyr)
library(xts)
library(DataCombine)
library(plotly)
library(tidyr)
library(tseries)

bseData <- read.csv("C:/Users/harsh/Downloads/BSESN.csv")
#View(bseData)
str(bseData)
bseData$Date <- ymd(bseData$Date)
bseData$Open <- as.numeric(bseData$Open)
bseData$Open <- as.numeric(bseData$Open)
bseData$High <- as.numeric(bseData$High)
bseData$Low <- as.numeric(bseData$Low)
bseData$Close <- as.numeric(bseData$Close)
bseData$Adj.Close <- as.numeric(bseData$Adj.Close)
bseData$Volume <- as.numeric(bseData$Volume)
str(bseData)
sapply(bseData, function(x)sum(is.na(x)))
head(bseData)
tail(bseData)
bseData <- drop_na(bseData)
summary(bseData)
plot(y = bseData$Adj.Close, x=bseData$Date, main = 'Adjusted Close Price of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
plot(y = bseData$Close, x=bseData$Date, main = 'Close Price of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
plot(y = bseData$Open, x=bseData$Date, main = 'Opening Price of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
plot(y = bseData$High, x=bseData$Date, main = 'Highest Price of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
plot(y = bseData$Low, x=bseData$Date, main = 'Lowest Price of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
plot(y = bseData$Volume, x=bseData$Date, main = 'Volume of S&P BSE SENSEX', ylab = 'Price', xlab = 'Date')
fig <- plot_ly(data = bseData, x= bseData$Date, type = 'ohlc', open = bseData$Open, close = bseData$Close, 
               high = bseData$High, low = bseData$Low)
fig <- fig %>% layout(title = 'BSE OHLC Chart')
fig
stocks <- xts(bseData[,c(-1,-8,-9,-10)], order.by = as.Date(bseData[,1],'%y/%m/%d'))
class(stocks)

bseData$bul <- 1
bseData$bul <- ifelse(bseData$Open > bseData$Close, 0, bseData$bul)

bseData$count <- 1
for(i in 2:nrow(bseData)){
  if(bseData[i,]$bul == bseData[i-1,]$bul){
    bseData[i,]$count <- bseData[i-1,]$count+1
  }
}
max(bseData$count)

bseData$nextC <- shift(bseData$count,1)

newTable <- table(bseData$count, bseData$nextC)
percent.table <- prop.table(newTable,1)
print.table({percent.table[percent.table==0] <- NA; percent.table})

bseData$nextOpen <- shift(bseData$Open, 1)
bseData$nextClose <- shift(bseData$Close, 1)

trades <- subset(bseData, bseData$count>=8)
trades$direction <- 1
trades$direction <- ifelse(trades$bul == 1, -1, trades$direction)

bseData$row <- seq(1, nrow(bseData), 1)
bseData$smoothed <- ksmooth(bseData$row, bseData$Close, 'normal', bandwidth = 10)$y
plot(bseData$Close)
lines(bseData$smoothed, col='blue')

peak <- which(diff(diff(bseData$smoothed)>=0)<0)+1
trough <- which(diff(diff(bseData$smoothed)>0)>0)+1
bseData$minmax <- 0
bseData$minmax <- ifelse(bseData$row %in% peak, 1, bseData$minmax)
bseData$minmax <- ifelse(bseData$row %in% trough, -1, bseData$minmax)