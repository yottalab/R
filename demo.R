setwd("C:/Users/ROG/Desktop/R Reseach")
library(data.table)
library(reshape2)
library(ggplot2)
library(scales)
library(plyr)

start_date <- as.Date("2009-10-09")
end_date <- as.Date("2009-11-15")

AAPL <- fread("NASDAQ_AAPL.csv")
GOOG <- fread("NASDAQ_GOOG.csv")

l = list(AAPL,GOOG)
load <- rbindlist(l)

sum <- load[,sum(stock_volume),by=stock_symbol]
sum

count <- load[,':='(COUNT = .N), by = stock_symbol]#[stock_price_close>205]

count <- count[,exchange:=NULL]
count <- count[,stock_price_open:=NULL]
count <- count[,stock_price_adj_close:=NULL]
count <- count[,':='(COUNT = .N), by = stock_symbol]

count <- count[rev(order(as.Date(count$date, format="%d/%m/%Y"))),]
count <- as.data.frame(count)

result <- as.data.frame(as.Date(count$date, format = "%d/%m/%Y"))
names(result)[names(result) == 'as.Date(count$date, format = "%d/%m/%Y")'] <- 'date'
count$date <- result$date
count

result <- subset(count,start_date < result$date)
result
result <- subset(result,date < end_date)
result
result1 <- subset(result,stock_symbol == 'AAPL')
result1
result2 <- subset(result,stock_symbol == 'GOOG')
result2

#ggplot(result1, aes(x = date, y = stock_price_close, group = stock_symbol)) + geom_line() +
#  scale_x_date(labels = date_format("%d-%m-%Y")) +
#  geom_line(mapping = aes(y = stock_price_high), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_low), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_close), lwd = 1.3, colour = "blue") +
#  facet_wrap( ~ stock_symbol)

#ggplot(result2, aes(x = date, y = stock_price_close, group = stock_symbol)) + geom_line() +
#  scale_x_date(labels = date_format("%d-%m-%Y")) +
#  geom_line(mapping = aes(y = stock_price_high), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_low), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_close), lwd = 1.3, colour = "blue") +
#  facet_wrap( ~ stock_symbol)

#ggplot(result, aes(x = date, y = stock_price_close, group = stock_symbol)) + geom_line() +
#  scale_x_date(labels = date_format("%d-%m-%Y")) +
#  geom_line(mapping = aes(y = stock_price_high), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_low), lty = "dashed") +
#  geom_line(mapping = aes(y = stock_price_close), lwd = 1.3, colour = "blue")
#+ facet_wrap( ~ stock_symbol)

result1$MEAN <- ave(((result1$stock_price_low+result1$stock_price_high)/2), result1$date)
result1
#close vs mean
result1$F1 <- ave(((result1$stock_price_close > result1$MEAN)), result1$date)
result1 <- as.data.table(result1)
result1
#difference between close and next.close
result11 <- result1[, F2 := stock_price_close - c(NA, head(stock_price_close, -1))][]
result11
#difference high and low
result11$F3 <- ave(((result11$stock_price_high - result11$stock_price_low)), result11$date)
result11

result11 <- as.data.frame(result11)
result11

x <- c(1,0,1);
x
y <- c(1,1,1);
y

corr <- function(x, y){
  if(length(y) < length(x)){
    temp_x <- x
    x<- y
    y<- temp_x
  }
  lx <- length(x)
  ly <- length(y)
  biaser <- (lx-1)/lx
  mx <- mean(x)
  vx <- var(x)*biaser
  row<- NULL
  for (i in 1:(ly-lx+1)){
    dummy <- y[i:(i+lx-1)]
    my <- mean(dummy)
    vy <- var(dummy)*biaser
    elem <- ((x-mx)%*%(dummy-my))/(lx*sqrt(vx*vy))
    row <- c(row, elem)
  }
  return(row)
}

lwin = 3;
test <- matrix(corr(x[1:(lwin)], y))
test

#
















for (i in 1:(length(x)-lwin+1)) {
  
if (i==1){mat<- matrix(corr(y[1:(lwin)],x))}
  else{
    mat<- cbind(mat, matrix(corr(y[i:(lwin+i-1)],x)))
  }
}
mat

a<- mat[,1]
a<- (a>0.6)
word <- NULL
wordList<- NULL
for (i in 1:(length(a))){
  if (a[i] ==T) {word <- c(word, i)}
  else if (!is.null(word))
  {wordList[length(wordList)+1]<- list(word)
   word<-NULL}
}
wordList












if(length(y) < length(x)){
  temp_x <- x
  x<- y
  y<- temp_x
}
lx <- length(x)
lx
ly <- length(y)
ly
biaser <- (lx-1)/lx
biaser
mx <- mean(x)
mx
vx <- var(x)*biaser
vx

row=NULL
i = 1
dummy <- y[i:(i+lx-1)]
dummy
my <- mean(dummy)
my
vy <- var(dummy)*biaser
vy
elem <- ((x-mx)%*%(dummy-my))/(lx*sqrt(vx*vy))
elem
row <- c(row, elem)
row



















