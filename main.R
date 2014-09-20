GOOG<- read.csv("Stock/Data/Sample/NASDAQ_GOOG.csv")
GOOG$exchange <- NULL
GOOG$stock_symbol <- NULL
GOOG$calendate <- GOOG$date
GOOG$date <- as.numeric(as.Date(GOOG$date, format = "%d/%m/%Y"))


endDateGOOG <- GOOG[,"date"][1]
startDateGOOG <- GOOG[,"date"][length(GOOG[,"date"])]

YHOO<- read.csv("Stock/Data/Sample/NASDAQ_YHOO.csv")
YHOO$exchange <- NULL
YHOO$stock_symbol <- NULL
YHOO$calendate <- YHOO$date

YHOO$date <- as.numeric(as.Date(YHOO$date, format = "%d/%m/%Y"))


endDateYHOO <- YHOO[,"date"][1]
startDateYHOO <- YHOO[,"date"][length(YHOO[,"date"])]

AAPL<- read.csv("Stock/Data/Sample/NASDAQ_AAPL.csv")
AAPL$exchange <- NULL
AAPL$stock_symbol <- NULL
AAPL$calendate <- AAPL$date
AAPL$date <- as.numeric(as.Date(AAPL$date, format = "%d/%m/%Y"))


endDateAAPL <- AAPL[,"date"][1]
startDateAAPL <- AAPL[,"date"][length(AAPL[,"date"])]

analysisBegin = max(startDateGOOG, startDateYHOO, startDateAAPL)
analysisEnd = min(endDateGOOG, endDateYHOO, endDateAAPL)
windowed.GOOG <- GOOG[GOOG$date >= analysisBegin,]
windowed.GOOG <- windowed.GOOG[GOOG$date <= analysisEnd,]
windowed.AAPL <- AAPL[AAPL$date >= analysisBegin,]
windowed.AAPL <- windowed.AAPL[windowed.AAPL$date <= analysisEnd,]
windowed.YHOO <- YHOO[YHOO$date >= analysisBegin,]
windowed.YHOO <- windowed.YHOO[windowed.YHOO$date <= analysisEnd,]



corre <- function(x, y){
  lx <- length(x)
  biaser <- (lx-1)/lx
  mx <- mean(x)
  vx <- var(x)*biaser
  
  row<- NULL
  
  for (i in 1:(length(y)-lx+1)){
    
    dummy <- y[i:(i+lx-1)]
    my <- mean(dummy)
    
    vy <- var(dummy)*biaser
    elem <- ((x-mx)%*%(dummy-my))/(lx*sqrt(vx*vy))
      
    row <- c(row, elem)
  }
  
  return(row)
}



lwin <- 50


for (i in 1:(length(windowed.AAPL$stock_price_open)-lwin+1)) {
  
  if (i==1){mat<- matrix(corre(windowed.GOOG$stock_price_open[1:(lwin)],windowed.AAPL$stock_price_open))}
  else{
  mat<- cbind(mat, matrix(corre(windowed.GOOG$stock_price_open[i:(lwin+i-1)],windowed.AAPL$stock_price_open)))
      }
}

a<- mat[,1]
a<- (a>0.8)
word <- NULL
wordList<- NULL
for (i in 1:(length(a))){
  if (a[i] ==T) {word <- c(word, i)}
  else if (!is.null(word))
  {wordList[length(wordList)+1]<- list(word)
  word<-NULL}
}



dateMatrix<- matrix(0, nrow = length(wordList), ncol=2)
calendateMatrix<- matrix(0, nrow = length(wordList), ncol=2)
for (j in 1:(length(wordList))){
  
  dateMatrix[j, 1] <- AAPL$date[max(wordList[[j]])+lwin]
  dateMatrix[j, 2] <- AAPL$date[min(wordList[[j]])]
  
  calendateMatrix[j, 1] <- as.character(AAPL$calendate[max(wordList[[j]])+lwin])
  calendateMatrix[j, 2] <- as.character(AAPL$calendate[min(wordList[[j]])])
  
}





















X1 <- as.numeric(as.character(GOOG$stock_price_open));
X2 <- as.numeric(as.character(GOOG$stock_price_open));

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

test <- matrix(corr(X1[1:lwin], X2))
test <- cbind(test, matrix(corr(X1[2:(lwin+1)], X2)))









