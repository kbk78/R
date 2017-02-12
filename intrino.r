rm(list=ls(())
install.packages("httr")
library("zoo")
library("xml2")
library("plyr")
library("httr")

username = "a8e617296cfd2c16b3f7398b1e33e8f6"
password = "a072f834567f776fda7c3635c968b38e"



# 52_week_high 52_week_low average_daily_volume bookvaluepershare dividend

getstockhist=function(tic,item,start_date,end_date){
#q <- paste("https://api.intrinio.com/historical_data?ticker=",tic,'&item=',item,sep="")
q <- paste("https://api.intrinio.com/historical_data?ticker=",tic,
           "&item=", item,
           "&start_date=",start_date,
           "&end_date=",end_date,
            sep="") 
tp <- GET(q, authenticate("a8e617296cfd2c16b3f7398b1e33e8f6","a072f834567f776fda7c3635c968b38e", type = "basic"))
z=head(unlist(content(tp,'parsed')),-7)
b = as.data.frame(matrix(z,length(z)/2, byrow = T))
names(b)=c('date',item)
rownames(b) = b$date
b$date=NULL
return(b)
}

getprice=function(tic,item,start_date,end_date){
  q <- paste("https://api.intrinio.com/prices?identifier=",tic,sep="")
  tp <- GET(q, authenticate("a8e617296cfd2c16b3f7398b1e33e8f6","a072f834567f776fda7c3635c968b38e", type = "basic"))
  z=head(unlist(content(tp,'parsed')),-7)
  b = as.data.frame(matrix(z,length(z)/2, byrow = T))
  names(b)=c('date',item)
  rownames(b) = b$data.date
  return(b)
}
ap = getstockhist('AAPL','pricetoearnings',"2010-01-01","2016-10-01")



----------------------------------------------------
#################################################################################################

#Setting up the environment

#################################################################################################

rm(list=ls())
# install required R packages
if (!'xml2' %in% installed.packages()){
  install.packages('xml2')
}

if (!'httr' %in% installed.packages()){
  install.packages('httr')
}

if (!'plyr' %in% installed.packages()){
  install.packages('plyr')
}
if (!'zoo' %in% installed.packages()){
  install.packages('zoo')
}

library("zoo")
library("xml2")
library("plyr")
library("httr")
#################################################################################################

#Price Function

#################################################################################################

prices <- function(ticker){
  price_base <- "https://api.intrinio.com/prices?identifier="          
  username <- "a543b029ec930ab0c7add95bfa1ea3ac"                      
  password <- "991d8ca925d74ecfbe7f78b4784d88b0"                      
  
  price <- paste(price_base,ticker,sep="")                           
  tp <- GET(price, authenticate(username, password, type = "basic")) 
  z <- unlist(content(tp,"parsed"))
  
  n <- length(z)
  b <- as.data.frame(matrix(z[1:(n-5)],(n-5)/13, byrow = T))
  names(b) <- names(z)[1:13]
  names(b) <- c("date", "open", "high", "low", "close", "volume", "ex_dividend", "split_ratio", "adj_open", "adj_high", "adj_low", "adj_close", "adj_volume")
  
  #change the data types
  b$date <- as.Date(b$date)
  w <- which( sapply( b, class ) == 'factor' )
  b[w] <- lapply( b[w], function(x) as.numeric(as.character(x)) )
  return(b)
}

#################################################################################################

#Historical Function

#################################################################################################

history <- function(ticker, item, start_date, end_date){
  history_base <- "https://api.intrinio.com/historical_data?ticker="        
  username <- "a543b029ec930ab0c7add95bfa1ea3ac"                      
  password <- "991d8ca925d74ecfbe7f78b4784d88b0"                      
  
  historical <- paste(history_base, ticker, "&item=", item, "&start_date=", start_date, "&end_date=", end_date, sep="")                          
  tp <- GET(historical, authenticate(username, password, type = "basic")) 
  z <- unlist(content(tp,"parsed"))
  
  n <- length(z)
  b <- as.data.frame(matrix(z[1:(n-5)],(n-5)/2, byrow = T))
  
  #delete the last line with the ticker and variable name
  if(b[nrow(b), 1] == ticker)
  {
    b <- b[1:nrow(b)-1, ]
  }
  names(b) <- names(z)[1:2]
  names(b) <- c("date", item)
  b$date <- as.Date(b$date)
  w <- which( sapply( b, class ) == 'factor' )
  b[w] <- lapply( b[w], function(x) as.numeric(as.character(x)) )
  return(b)
}


#################################################################################################

#Ticker Data

#################################################################################################

ticker_data <- function(ticker, items, start_date, end_date){
  
  if(length(items) == 1) {
    tdata <- history(ticker, items[1], start_date, end_date)
  }
  
  else {
         tdata <- history(ticker, items[1], start_date, end_date)
         for(i in 2:length(items))
         {
            hdata <- history(ticker, items[i], start_date, end_date)
            tdata <- merge(tdata, hdata, by = "date", all = TRUE)
         }
  }
  ticker_price <- prices(ticker)
  ticker_data <- merge(tdata, ticker_price, by = "date", all = TRUE)
  ticker_data_filled <- na.locf(ticker_data)
  
  ticker_data_filled$date <- as.Date( ticker_data_filled$date)
  w <- which( sapply(  ticker_data_filled, class ) == 'character' )
  ticker_data_filled[w] <- lapply(  ticker_data_filled[w], function(x) as.numeric(x) )
  
  return( ticker_data_filled)
}


#################################################################################################

#Ticker Data for a list of Stocks -- returns a dataframe

#################################################################################################
allTickerData <- function(tickers, items, start_date, end_date){
  tData <- data.frame()
  allData <- data.frame()
  for(i in 1:length(tickers)) {
    tData <- ticker_data(tickers[i], items, start_date, end_date)
    tData$ticker <- tickers[i]
    allData <- rbind(tData, allData)
  }
  return(allData)
}


#################################################################################################

#Ticker Data and Index Data for a list of Stocks -- returns a dataframe with stock and index data

#################################################################################################
allTickerIndexData <- function(tickers, mIndex, items, start_date, end_date){
  tData <- data.frame()
  allData <- data.frame()
  for(i in 1:length(tickers)) {
    tData <- ticker_data(tickers[i], items, start_date, end_date)
    tData$ticker <- tickers[i]
    allData <- rbind(tData, allData)
  }
  indexData <- indexPrices(mIndex)
  allData <- merge(allData, indexData, by = "date")
  return(allData)
}


#################################################################################################

#GET S/P Index data

#################################################################################################
indexPrices <- function(mIndex){
  price_base <- "https://api.intrinio.com/prices?identifier=$"          
  username <- "a543b029ec930ab0c7add95bfa1ea3ac"                      
  password <- "991d8ca925d74ecfbe7f78b4784d88b0"                    
  
  indexVal <- paste(price_base, mIndex, sep="")
  tp <- GET(indexVal, authenticate(username, password, type = "basic")) 
  z <- unlist(content(tp,"parsed"))
  n <- length(z)
  b <- as.data.frame(matrix(z[1:(n-5)], (n-5)/6, byrow = T))
 
  #set column names and convert them to right data types
  colnames(b) <- c("date", paste(mIndex,"open", sep="."), paste(mIndex,"high", sep="."), paste(mIndex,"low", sep="."), paste(mIndex,"close", sep="."), paste(mIndex,"volume", sep="."))
  b$date <- as.Date(b$date)
  w <- which( sapply( b, class ) == 'factor' )
  b[w] <- lapply( b[w], function(x) as.numeric(as.character(x)) )
  b <- na.omit(b)
  return(b)
}


#---------------------------------------------------------

##########################################################

#Setting up the environment

##########################################################

require(zoo)
require(ggplot2)

##########################################################

#Setting up the ticker data function

##########################################################

mIndex <- "SPX"

tickers <- c("BA", "UTX", "LMT", "NOC", "RTN", "GD")

items <- c("pricetoearnings", "beta", "basiceps", "bookvaluepershare", "totalrevenue", "freecashflow", "ebitdamargin","altmanzscore", "ccc", "capex")

start_date <- "2009-12-31" 

end_date <- Sys.Date()

##########################################################

#Calling historical explainatory varaibles and price as response

##########################################################

data <- allTickerIndexData(tickers, mIndex, items, start_date, end_date)


##########################################################

#Removing dates before start_date and pulling seperate data frames for each

##########################################################

data_reduced <- subset(data, data$date > "2009-12-30") #could convert date to variable if you wanted

BA <- subset(data_reduced, data_reduced$ticker == "BA")
UTX <- subset(data_reduced, data_reduced$ticker == "UTX")
LMT <- subset(data_reduced, data_reduced$ticker == "LMT")
NOC <- subset(data_reduced, data_reduced$ticker == "NOC")
RTN <- subset(data_reduced, data_reduced$ticker == "RTN")
GD <- subset(data_reduced, data_reduced$ticker == "GD")

##########################################################

#Lets do some exploring! Start with correlation matrix

##########################################################

x <- BA[22]

y <- BA[c(3:11,28)]

cor_mod1 <- cor(x,y)
cor_mod1
cor_mod1.1 <- cor(y,y)
cor_mod1.1

##########################################################

#lm1

##########################################################

lm1 <- lm(adj_close~SPX.close+beta+basiceps+bookvaluepershare+totalrevenue+freecashflow+ebitdamargin+altmanzscore+ccc+capex, data = BA)

summary(lm1)


##########################################################

#Plot

##########################################################

p1 <- ggplot(data=BA, aes(x=date, y = adj_close)) + geom_line(stat = "identity")
p1.1 <- p1 + xlab("Date") + ylab("Price") + scale_x_date(date_labels = "%b %y") + ggtitle("BA Close Price")
p1.1
p1.2 <- p1.1 + geom_line(aes(x = BA$date, y = BA$SPX.close/10), color = "darkblue")
p1.2



	
