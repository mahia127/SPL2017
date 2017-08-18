# clear variables and close windows
rm(list =ls(all = TRUE)
graphics.off()
clc
clear 
close all

#packages

#library(dplyr)
#library(data.table)
#library(xts)
#library(zoo)
#library(quantmod)
#library(PerformanceAnalytics)
#library(TTR)
#library(BatchGetSymbols, quietly = T)

#parameter settings

TickerSymbol = "NASDAQ"                       #can choose between, "NASDAQ", "AMEX", "NYSE"
symbols      = stockSymbols(TickerSymbol)     #Storage for the TickerSymbols

# ------------------------------------------------------------------------ #
# filter several stocks                                                    #
# ------------------------------------------------------------------------ #

symbols = symbols[!duplicated(symbols [ , 2]) , ]                      # Get rid of the stocks, which has several Symbols for the same Stock
symbols = filter(symbols,  !is.na(MarketCap))                          # Filtering all the Stocks without MarketCap
symbols = symbols[grepl("B", symbols$MarketCap, ignore.case=TRUE),]    # Only use Corporations with a Marketcap over one billion
symbols = symbols[!grepl("ETF", symbols$Name,ignore.case=TRUE),]       # get rid of all etfs, and funds
symbols = symbols[!grepl("Fund", symbols$Name, ignore.case=TRUE),]     # get rid of all etfs, and funds
symbols = symbols[!grepl("Shares", symbols$Name, ignore.case=TRUE),]   # get rid of all etfs, and funds
symbols = symbols[!grepl("Index", symbols$Name, ignore.case=TRUE),]    # get rid of all etfs, and funds

#parameter settings

tickers    = symbols$Symbol   # Get rid of all the "useless data, so only the Symbols remain
first.date = Sys.Date()-150   # today -150 days
last.date  = Sys.Date()       # if (), it's today

# ------------------------------------------------------------------------ #
# using BatchGetSymbols                                                    #
# ------------------------------------------------------------------------ #   
   
l.out = BatchGetSymbols(tickers = tickers,                           
                         first.date = first.date,
                         last.date = last.date)   
                         
# ------------------------------------------------------------------------ #
# using BatchGetSymbols and build the Adjusted_Price Dataframe             #
# ------------------------------------------------------------------------ # 

Stock_Prices             = l.out$df.tickers                                     # Extracting the adjusted Prices
Adjusted_Price           = Stock_Prices[-c(1:5)]                                # get rid of all prices Instead of the Adjusted Close Price
Adjusted_Price           = reshape(Adjusted_Price,
                   idvar = "ref.date", timevar = "ticker", direction = "wide")  # reshape the Matrix, so we can use it for the correlation
rownames(Adjusted_Price) = Adjusted_Price$ref.date                              # dates, as row names 
Adjusted_Price           = Adjusted_Price[,-1]
colnames(Adjusted_Price) = sub(".*\\.", "", colnames(Adjusted_Price))           # rename the columns
Adjusted_Price           = Adjusted_Price[sapply(Adjusted_Price,
                                       function(x) !any(is.na(x)))]             # filter out all the stocks not listed in the period
