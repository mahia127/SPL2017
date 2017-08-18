##############################################################################
##############################################################################
############# - Data Extraction, Handling na values (improved) - #############
##############################################################################
##############################################################################

#used packages
library(xts)
library(zoo)
library(data.table)
library(quantmod)

#here the Top 3 pairs should insert automatically, for now, we just use AAPL, and IBM

data_down = function (StockSymbol1 ,StockSymbol2 , StartDate = "1900-01-01"){
  Stock1 = na.approx(getSymbols(StockSymbol1 , from = StartDate ,
                                auto.assign = FALSE))
  Stock1_table <- as.data.table(Stock1)
  s1 <<- as.data.frame(Stock1_table[,c(1,7)])
  colnames(s1) <<- c("Date","Adj.Close")
  
  Stock2 = na.approx(getSymbols(StockSymbol2, from = StartDate, 
                                auto.assign = FALSE))
  Stock2_table <- as.data.table(Stock2)
  s2 <<- as.data.frame(Stock2_table[,c(1,7)])
  colnames(s2) <<- c("Date","Adj.Close")
}

### seems like no erro or missing values in closing prices
#which(Stock1_Table[,5]==0)
#which(Stock1_Table[,5]=="NA")