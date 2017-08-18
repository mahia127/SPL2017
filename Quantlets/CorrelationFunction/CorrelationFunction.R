library(dplyr)
library(data.table)
library(xts)
library(zoo)
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(BatchGetSymbols, quietly = T)

install.packages("dplyr")

cor_download = function(last.date = Sys.Date(),
                        first.date = as.Date("1990-01-01"), 
                        TickerSymbol = "NASDAQ"){
  
  symbols <- stockSymbols(TickerSymbol)
  
  ########### Get rid of the stocks, which has several Symbols for the same Stock ########### 
  
  symbols <- symbols[!duplicated(symbols [ ,2]), ]
  
  ########### Filtering all the Stocks without MarketCap, cause they are producing Errors, because the are'nt on Yahoo finance anymore #######
  #cause of BatchGetSymbols, not neccesarry anymore
  
  symbols <- filter(symbols, !is.na(MarketCap))
  
  ########### Only use Corporations with a Marketcap over one billion
  
  symbols <- symbols[grepl("B", symbols$MarketCap, ignore.case = TRUE), ]
  
  ########### get rid of all etfs, and funds
  
  symbols <- symbols[!grepl("ETF", symbols$Name, ignore.case = TRUE), ]
  
  symbols <- symbols[!grepl("Fund", symbols$Name, ignore.case=TRUE),]
  
  symbols <- symbols[!grepl("Shares", symbols$Name, ignore.case=TRUE),]
  
  symbols <- symbols[!grepl("Index", symbols$Name, ignore.case=TRUE),]
  
  ########### Get rid of all the "useless data, so only the Symbols remain ######
  
  tickers <- symbols$Symbol
  
  ########### using BatchGetSymbols, to avoid Errors, caused by the fucking yahoo finance ###########
  
  # Date part to add start date end date
  #first.date <- Sys.Date()-150 # today -150 days, can be changed for sure
  
  all_stock <<- BatchGetSymbols(tickers = tickers,
                                first.date = first.date,
                                last.date = last.date)   # that's gonna take a while, ---> go get a coffee!
}

start_n_pair = 1
end_n_pair = 100000
start.date.cor = "1997-01-01"
end.date.cor = "2007-01-01"

Stock_Prices <- all_stock$df.tickers
control <- all_stock$df.control #just to have a quick look, how many tickers are useless!

########### get rid of all prices Instead of the Adjusted Close Price ###########

Adjusted_Price = Stock_Prices[-c(1:5)]

########### reshape it, so i can use it for the cor ###########

Adjusted_Price = reshape(Adjusted_Price, idvar = "ref.date", timevar = "ticker", direction = "wide")

########### dates, as row names ########### 

rownames(Adjusted_Price) = Adjusted_Price$ref.date
Adjusted_Price = Adjusted_Price[,-1]

########### rename the columns ###########

colnames(Adjusted_Price) = sub(".*\\.", "", colnames(Adjusted_Price))

# extract the price in the period
# again, the date is the closest trading day to the sepecified dates
row_start = min(which(abs(as.Date(rownames(Adjusted_Price)) - start.date.cor) 
                      == min(abs(as.Date(rownames(Adjusted_Price)) - start.date.cor))))


#
row_end = min(which(abs(as.Date(rownames(Adjusted_Price)) - end.date.cor) 
                    == min(abs(as.Date(rownames(Adjusted_Price)) - end.date.cor))))

Adjusted_Price = Adjusted_Price[row_start:row_end, ]

# filter out all the stocks not listed in the period
Adjusted_Price = Adjusted_Price[sapply(Adjusted_Price,
                                       function(x) !any(is.na(x)))]

#cor
final = Adjusted_Price

z = cor(final, method = "spearman")

#Sorting

z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
z=as.data.frame(as.table(z))  #Turn into a 3-column table
z=na.omit(z)  #Get rid of the junk we flagged above
z=z[order(-z$Freq),]    #Sort by highest correlation
rownames(z) = 1:length(z$Freq) #Rename row name to see where we are

############################### Sort out the best 3 pairs ###############################

#Think about skipping the pairs with cor=0.99, cause yahoo uses different symbols for the "same" stocks
# Fonds are a problem, how to exclude them ????
# maybe a cor near to 0.99 isn't the most profitable, maybe a range between 0.94-0.97 is the most profitable
# backtest it, neccesarry
# thinking about other or different reasons for picking the stocks !!

cor_table = z[start_n_pair:end_n_pair, ]
