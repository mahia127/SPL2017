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

#to use the function
#data_down(insert the two stocks you want to use here and delete#)

start.date = as.Date("1997-01-01")
end.date = as.Date("2007-01-01")

strategy_exe = function (s1 , s2 , start.date = max(s1$Date[1],s2$Date[2]),
                         end.date = Sys.Date(), limit=0.005){
  
  #cut table into the specific time period
  #nearest approximate trading day only - minor impact on regression
  #default is the whole period of the pair
  
  start1 = which(abs(s1$Date - start.date) == min(abs(s1$Date - start.date)))
  end1 = which(abs(s1$Date - end.date) == min(abs(s1$Date - end.date)))
  s1 = s1[start1:end1, ]
  
  start2 = which(abs(s2$Date - start.date) == min(abs(s2$Date - start.date)))
  end2 = which(abs(s2$Date - end.date) == min(abs(s2$Date - end.date)))
  s2 = s2[start2:end2, ]
  
  #return should be given in percentage
  Rt_long=Rt_short=vector()
  
  # 1st Rt for the smaller row number stock is 1, meaningless
  #daytoday-return long
  Rt_long = c(1,diff(s1[,2])/head(s1[,2], -1))
  
  #daytoday-return short
  Rt_short = c(1,(-diff(s2[,2])/head(s2[,2], -1)) )
  
  R5t_long=R5t_short=vector()
  
  #5-day long return
  for (i in 1:(length(s1$Date)-5)){
    R5t_long[i]=((s1[i+5,2]-s1[i,2])/s1[i,2])
  }
  
  #5-day short return
  for (i in 1:(length(s2$Date)-5)){
    R5t_short[i]=-((s2[i+5,2]-s2[i,2])/s2[i,2])
  }
  
  # returns Rt and R5t is just the sum of Rt_short, Rt_long; R5t_short, R5t_long
  # be careful with the number of entry, check the corresponding entry before further calculation
  # 1st term = 2 is irrelavent as it will not be used in regression
  
  Rt = Rt_long + Rt_short
  R5t = R5t_long + R5t_short
  
  #sort data into regression table
  n_Rt=length(Rt)
  Rt_0=Rt[6:n_Rt]
  Rt_1=Rt[5:(n_Rt-1)]
  Rt_2=Rt[4:(n_Rt-2)]
  Rt_3=Rt[3:(n_Rt-3)]
  Rt_4=Rt[2:(n_Rt-4)]
  
  # observe the table directly to double check errors
  # see=cbind(R5t,Rt_0,Rt_1,Rt_2,Rt_3,Rt_4)
  # View(see)
  
  # linear regression coefficient for each single day is as Rt_n_c
  Rt_0_c=Rt_1_c=Rt_2_c=Rt_3_c=Rt_4_c=vector()
  local(
    for (i in 1:length(R5t)){
      n=1:i
      lr=lm(R5t[n]~Rt_0[n]+Rt_1[n]+Rt_2[n]+Rt_3[n]+Rt_4[n])
      coef=as.numeric(lr$coefficients)
      Rt_0_c[i]<<-coef[2]
      Rt_1_c[i]<<-coef[3]
      Rt_2_c[i]<<-coef[4]
      Rt_3_c[i]<<-coef[5]
      Rt_4_c[i]<<-coef[6]
    }
  )
  
  #average coefficients ac
  Rt_0_ac=Rt_1_ac=Rt_2_ac=Rt_3_ac=Rt_4_ac=vector()
  
  for (i in 1:(length(R5t)-124)){
    n = (i+5):(i+119)
    Rt_0_ac[i] = sum(Rt_0_c[n])/115
    Rt_1_ac[i] = sum(Rt_1_c[n])/115
    Rt_2_ac[i] = sum(Rt_2_c[n])/115
    Rt_3_ac[i] = sum(Rt_3_c[n])/115
    Rt_4_ac[i] = sum(Rt_4_c[n])/115
  }
  
  coef_av <<- cbind(Rt_0_ac,Rt_1_ac,Rt_2_ac,Rt_3_ac,Rt_4_ac)
  
  # expected 5-day return to pair trading strategy
  ex_R5t=vector()
  #average refers to the sum of all values in the past 120 days up to the day 5 days ago,
  for (i in 1:length(Rt_4_ac)){
    ex_R5t[i]=Rt_1[120+i]*Rt_1_ac[i]+Rt_2[120+i]*Rt_2_ac[i]+
      Rt_3[120+i]*Rt_3_ac[i]+Rt_4[120+i]*Rt_4_ac[i]
  }
  
  
  #next steps
  ##taking care of the different lengths of s1 and s2
  ##comparing ex_R5t to limit >+-0,5% if yes date s1 s2 and 1 or 2 for 1=long s1/short s2 and 2=short s1/long s2
  
  # trading direction trade_dir : 1 for +0.5% --- 2 for -0.5%
  trade_dir = vector()
  
  # LS_dir is long-or-short direction
  trade_dir = ifelse(ex_R5t>limit,1,ifelse(ex_R5t<(-limit),2,0))
  
  trade_table = data.frame(s1, s2[,2], c(rep(0,129),trade_dir))
  colnames(trade_table) = c("Date","s1_o","s2_o","LS_dir")
  
  #View(trade_table)
  
  ### 5-day exit strategy ###
  
  exit_index=which(trade_table$LS_dir != 0)+5
  
  exit_table=data.frame(s1[exit_index, ], s2[ ,2][exit_index])
  
  #View(exit_table)
  
  trade_table_5d=cbind(trade_table[which(trade_table$LS_dir!=0),],exit_table)
  
  # adjust for the last 5 entries, future data not yet observed
  # as well as rearrange the level of the table
  # get rid of NA cases
  trade_table_5d=droplevels(trade_table_5d[complete.cases(trade_table_5d),])
  rownames(trade_table_5d)=1:nrow(trade_table_5d)
  #View(trade_table_5d)
  
  colnames(trade_table_5d)[c(1,5:7)] = c("Date_o","Date_c", "s1_c","s2_c")
  
  ### better version of return vector
  # for every 1 dollar - dollar-wise
  return_5d = ifelse(trade_table_5d$LS_dir==1,+1,-1) *
    (1/trade_table_5d$s1_o * (trade_table_5d$s1_o-trade_table_5d$s1_c)
     + 1/trade_table_5d$s2_o *(trade_table_5d$s2_c-trade_table_5d$s2_o))
  
  
  trade_table_5d <<- cbind(trade_table_5d,return_5d)
  colnames(trade_table_5d)[8] <<- "Return"
  
  ############ - End
  
  ### 2nd strategy ###
  
  # intermediate trading table to visualize the 2nd strategy execution
  
  inter_table_2nd = droplevels(cbind(trade_table[-(1:129),],ex_R5t))
  colnames(inter_table_2nd)[5] = "ex_R5t"
  rownames(inter_table_2nd)=1:nrow(inter_table_2nd)
  
  # different methods in 1st & 2nd strategy
  # 1st - first make combine exit data frame to trading table; then remove NA
  # 2nd - first remove NA; then arrange exit data
  
  # if the trade doesn't happen, assign NA to close_row[i]
  # if the trade happens, find the first row number which inter_table_2nd$LS_dir turns +ve/-ve
  # for the value = Position()
  # if LS_dir = 1, look for -ve, return row number to value
  # if LS_dir = 2, look for +ve, return row number to value
  # if no turn has occured, return NA to value
  # then return the row number or 10 day (if NA occurs)
  
  close_row=vector()
  for (i in 1:(nrow(inter_table_2nd)-10)){
    if (inter_table_2nd$LS_dir[i]==0){
      close_row[i] = NA
    }else {
      value = Position(function(b) switch(inter_table_2nd$LS_dir[i],b<0,b>0),
                       inter_table_2nd$ex_R5t[(i+4):(i+10)])
      close_row[i] = i + 3 + min(ifelse(is.na(value),11,value),7)
    }
  }
  
  trade_table_2nd = cbind(inter_table_2nd[(1:length(close_row)),],
                          inter_table_2nd[close_row,1:3])
  colnames(trade_table_2nd)[c(1,6:8)]=c("Date_o","Date_c","s1_c","s2_c")
  
  # get rid of NAs, same to get rid of 0 in trading direction
  trade_table_2nd = trade_table_2nd[complete.cases(trade_table_2nd),]
  #View(trade_table_2nd)
  
  ### better version of return vector
  # for every 1 dollar - dollar-wise
  return_2nd = ifelse(trade_table_2nd$LS_dir==1,+1,-1) *
    (1/trade_table_2nd$s1_o * (trade_table_2nd$s1_o-trade_table_2nd$s1_c)
     + 1/trade_table_2nd$s2_o *(trade_table_2nd$s2_c-trade_table_2nd$s2_o))
  
  rownames(trade_table_2nd) = 1:nrow(trade_table_2nd)
  trade_table_2nd <<- cbind(trade_table_2nd,return_2nd)
  colnames(trade_table_2nd)[9] <<- "Return"
  
  # there may be difference among trade_table - trade_table_2nd - trade_table_5d
  # some recent trades occur in the recent observing period
  # future exit position & price is not yet observed
  trade_table <<- trade_table[trade_table$LS_dir != 0, ]
  
}

##### to use function
#####strategy_exe(insert the two stock names here and delete#)

par( mfrow = c(3, 2))

#mannually input the table - tt_2nd + n, e,g, tt_2nd6181
cum_return = cumsum(tt_2nd6181$Return)
plot(tt_2nd6181$Date_c, cum_return,
     type = "l", main = "Cumulative return of trading", ylab = "Return", xlab = "Date")
abline(0,0)

cum_return = cumsum(tt_2nd6182$Return)
plot(tt_2nd6182$Date_c, cum_return,
     type = "l", main = "Cumulative return of trading", ylab = "Return", xlab = "Date")
abline(0,0)

cum_return = cumsum(tt_2nd6183$Return)
plot(tt_2nd6183$Date_c, cum_return,
     type = "l", main = "Cumulative return of trading", ylab = "Return", xlab = "Date")
abline(0,0)

cum_return = cumsum(tt_2nd6184$Return)
plot(tt_2nd6184$Date_c, cum_return,
     type = "l", main = "Cumulative return of trading", ylab = "Return", xlab = "Date")
abline(0,0)

cum_return = cumsum(tt_2nd6185$Return)
plot(tt_2nd6185$Date_c, cum_return,
     type = "l", main = "Cumulative return of trading", ylab = "Return", xlab = "Date")
abline(0,0)

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
  
  ########### using BatchGetSymbols, to avoid Errors###########
  
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

performance = function(table){
  #Average transaction return 
  p1 = mean(table$Return)
  
  #Median transaction return 
  p2 = median(table$Return)
  
  #25th percentile of returns 
  p3 = quantile(table$Return,0.25)
  
  #75th percentile of returns 
  p4 = quantile(table$Return,0.75)
  
  #Positive return-% 
  p5 = length(which(table$Return>0))/length(table$Return)
  
  #Return volatility
  p6 = sd(table$Return)
  
  #Activity-% 
  # minus 129 to adjust the observing period at the beginning
  p7 = (length(table$Return)-129)/(length(trade_table$LS_dir)-129)
  
  #Average holding period
  
  d1=d2=vector()
  for (i in 1:length(table$Date_o)){
    d1[i]=which(table$Date_o[i]==s1$Date)
    d2[i]=which(table$Date_c[i]==s1$Date)
  }
  
  p8 = sum(d2-d1)/length(d2)
  
  performance_table <<- data.frame(c(p1 , p2, p3 ,p4 , p5 , p6 , p7 , p8),
                                   row.names = c("Average transaction return",
                                                 "Median transaction return",
                                                 "25th percentile of returns",
                                                 "75th percentile of returns",
                                                 "Positive return-%",
                                                 "Return volatility",
                                                 "Activity-%",
                                                 "Average holding period")
  )
  colnames(performance_table) <<- "Value"
}

performance(tt_2nd6181)
try = performance_table
performance(tt_2nd6182)
try = cbind(try , performance_table)
performance(tt_2nd6183)
try = cbind(try , performance_table)
performance(tt_2nd6184)
try = cbind(try , performance_table)
performance(tt_2nd6185)
try = cbind(try , performance_table)

colnames(try) = c("Top 1", "Top 2", "Top 3", "Top 4", "Top 5")

#enter the number of pairs you would like to include in the portfolio
n_pair_start = 6181
n_pair_end = 6185
table = cor_table

#enter the portfolio start date and end date
start.date.p = as.Date("1997-01-01")
end.date.p = as.Date("2007-01-01")

for (i in n_pair_end : n_pair_start){
  stock1 = as.character(table[i, 1])
  stock2 = as.character(table[i, 2])
  data_down(stock1, stock2, StartDate = start.date.p)
  strategy_exe(s1, s2, start.date = start.date.p, end.date = end.date.p)
  assign(paste0("tt_2nd",i),trade_table_2nd)
}
#

# return both VaR and ES
risk_man = function (table, q = 0.01){
  #VaR, ES at 1%, 1% all the way through
  VaR_norm <<- qnorm(q, mean(table$Return), sd(table$Return))
  VaR_emp <<- as.numeric(quantile(table$Return, q))
  
  ES_emp <<- mean(table$Return[table$Return < VaR_emp])
  # simulate for the ES under normal distribution
  n = 1000000
  sim_N = rnorm(n, mean(table$Return), sd(table$Return))
  ES_norm <<- mean(sim_N[sim_N < VaR_norm])
  
  assign("return", table$Return, envir = .GlobalEnv)
}

risk_man(trade_table_2nd)

# as VaR_emp is much smaller than VaR_norm
# VaR under normal distribution much under-estimate true VaR

# Use both graph, 1st one to inspect, 2nd to investigate how it behaves
plot(return, type = "h", main = "Return of 1 dollar in single trade",
     ylab = "Return", xlab = "Number of trades")
abline(VaR_norm, 0, col = "blue")
abline(VaR_emp, 0, col = "red")
abline(ES_norm, 0, col = "darkblue")
abline(ES_emp, 0, col = "brown1")
legend("topright", c("VaR_norm", "Var_emp", "ES_norm", "ES_emp"),
       lty = rep(1,4), lwd = rep(1,4),
       col = c ("blue", "red", "darkblue", "brown1"), cex = 0.7)

plot(return, type = "p", pch = ".", cex = 2,
     main = "Return of 1 dollar in single trade",
     ylab = "Return", xlab = "Number of trades")
abline(VaR_norm, 0, col = "blue")
abline(VaR_emp, 0, col = "red")
abline(ES_norm, 0, col = "darkblue")
abline(ES_emp, 0, col = "brown1")
legend("topright", c("VaR_norm", "Var_emp", "ES_norm", "ES_emp"),
       lty = rep(1,4), lwd = rep(1,4),
       col = c ("blue", "red", "darkblue", "brown1"), cex = 0.7)

# QQ-plot, simulate the normal distribution to see if it is heavily tailed
n = 1000000
sim_N = rnorm(n,mean(return),sd(return))
# reminder: sd of return is 0.06855707, so fitted normal belongs [-0.3,0.3]
qqplot(sim_N, return, main="QQ-plot: return vs fitted normal", 
       ylab="Return quantiles", 
       xlab="Fitted normal quantiles")
abline(0,1)
# both end is heavily tailed, but positive is the extreme return, no need to care

strategy_exe(s1, s2, start.date = start.date, end.date = end.date)