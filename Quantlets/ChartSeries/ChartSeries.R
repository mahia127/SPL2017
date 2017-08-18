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
   
   #parameter settings
   
   StartDate     = "2016-01-01"             #set Start Date
   Stock1        = "TTWO"                   #insert the tickersymbol for Stock 1 manually
   Stock2        = "PLUS"                   #insert the tickersymbol for Stock 2 manually
   Stock1_Price  = xts()                    #for the chart series function we need a xts input
   Stock2_Price  = xts()
   
   # ------------------------------------------------------------------------ #
   # load stock price data                                                    #
   # ------------------------------------------------------------------------ #
   
   Stock1_Price <- getSymbols(Stock1, from = StartDate, auto.assign = FALSE)           #load the Stockprice data
   Stock1_Price <- na.approx(Stock1_Price)                                             #handle na. values
   Stock2_Price <- getSymbols(Stock2, from = StartDate, auto.assign = FALSE)
   Stock2_Price <- na.approx(Stock2_Price)
   
   # ------------------------------------------------------------------------ #
   # generate the chart table                                                 #
   # ------------------------------------------------------------------------ #
   
   chartSeries(c(Stock1_Price, Stock2_Price), type = "line", subset = "last 12 months", 
               name = "Spread of TTWO and PLUS", theme = chartTheme("white"), multi.col = FALSE)       #generate the chart