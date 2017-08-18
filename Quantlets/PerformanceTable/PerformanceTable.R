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