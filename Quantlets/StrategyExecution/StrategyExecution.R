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

strategy_exe(s1, s2, start.date = start.date, end.date = end.date)