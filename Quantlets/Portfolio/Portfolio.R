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