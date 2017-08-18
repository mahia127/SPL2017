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