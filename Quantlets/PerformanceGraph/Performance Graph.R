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