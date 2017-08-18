# For using this quantlet you need the Adjusted_Price Dataframe from the DataDownload quantlet
# After running the code you will see a Correlation table

# parameter settings

final = Adjusted_Price #change name

# ------------------------------------------------------------------------ #
# Correlation Matrix                                                       #
# ------------------------------------------------------------------------ #

z     = cor(final, method = "spearman")

# ------------------------------------------------------------------------ #
# Sort the correlation and reshape the correlation matrix as a table       #
# ------------------------------------------------------------------------ #

z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
z = as.data.frame(as.table(z))  #Turn into a 3-column table
z = na.omit(z)  #Get rid of the junk we flagged above
z = z[order(-z$Freq),]    #Sort by highest correlation
rownames(z) <- 1:length(z$Freq) #Rename row name to see where we are
colnames(z) <- c("Stock 1", "Stock 2", "Correlation")

# ------------------------------------------------------------------------ #
# View the correlation table                                               #
# ------------------------------------------------------------------------ #

view(z)
