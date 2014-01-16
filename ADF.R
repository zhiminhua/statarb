# This program is used to conduct Augmented Dickey-Fuller test
library(zoo)
library(tseries)

# Load the data
stock1 <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=1398.HK&ignore=.csv", stringsAsFactors=F)
stock2 <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=0939.HK&ignore=.csv", stringsAsFactors=F)

stock1 <- zoo(stock1[,7], as.Date(stock1[,1]))
stock2 <- zoo(stock2[,7], as.Date(stock2[,1]))

t.zoo <- merge(stock1, stock2, all=FALSE)
t <- as.data.frame(t.zoo)

cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")

# Calculate hedge ratio
m <- lm(stock1 ~ stock2 + 0, data=t)
beta <- coef(m)[1]
cat("Assumed hedge ratio is", beta, "\n")

# Perform ADF Test for cointegration
sprd <- t$stock1 - beta*t$stock2
ht <- adf.test(sprd, alternative="stationary", k=0)

cat("ADF p-value is", ht$p.value, "\n")

if (ht$p.value < 0.05) {
    cat("The spread is likely mean-reverting\n")
} else {
    cat("The spread is not mean-reverting.\n")
}
