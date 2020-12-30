library(glue)
library(httr)
library(data.table)
library(stringr)
library(readr)
library(AlphavantageForex)
library(xts)


apiKey <- "Your API here"

#The target contains all the currency list
url <- "https://www.alphavantage.co/physical_currency_list/"

#The csv file contains two columns, our interest is column 1, which contains the currency symbols
currencyList <- as.character(read.csv(url)[,1])

#Total number of currency pair available and the listing them
print(paste0("Total number of currency data available:", length(currencyList)))
print(currencyList)


#Since currencies are traded in pair, we are creating a combo of all currencies
currencyCombo <- expand.grid(currencyList,currencyList)

#Remove currencies that is paired with itself
currencyCombo <- currencyCombo[-c(which(currencyCombo$Var1 == currencyCombo$Var2)),]
#combinng the pairs together as a vector
currencyCombo <- as.vector(paste0(currencyCombo[,1], currencyCombo[,2]))
print(length(currencyCombo))


#initiating empty lists
masterData <- list()
successPair <- c()
failedPair <- c()

#selecting few currency pairs for downloading
symbols <- c("EURUSD", "USDJPY", "GBPUSD")
invisible(lapply(symbols, multipleFX_Pair))

#Examining the pairs that were successful
length(successPair)
print(paste0("The following pairs were sucessfuly downloaded:"))
print(successPair)

#Find the start date of each pair
lapply(masterData, head, n = 1)

#Find the end date of each pair
lapply(masterData, tail, n = 1)
