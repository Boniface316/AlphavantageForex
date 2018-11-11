#' Get financial data from the Alpha Vantage API
#' @name getFXdata
#' @param FUN function list that matches with Aplphavantag function list
#' @param from_symbol base currency
#' @param to_symbol quote currency
#' @param interval frequency of the data
#' @param outputsize compact or full
#' @param datatype csv or json
#' @return  returns the time series OHLC
#'
getFXdata <- function(FUN, apiKey,  from_symbol = NULL, to_symbol = NULL,
                      interval = NULL, outputsize = "full", datatype = "csv",...) {

  if (is.null(apiKey)) {
    stop("API key is not provided")
  }

  # Setup
  placeHolder <- list(...)

  # Save variables
  placeHolder$from_symbol <- from_symbol
  placeHolder$to_symbol   <- to_symbol
  placeHolder$interval    <- interval
  placeHolder$outputsize  <- outputsize
  placeHolder$apikey      <- apiKey
  placeHolder$datatype    <- datatype

  # Generate URL
  url_params <- str_c(names(placeHolder), placeHolder, sep = "=", collapse = "&")
  url <- glue("https://www.alphavantage.co/query?function={FUN}&{url_params}")

  # Alpha Advantage API call
  response <- GET(url)

  #Save the content in csv file
  content <- content(response, as = "text", encoding = "UTF-8")
  content <- read_csv(content)

  #convert to the data to xts type
  content <- xts(order.by = content$timestamp, coredata(content[2:5]))

  return(content)

}



#' Get financial data from the Alpha Vantage API using multiple currency pairs
#' @name multipleFX_pair
#' @param currencyPair currency pairs of our interest
#' @return  returns the time series OHLC in a list
#'
multipleFX_Pair <- function(currencyPair, apiKey = NULL, ...) {

  masterData <- get("masterData", envir = .GlobalEnv)
  successPair <- get("successPair", envir = .GlobalEnv)
  failedPair <- get("failedPair", envir = .GlobalEnv)

  from_symbol <- substr(currencyPair,1,3)
  to_symbol <- substr(currencyPair,4,6)

  if(is.null(apiKey)) {
    apiKey <- get("apiKey", envir = .GlobalEnv)
  }

  tryCatch(
    expr = {
      dat <- getFXdata(FUN = "fx_daily", apiKey = apiKey,  from_symbol = from_symbol, to_symbol = to_symbol)
      successPair <- c(successPair,currencyPair)
      assign("successPair", successPair, envir = .GlobalEnv)
      print(paste(currencyPair, "succeeds"))
      masterData[[currencyPair]] <- dat
      #write.csv(dat, file = paste0(currencyPair,".csv"))
    }
    ,
    error = function(err) {
      print(paste(currencyPair, "Fails"))
      failedPair <- c(failedPair, currencyPair)
      assign("failedPair", failedPair, envir = .GlobalEnv)
    }
  )


  assign("masterData", masterData, envir = .GlobalEnv)

  return("Check the list masterData")

}
