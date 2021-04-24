library(jsonlite)
library(glue)
library(ggplot2)
library(roxygen2)

#' @import jsonlite
#' @import glue
#' @import ggplot2



################################################################################

#' @export
#' @param pair Cryptocurrency and currency pair to import. Can be obtained with "format_pair" function.
#' @title Import Cryptocurrency Data
#' @description Imports open, close, high, and low price; as well as volume of selected cryptocurrency.
#' @examples ## Type the cryptocurrency-local currency pair in quotes.
#' @examples # get_coin_data("BTC-USD")
#'
#' @examples ## Use the format_pair() function to get pair format.
#' @examples # pair <- format_pair("btc", "usd")
#' @examples # get_coin_data(pair)
get_coin_data <- function(pair) {

  url = glue::glue("https://api.pro.coinbase.com/products/{pair}/candles?granularity=86400")
  mydata <- fromJSON(url)

  columnNames <- c('unix', 'low', 'high', 'open', 'close', 'volume')
  df <- as.data.frame(mydata)
  colnames(df) <- columnNames  # rename the columns

  date <- Sys.Date()
  rownames(df) <- seq(from = date, by = "-1 day", length.out = nrow(df))

  df$coinPair <- glue({pair})

  return(df)
}

#pair <- "BTC-USD"
#bitcoin <- get_coin_data(pair)



################################################################################

#' @export
#' @param coin_abbreviation  Abbreviation of cryptocurrency to import (ex. "btc").
#' @param currency_abbreviation Abbreviation of local currency (ex. "usd").
#' @title Format Cryptocurrency and Currency Pair
#' @description Formats cryptocurrency and local currency pair to load into the "get_coin_data" package. An example of the output will resemble "BTC-USD".
#' @examples ## Enter coin abbreviation and local currency abbreviation in quotes, seperated by a comma.
#' @examples # format_pair("btc", "usd")
format_pair <- function(coin_abbreviation, currency_abbreviation) {

  coinPair <- paste0(toupper(coin_abbreviation), "-", toupper(currency_abbreviation))

  return(coinPair)
}

#pair <- format_pair("BTC", "usd")
#bitcoin <- get_coin_data(pair)



################################################################################

#' @export
#' @param coin_data Dataframe produced by the "get_coin_data" function.
#' @title Plot Cryptocurrency Data
#' @description Plots cryptocurrency data using dataframe obtained through "get_coin_data" function using ggplot2.
#' @examples ## Using the get_coin_data() function to obtain dataframe and inserting that dataframe in the function.
#' @examples # coin_data <- get_coin_data(pair)
#' @examples # plot_coin_data(coin_data)
plot_coin_data <- function(coin_data){

  g <- ggplot(coin_data, aes(x=rownames(coin_data), y=close)) +
    geom_line(group=1) +
    geom_point(size=.3) +
    labs(title = glue("{coin_data$coin[1]}", " Closing Price"),x = "Dates", y = "Closing Price") +
    theme(axis.text.x = element_text(angle = 90, size = 6)) +
    scale_x_discrete(breaks = rownames(coin_data)[seq(1, nrow(coin_data), by=4)])

  return(g)
}

#plot_coin_data(bitcoin)



################################################################################

#' @export
#' @param coin_data Dataframe produced by the "get_coin_data" function.
#' @title 52-Week Highest Price
#' @description Gets the highest price within a 52 week period and prints the price and date of that occurrence.
#' @examples ## Using the get_coin_data() function to obtain dataframe and inserting that dataframe in the function.
#' @examples # coin_data <- get_coin_data(pair)
#' @examples # high_52_week(coin_data)
high_52_week <- function(coin_data) {

  index <- which.max(coin_data$high)
  date <- rownames(coin_data)[index]
  price <- coin_data$high[index]

  answer <- paste0("The 52 week high was $", price, " on ", date)

  return(answer)
}



################################################################################

#' @export
#' @param coin_data Dataframe produced by the "get_coin_data" function.
#' @title 52-Week Lowest Price
#' @description Gets the lowest price within a 52 week period and prints the price and date of that occurrence.
#' @examples ## Using the get_coin_data() function to obtain dataframe and inserting that dataframe in the function.
#' @examples # coin_data <- get_coin_data(pair)
#' @examples # low_52_week(coin_data)
low_52_week <- function(coin_data) {

  index <- which.min(coin_data$low)
  date <- rownames(coin_data)[index]
  price <- coin_data$low[index]

  answer <- paste0("The 52 week low was $", price, " on ", date)

  return(answer)
}



################################################################################

#' @export
#' @param coin_data Dataframe produced by the "get_coin_data" function.
#' @title Plot Cryptocurrency Trading Volume Data
#' @description Plots cryptocurrency trading volume data using dataframe obtained through "get_coin_data" function using ggplot2.
#' @examples ## Using the get_coin_data() function to obtain dataframe and inserting that dataframe in the function.
#' @examples # coin_data <- get_coin_data(pair)
#' @examples # plot_coin_volume_data(coin_data)
plot_coin_volume_data <- function(coin_data) {

  g <- ggplot(coin_data, aes(x=rownames(coin_data), y=volume)) +
    geom_col() +
    labs(title = glue("{coin_data$coin[1]}", " 24HR Volume"),x = "Dates", y = "24HR Volume") +
    theme(axis.text.x = element_text(angle = 90, size = 6)) +
    scale_x_discrete(breaks = rownames(coin_data)[seq(1, nrow(coin_data), by=4)])

  return(g)
}

#plot_coin_data(bitcoin)

