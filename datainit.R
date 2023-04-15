library(httr)
library(xml2)
library(stringr)
library(ggplot2)
library(dplyr)

url <- "https://coinranking1.p.rapidapi.com/coins"

coin_query_string <- list(
  referenceCurrencyUuid = "yhjMzLPhuIDl",
  timePeriod = "24h",
  orderBy = "marketCap",
  orderDirection = "desc",
  limit = "50",
  offset = "0"
)

query_coins <- VERB("GET", url, add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1', 'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'), query = coin_query_string, content_type("application/octet-stream"))

coins <- httr::content(query_coins)
coins <- as.character(coins)

ids <- stringr::str_extract_all(coins, "uuid = \\\"[A-Za-z0-9_-]+\\\"")[[2]]
ids <- stringr::str_remove_all(ids, "uuid = \\\"|\\\"")

symbols <- stringr::str_extract_all(coins, "symbol = \\\"[A-Za-z0-9]+\\\"")[[2]]
symbols <- stringr::str_remove_all(symbols, "symbol = \\\"|\\\"")

names <- stringr::str_extract_all(coins, "name = \\\"[A-Za-z0-9 .()]+\\\"")[[2]]
names <- stringr::str_remove_all(names, "name = \\\"|\\\"")

colors <- stringr::str_extract_all(coins, "color = (\\\"[#A-Za-z0-9]+\\\")|NULL")[[2]]
colors <- stringr::str_remove_all(colors, "color = \\\"|\\\"")

tiers <- stringr::str_extract_all(coins, "tier = [0-9]")[[2]]
tiers <- stringr::str_remove_all(tiers, "tier = ")

prices <- stringr::str_extract_all(coins, "price = (\\\"[0-9.]+\\\")")[[2]]
prices <- as.numeric(stringr::str_remove_all(prices, "price = \\\"|\\\""))

volume24h <- stringr::str_extract_all(coins, "`24hVolume` = (\\\"[0-9.]+\\\")")[[2]]
volume24h <- as.numeric(stringr::str_remove_all(volume24h, "`24hVolume` = \\\"|\\\""))

changes <- stringr::str_extract_all(coins, "change = (\\\"-?[0-9.]+\\\")")[[2]]
changes <- as.numeric(stringr::str_remove_all(changes, "change = \\\"|\\\""))

marketCap <- stringr::str_extract_all(coins, "marketCap = (\\\"[0-9.]+\\\")")[[2]]
marketCap <- as.numeric(stringr::str_remove_all(marketCap, "marketCap = \\\"|\\\""))

database <- data.frame(
  ids, symbols, names, colors, tiers, prices, volume24h, changes, marketCap
)

max_amount <- c()
current_amount <- c()
circulatings <- c()
number_of_markets <- c()
number_of_exchanges <- c()

for (id in database$ids) {
  supply_url <- paste("https://coinranking1.p.rapidapi.com/coin/", id, sep = "")
  supply <- VERB("GET",
                 supply_url, 
                 add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                             'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                 content_type("application/octet-stream")
  )
  supply <- httr::content(supply)
  supply <- as.character(supply)
  max_amount_id <- stringr::str_extract_all(supply, "max = \\\"[0-9.]+\\\"|max = NULL")[[2]]
  max_amount_id <- as.numeric(stringr::str_remove_all(max_amount_id, "max = |\\\"|\\\""))
  current_amount_id <- stringr::str_extract_all(supply, "total = \\\"[0-9.]+\\\"|total = NULL")[[2]]
  current_amount_id <- as.numeric(stringr::str_remove_all(current_amount_id, "total = |\\\"|\\\""))
  circulating <- stringr::str_extract_all(supply, "circulating = \\\"[0-9.]+\\\"|circulating = NULL")[[2]]
  circulating <- as.numeric(stringr::str_remove_all(circulating, "circulating = |\\\"|\\\""))
  market_number <- stringr::str_extract_all(supply, "numberOfMarkets = [0-9.]+|numberOfMarkets = NULL")[[2]]
  market_number <- as.numeric(stringr::str_remove_all(market_number, "numberOfMarkets = "))
  exchanges_number <- stringr::str_extract_all(supply, "numberOfExchanges = [0-9.]+|numberOfExchanges = NULL")[[2]]
  exchanges_number <- as.numeric(stringr::str_remove_all(exchanges_number, "numberOfExchanges = "))
  max_amount <- c(max_amount, max_amount_id)
  current_amount <- c(current_amount, current_amount_id)
  circulatings <- c(circulatings, circulating)
  number_of_markets <- c(number_of_markets, market_number)
  number_of_exchanges <- c(number_of_exchanges, exchanges_number)
}

database$max <- max_amount
database$current <- current_amount
database$circulatings <- circulatings
database$market_number <- number_of_markets
database$exchange_number <- number_of_exchanges
database$frequency_exchange <- ifelse(database$volume24h > median(database$volume24h), "high", "low")
database <- database %>% mutate(`V/MC` = volume24h/marketCap)

max_change <- database[which.max(database$changes), ]$ids
max_volume <- database[which.max(database$volume24h), ]$ids
max_price <- database[which.max(database$prices), ]$ids
min_change <- database[which.min(database$changes), ]$ids
min_volume <- database[which.min(database$volume24h), ]$ids
min_price <- database[which.min(database$prices), ]$ids

database <- database %>% filter(ids %in% c(max_change, max_volume, max_price, min_change, min_volume, min_price))

query_string <- list(timePeriod = "5y") # Extract the last five years data

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", max_change, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream")
)
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- max_change
all_history_price <- history_price 

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", max_volume, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream"))
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- max_volume
all_history_price <- bind_rows(all_history_price, history_price)

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", max_price, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream"))
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- max_price
all_history_price <- bind_rows(all_history_price, history_price)

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", min_change, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream"))
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- min_change
all_history_price <- bind_rows(all_history_price, history_price)

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", min_volume, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream"))
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- min_volume
all_history_price <- bind_rows(all_history_price, history_price)

price_url <- paste("https://coinranking1.p.rapidapi.com/coin/", min_price, "/history", sep="")
history_price <- VERB("GET",
                      price_url, 
                      add_headers('X-RapidAPI-Key' = '9c9d129698mshb8275bce0446cbcp11a9c2jsn311a96af71b1',
                                  'X-RapidAPI-Host' = 'coinranking1.p.rapidapi.com'),
                      query = query_string,
                      content_type("application/octet-stream"))
history_price <- httr::content(history_price)
history_price <- as.character(history_price)
price <- stringr::str_extract_all(history_price, "price = (\\\"[0-9.]+\\\")|price = NULL")[[2]]
price <- stringr::str_remove_all(price, "price = |\\\"|\\\"")
timestamp <- stringr::str_extract_all(history_price, "timestamp = [0-9]+")[[2]]
timestamp <- stringr::str_remove_all(timestamp, "timestamp = ")
history_price <- data.frame(price, timestamp)
history_price$timestamp <- as.POSIXct(as.integer(history_price$timestamp), origin="1970-01-01", tz="UTC")
history_price$price <- as.numeric(history_price$price)
history_price$ids <- min_price
all_history_price <- bind_rows(all_history_price, history_price)

database <- database%>%
  left_join(all_history_price, by = "ids")