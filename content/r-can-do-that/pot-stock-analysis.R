library(tidyverse)
library(stocks)
library(janitor)
library(lubridate)
library(readxl)
library(tidyquant)

holdings <- read_excel("./content/r-can-do-that/HorizonsHMMJHoldings.xls")

weed_key_ratios <- tq_get(c("GWPH", "IIPR", "INSY", "SMG",
                            "XXII","ZYNE", "CGC", "APHQF", "CRON"), get = "key.ratios")

weed_stock_prices <- tq_get(c("GWPH", "IIPR", "INSY", "SMG",
                               "XXII","ZYNE", "CGC", "APHQF", "CRON"),
                             get = "stock.prices", from = "2000-01-01")

codes <- tq_exchange(c("NASDAQ", "NYSE", "AMEX"))

View(l)


