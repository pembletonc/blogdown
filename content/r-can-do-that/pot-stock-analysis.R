library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)
library(tidyquant)

devtools::install_github("business-science/tidyquant")



#holdings <- read_excel("./content/r-can-do-that/HorizonsHMMJHoldings.xls")
#codes <- tq_exchange(c("NASDAQ", "NYSE", "AMEX"))

weed_key_ratios <- tq_get(c("GWPH", "IIPR", "INSY", "SMG",
                            "XXII","ZYNE", "CGC", "APHQF", "CRON"), 
                          get = "key.ratios", from = "2014-01-01")

weed_stock_prices <- tq_get(c("GWPH", "IIPR", "INSY", "SMG",
                               "XXII","ZYNE", "CGC", "APHQF", "CRON"),
                             get = "stock.prices.canada", from = "2000-01-01")


weed_stock_prices$symbol

weed_stock_prices %>% 
  group_by(symbol) %>% 
  count(sort = TRUE)

weed_stock_prices %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line(aes(col = symbol)) +
  theme_tq(base_family = "Lato") +
  scale_x_date(labels = scales::date_format("%m-%Y"), date_breaks = "1 year", "Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Daily Closing Value (USD)")


weed_stock_prices %>%
  filter(date > "2013-12-31") %>% 
  ggplot(aes(x = date, y = close)) +
  geom_line(aes(col = symbol)) +
  theme_tq(base_family = "Lato") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text()) +
  ylab("Daily Closing Value (USD)")


#CAPM-----
weed_stock_prices <- tq_get(c("GWPH", "IIPR", "INSY", "SMG",
                              "XXII","ZYNE", "CGC", "APHQF", "CRON"),
                            from = "2014-01-01", to = "2018-08-13")

performance <- weed_stock_prices %>% 
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               col_rename = "Ra")


baseline <- "MJ" %>% 
  tq_get(get = "stock.prices",
         from = "2014-01-01",
         to = "2018-08-13") %>% 
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = "monthly",
               col_rename = "Rb")

glimpse(baseline)

RaRb <- left_join(performance, baseline, by = "date")


RaRb_capm <- RaRb %>%
  tq_performance(Ra = Ra, 
                 Rb = Rb, 
                 performance_fun = table.CAPM)

pal <-  wesanderson::wes_palette(9, name = "FantasticFox1", type = "continuous")

RaRb_capm %>%
  ggplot(aes(x = Alpha, y = AnnualizedAlpha)) +
  geom_point(aes(col = symbol)) +
  geom_smooth(method = "lm", lwd = .5, alpha = .3, col = "purple")+
  theme_tq(base_family = "Lato") +
  scale_fill_discrete(pal = pal) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text()) +
  ggtitle("CAPM Alpha Values of Portfolio Against MJ ETF")

RaRb_capm %>%
  ggplot(aes(x = `Beta`, y = `Correlation`)) +
  geom_point(aes(col = symbol)) +
  geom_smooth(method = "lm", lwd = .5, alpha = .3, col = "purple")+
  theme_tq(base_family = "Lato") +
  scale_fill_tq()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text()) +
  ggtitle("CAPM Beta Correlation Values of Portfolio Against MJ ETF")

RaRb_capm$Correlation

#key weed ratios----

tribble(~`Key Ratio`, ~Definition,
        "Financials", "These ratios include gross margin %, operating margin %, EPS, book value per share, and more.",
        "Profitability", "These ratios include margin as a percentage of sales (gross margin, operating margin, EBT margin, etc) and profitability metrics such as tax rate %, asset turnover, ROA, financial leverage, ROE, return on invested capital, and more.",
        "Growth", "These ratios include year over year, 3-year average, 5-year average, and 10-year average growth rates for revenue, operating income, net income, and EPS.",
        "Cash Flow", "These ratios include operating cash flow growth % YOY, free cash flow growth % YOY, capital expenditure as a % of sales, and more.",
        "Financial Health", "These ratios include balance sheet items as a percentage of total assets and liabilities, and liquidity/financial health metrics such as current ratio, quick ratio, debt/equity, and financial leverage.",
        "Efficiency Ratios", "These ratios include days sales outstanding, days inventory, inventory turnover, asset turnover and more.",
        "Valuation Ratios", "These ratios include price to earnings (P/E), price to sales (P/S), price to book (P/B), and price to operating cash flow.")







weed_key_ratios %>% 
  filter(section == "Profitability") %>% 
  unnest() %>% 
  filter(date > "2013-12-31") %>% 
  #group_by(category) %>% 
  count(sort = TRUE)

weed_key_ratios %>% 
  filter(section == "Profitability" ) %>% 
  unnest() %>% 
  filter(date > "2013-12-31"& category == "Return on Assets %") %>% 
  group_by(category) %>% 
  count(sort = TRUE)

pal <-  wesanderson::wes_palette(8, name = "FantasticFox1", type = "continuous")

weed_key_ratios %>% 
  filter(section == "Profitability") %>% 
  unnest() %>% 
  filter(date > "2013-12-31" & category == "Return on Assets %" & sub.section == "Profitability") %>% 
  ggplot(aes(x = date, y = value)) +
  geom_line(aes(col = symbol), lwd = .75) +
  #facet_wrap(~ symbol, ncol = 2, scale = "free_y") + 
  theme_tq(base_family = "Lato") +
  scale_x_date(labels = scales::date_format("%Y"), date_breaks = "1 year") +
  ggtitle("Percentage of Return on Assets, 2014 - 2018") +
  scale_fill_continuous(pal = pal) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(legend.text = element_text()) +
  ylim(-100, 100) +
  ylab("Return on Assets %") 

wesanderson::wes_palettes





