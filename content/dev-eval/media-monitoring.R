library(rtweet)
library(tidyverse)


rt <- search_tweets("evaluation", n = 5000, include_rts = FALSE,
                    retryonratelimit = FALSE)

ts_plot(rt, "3 hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #evaluation Twitter statuses from past 9 days",
    subtitle = "Twitter status (tweet) counts aggregated using three-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


ts_pl



