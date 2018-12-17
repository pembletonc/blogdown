library(rtweet)
library(tidyverse)

create_token(app = "my_twitter_app",
             consumer_key = "slfkHXdbamCcqhqKEQaCHPBG6" ,
             consumer_secret = "cZGc4GAXoM8eqsgiGS8p1UJa9ecvGcC3kfAnSxw16WpHPrdA7h",
             access_token = "2651047838-e2eMzRfExP2oe3Rax4Cn1lI9rmUJ05VfACNtA7g",
             access_secret = "niV33rv5tFXviRqIahvB6eKPEqyeBIKoQVAZTexC2wbFS")


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



