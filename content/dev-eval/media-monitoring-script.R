library(tidyverse)
library(rtweet)
library(ggraph)
library(igraph)
library(scales)
library(hrbrthemes)

extrafont::loadfonts(device = "win")


tweets <- search_tweets("#COP24", n = 10000, include_rts = FALSE,
                        retryonratelimit = FALSE)

saveRDS(tweets)

View(tweets)


ts_plot(tweets, "6 hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #COP24 Twitter statuses",
    subtitle = "Twitter status (tweet) counts aggregated using 3-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

filter(tweets, retweet_count > 0) %>% 
  select(screen_name, mentions_screen_name) %>%
  unnest(mentions_screen_name) %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  igraph::graph_from_data_frame() -> rt_g


#clean up the names, and only use the names with X amount of retweets
V(rt_g)$node_label <- unname(ifelse(degree(rt_g)[V(rt_g)] > 50, names(V(rt_g)), "")) 
V(rt_g)$node_size <- unname(ifelse(degree(rt_g)[V(rt_g)] > 50, degree(rt_g), 0)) 

ggraph(rt_g, layout = 'linear', circular = TRUE) + 
  geom_edge_arc(edge_width=0.125, aes(alpha=..index..)) +
  geom_node_label(aes(label=node_label, size=node_size),
                  label.size=0, fill="#ffffff66", segment.colour="springgreen",
                  color="slateblue", repel=TRUE, fontface="bold") +
  coord_fixed() +
  scale_size_area(trans="sqrt") +
  labs(title="Retweet Relationships", subtitle="Most retweeted screen names labeled.\nDarkers edges == more retweets. Node size == larger degree") +
  theme_graph() +
  theme(legend.position="none")

#influence snapshot

influence_snapshot <- function(user, trans = c("log10", "identity")){
  user <- user[1]
  trans <- match.arg(tolower(trimws(trans[1])), c("log10", "identity"))
  
  user_info <- lookup_users(user)
  
  user_followers <- get_followers(user_info$user_id)
  
  uf_details <- lookup_users(user_followers$user_id)
  
  primary_influence <- scales::comma(sum(c(uf_details$followers_count, user_info$followers_count)))
  
    plot <- 
      filter(uf_details, followers_count > 0) %>% 
    ggplot(aes(followers_count)) +
    geom_density(aes(y = ..count..), color = "lightslategray", fill = "lightslategray",
                 alpha = .66, size = 1 ) +
    theme_minimal() +
    scale_x_continuous(expand = c(0,0), trans = "log10", labels = scales::comma, limits = c(1, 10000000)) +
    scale_y_continuous(labels = scales::comma) +
  labs(
      x = "Number of Followers of Followers (log scale)", 
      y = "Number of Followers",
      title = sprintf("Follower chain distribution of %s (@%s)", user_info$name, user_info$screen_name),
      subtitle=sprintf("Follower count: %s; Primary influence/reach: %s", 
                       scales::comma(user_info$followers_count),
                       scales::comma(primary_influence))
    ) +
    theme(axis.title.x = element_text(hjust = 0, size = .5, face = "bold"))
    
  
  print(plot)
  
  return(invisible(list(user_info=user_info, follower_details=uf_details)))
  
  
}

influence_snapshot("coreypembleton")

#ggsave("testsize.png", plot = last_plot(), width = 10, height = 5, dpi = 500)








