library(tidyverse)
library(readxl)
library(lubridate)
library(janitor) #devtools::install_version("janitor", version = "1.0.0", repos = "http://cran.us.r-project.org")
library(scales)
library(ruler)
library(extrafont)

extrafont::loadfonts()


# Instructions

#1. run the entire script titled "00 - Loading and Cleaning"
#2. Run this entire script
#3. note the function for building the plot requires minor changes for different data reprsentations,
#such as including x-axis titles or not. Adjust accoringly.

#The main questions for me are why Submission to Endorsement has become longer in 2016 
#before shortening again in 2017. And then why Endorsement to Approval are longer 
#in 2016 and 2017 than in 2015, eating away at the big gain realised in
#Effectiveness to Disbursement.

fire_incidents <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/cal-fire-incidents.csv")
fire_damage <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018-08-21/calfire_damage.csv")

fire_data <- fire_damage %>% 
  left_join(fire_incidents, by = c("year" = "YEAR")) %>% 
  select(year, structures, "number_of_fires" = "NUMBER OF FIRES",
         "acres_burned" = "ACRES BURNED", "dollar_damage" = "DOLLAR DAMAGE")


#function creation----
#define what isn't an outlier based on z-score
#"Observation is not an outlier based on z-score if its absolute value of default 
#z-score is lower then some threshold '

not_out_z <- function(x, thres = 3, na.rm =TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * sd(x, na.rm = na.rm) 
}

#define what isnt' an outlier based on mad
#Observation is not an outlier based on MAD if its absolute value of z-score with median as center and MAD as normalization unit is lower then some threshold (popular choice is 3).

not_out_mad <- function(x, thres = 3, na.rm = TRUE){
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

#define what isn't an outlier based on Tukey's fences

not_out_tukey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}

maha_dist <- . %>% select_if(is.numeric) %>% 
  mahalanobis(center = colMeans(.), cov = cov(.))

not_out_maha <- function(tbl, not_out_f, ...) {
  tbl %>% maha_dist() %>% not_out_f(...)
}

not_outlier_funs <- funs(
  z = not_out_z,
  mad = not_out_mad,
  tukey = not_out_tukey
)

glimpse(fire_data)

fire_data %>%   
  transmute_if(is.numeric, not_outlier_funs)


fire_data %>% 
  transmute(maha = maha_dist(.)) %>% 
  transmute_at(vars(maha = maha), not_outlier_funs)

#group by year, ungroup

glimpse(fire_data)

data_tbl <- fire_data %>% 
  unite(col = "group", year)

glimpse(data_tbl)

compute_group_non_outliers <- . %>%
  # Compute per group mean values of columns
  group_by(group) %>%
  summarise_if(is.numeric, mean) %>%
  ungroup() %>%
  # Detect outliers among groups
  mutate_if(is.numeric, 
            not_outlier_funs) %>% 
  # Remove unnecessary columns
  select_if(Negate(is.numeric))


data_tbl %>% compute_group_non_outliers()

row_packs_not_out <- row_packs(
  column = . %>% transmute_if(is.numeric, not_outlier_funs),
  maha = . %>% transmute(maha = maha_dist(.)) %>% 
    transmute_at(vars(maha = maha), not_outlier_funs)
)

group_packs_not_out <- group_packs(
  group = compute_group_non_outliers,
  .group_vars = "group"
)

#applying these packs of rulesets established is called the exposing process:


full_report <- data_tbl %>% 
  expose(row_packs_not_out, group_packs_not_out,
         .remove_obeyers = FALSE) %>% 
  get_report()

View(full_report)

used_rules <- full_report %>% 
  distinct(pack, rule)

#breaker report contains data about units which break certain rules
breaker_report <- full_report %>% 
  filter(!(value %in% TRUE))

group_breakers <- breaker_report %>% 
  filter(pack == "group") %>% 
  select(-id) %>% 
  left_join(
    y = data_tbl %>% transmute(var = group, id = 1:n()),
    by = "var"
  ) %>% 
  select(pack, rule, var, id, value)

group_breakers

outliers <- bind_rows(
  breaker_report %>% 
  filter(pack != "group"), 
  group_breakers
) %>% 
  select(pack, rule, id)

#we can see how not all group based definitions resulted with outliers
outliers %>% 
  count(pack, rule) %>% 
  filter(pack == "group") %>% 
  print(n = Inf)

#outliers can tell us the outlier rows

outliers %>% 
  count(pack, rule, sort = TRUE)

outlier_score <- outliers %>% 
  group_by(id) %>% 
  summarise(score = n() / nrow(used_rules))

#top 10 outliers

outlier_score %>% 
  arrange(desc(score)) %>% 
  slice(1:10)

#tag outliers as `strong outliers` those with a score of more than 0.2

fire_data_outliers <- fire_data %>% 
  mutate(id = 1:n()) %>% 
  left_join(y = outlier_score, by = "id") %>% 
  mutate(
    score = coalesce(score, 0),
    is_out = if_else(score > 0.2, "Outlier", "Not outlier")
  )

View(fire_reports)

#31 outliers at 0.2
sum(strong_outliers$score > 0.2)

#most extreme outliers (for label)

extreme_outliers <- fire_data %>% 
  filter(score > .2)

#plot it out


fire_data_outliers %>% 
ggplot(aes_string("year", "structures", colour = "is_out")) +
  geom_point(data = fire_data_outliers, aes(size = structures)) +
  scale_colour_manual(values = c("#AAAAAA", "#004080")) +
  scale_x_continuous(breaks = seq(1989, 2017, by = 2), expand = expand_scale(mult = c(.1, .1))) +
  guides(colour = guide_legend(title = NULL,
                               override.aes = list(size = 4))) +
  labs(title = "Strong outliers illustration by ") +
  theme_minimal(base_family = "Lato") + 
  labs(title = "Strong outliers illustrated by number of \nstructures destroyed in California Fires") +
  ylab("# of structures destroyed") +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.ticks = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_text(size = 10, angle = 45)
  )

