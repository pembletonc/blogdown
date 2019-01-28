library(tidyverse)
library(readxl)
library(lubridate)


cas1 <- read_excel("./content/space-stats/Project44_Casualty-List_v3.xlsx")

cas1 %>% 
  mutate(date_of_death = dmy(date_of_death)) %>% 
  group_by(rank, date_of_death) %>% 
  count() %>%
  arrange(desc(n)) %>%
  filter(rank == "Rifleman"| rank == "Private" | rank == "Trooper" |
         rank == "Sapper" | rank == "Gunner") %>% 
  ggplot(aes(x = date_of_death, y = n, group = rank)) +
  geom_line(aes(color = rank)) +
  theme_minimal() +
  theme()

