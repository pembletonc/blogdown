library(tidyverse)
library(readxl)
library(lubridate)


cas1 <- read_excel("./content/space-stats/Project44_Casualty-List_v3.xlsx")


#geom_tile

jr_ncm <- c("Rifleman", "Private", "Trooper","Sapper","Gunner" )

cas2 <- cas1 %>% 
  group_by(rank, date_of_death) %>% 
  count() %>%
  arrange(desc(n)) %>%
  ungroup() %>% 
  filter(rank %in% jr_ncm) %>%
  mutate(rank = factor(rank, levels = jr_ncm),
         date_of_death = dmy(date_of_death),
         month_of_death = month(date_of_death))

#geom tile

cas2 %>%
  filter(date_of_death >= "1944-07-01", date_of_death < "1944-08-01") %>% 
  ggplot(aes(x = date_of_death, y = rank, fill = n)) +
  geom_tile(width = .75, height = .75) +
  scale_fill_viridis_c(option = "B", begin = .22, end = .75, 
                       name = "Number of junior NCM casualties") +
  scale_y_discrete(name = NULL) +
  coord_fixed(expand = FALSE) +
  theme_minimal()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 12))


cas2 %>%
  filter(date_of_death >= "1944-07-01", date_of_death < "1944-08-01") %>% 
  ggplot(aes(x = date_of_death, y = rank, fill = n)) +
  geom_tile(width = .75, height = .75, stat = "identity") +
 scale_fill_viridis_d(option = "D", begin = 0, end = 200, 
                       name = "Number of junior NCM casualties") +
  scale_y_discrete(name = NULL) +
  coord_fixed(expand = FALSE) +
  theme_minimal()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        #axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 12))





#geom_line
cas2 %>% 
  ggplot(aes(x = date_of_death, y = n, group = rank)) +
  geom_line(aes(color = rank), lwd = 1) +
  theme_minimal() +
  theme()

