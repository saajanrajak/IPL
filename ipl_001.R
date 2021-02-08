
# Aim: EDA of IPL Data
# Date: 08-feb-2021
# Author: Saajan 

#load require libraries
library(tidyverse)


# Load the Data -----------------------------------------------------------

raw_ipl_2020 <- read_csv("C:/Users/quest/Downloads/Projects/ipl2020/Deliveries IPL 2020.csv")
raw_ipl_2020_matches <- read_csv("C:/Users/quest/Downloads/Projects/ipl2020/Matches IPL 2020.csv")

df <- raw_ipl_2020
match <- raw_ipl_2020_matches


# explore the data --------------------------------------------------------

#most league matched win by team

match %>% 
  group_by(winner) %>% 
  summarise(count = n()) %>% 
  ungroup() %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(fct_reorder(winner, count),count, fill = winner))+
  geom_bar(stat = "identity")+
  labs(x = 'Teams', y= 'Win Count', title = 'League Matches IPL', 
       subtitle = "Year 2020", caption = "Data source: data world")+
  theme(legend.position = "none")

# How much Toss affect the result
match %>% 
  group_by(toss_winner) %>% 
  summarise(toss_winner_count = n()) %>% 
  ungroup() %>% 
  full_join(match %>% 
              filter(toss_winner == winner) %>% 
              group_by(winner) %>% 
              summarise(win_count = n()) %>% 
              ungroup(), by = c("toss_winner" ="winner"))  %>% 
  mutate(win_percentage = round(win_count/toss_winner_count*100,2)) %>% 
  ggplot(aes(fct_reorder(toss_winner, win_percentage),win_percentage, fill = toss_winner))+
  geom_bar(stat = "identity")


# How much Toss affect the result
match %>% 
  group_by(toss_winner, venue) %>% 
  summarise(toss_winner_count = n()) %>% 
  ungroup() %>% 
  full_join(match %>% 
              filter(toss_winner == winner) %>% 
              group_by(winner, venue) %>% 
              summarise(win_count = n()) %>% 
              ungroup(), by = c("toss_winner" ="winner", 'venue'))  %>% 
  mutate(win_percentage = round(win_count/toss_winner_count*100,2)) %>% 
  ggplot(aes(fct_reorder(toss_winner, win_percentage),win_percentage,
             fill = toss_winner, group = venue))+
  geom_bar(stat = "identity")+
  labs(x = '', y = 'Percentage', title = "League Match Toss win percentage",
       subtitle = "Year 2020", caption = "data source: data world")+
  facet_wrap(~ venue)+
  theme(legend.position = "none")
  coord_flip()
  
  
  




