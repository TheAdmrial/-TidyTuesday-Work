##ULTRA TRAIL RUNNING##
#LOAD IN LIBRARIES

library("tidyverse")

# read in the data
ultra_rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/ultra_rankings.csv')
race <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-26/race.csv')

# looking at data
view(ultra_rankings)
view(race)

# basic wrangling
race2 <- race %>% 
  group_by(country) %>% 
  mutate(num_races_country = n()) %>% 
  filter(num_races_country > 20)



view(race3)
# initial visuals
# Top 10 popular countries
race2 %>% 
  ggplot() +
  geom_bar(aes(x = reorder(country, -num_races_country))) +
  labs(x = "Countries",
       y = "Number of Races",
       title = "Top 10 Ultra Marithon Countries")

# Top Ultra race cities
# wrangling
race3 <- race2 %>% 
  ungroup() %>% 
  group_by(city) %>% 
  mutate(num_races_city = n()) %>% 
  filter(!is.na(city),
         num_races_city >8)
# graphing
race3 %>% 
  ggplot() +
  geom_bar(aes(x = reorder(city, -num_races_city)))


