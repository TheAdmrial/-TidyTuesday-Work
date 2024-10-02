# adding in libraries
library(tidyverse)

# reading in the individual results data

country_results_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/country_results_df.csv')
# timeline_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/timeline_df.csv')


# individual_results_df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-09-24/individual_results_df.csv'
                                  # , col_select = !c('p7'))

View(country_results_df)

###### Graphing How country rankings shifted over time ######
## cleaning the data ##

# imo_country_data <- 
country_results_df %>% 
  select(
    c('year'
     , 'country'
     , 'team_size_all'
     , 'awards_gold'
     , 'awards_silver'
     , 'awards_bronze'
     , 'awards_honorable_mentions')
         ) %>% 
  mutate(
    imo_total_score = (awards_gold * 4) + (awards_silver * 3) + (awards_bronze * 2) + (awards_honorable_mentions)
  ) %>% 
  select(
    !c('awards_gold'
       , 'awards_silver'
       , 'awards_bronze'
       , 'awards_honorable_mentions'
       , 'team_size_all')
  ) %>% 
  filter(!is.na(imo_total_score)) %>% 
  group_by(year, country) %>% 
  slice_head(imo_total_score, n = 5) #%>%
  View()
