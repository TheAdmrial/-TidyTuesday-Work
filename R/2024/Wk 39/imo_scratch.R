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

imo_country_data <- country_results_df %>%
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
  group_by(year) %>% 
  slice_max(imo_total_score
            , n = 3
            , na_rm = TRUE
            , with_ties = TRUE) %>% 
  ungroup() #%>% 

###### Making the time series graph ######

top_countries <- c('United States of America' = '#377eb8'
                   , 'Russian Federation' = '#4daf4a'
                   , "People's Republic of China" = '#e41a1c'
                   #, 'Republic of Korea' = '#984ea3'
                   )
  
  
imo_country_data %>% 
    filter(country %in% c('United States of America'
                          , 'Russian Federation'
                          , "People's Republic of China"
                          # , 'Republic of Korea'
                          )) %>% 
  ggplot(aes(x = year, y = imo_total_score, color = country)) +
  geom_jitter(width = 0.2, height = 0) +
  geom_line(aes(color = country, group = country)) +
  scale_fill_manual(values=top_countries) +
  theme_minimal() +
  labs(title = "International Mathematic Olymipad (IMO) Total Scores Over Time by Country"
       , x = "Year"
       , y = "IMO Total Score"
       , color = "Country"
       , caption = "The total score was calculated by assigning 4, 3, 2, and 1 point values\nto the number of Gold, Silver, Bronze, and Honorable Mentions won.\nThis was an arbitrary value assignment.")

