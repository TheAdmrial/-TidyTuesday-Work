# reading in the packages needed
library(tidyverse)
library(ggwordcloud)
library(ggthemes)

# reading in the data
orca_cols <- cols(
  date = col_character()
  , begin_time = col_character()
  , end_time = col_character()  
)

orcas <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-15/orcas.csv'
                  , col_types = orca_cols)

View(orcas)

# Which pods or ecotypes have the longest duration encounters with CWR researchers?
orcas %>%
  # selecting out the columns that I need
  select(
    year
    , date
    , end_time
    , begin_time
    , pods_or_ecotype
    , duration
    , begin_latitude
    , begin_longitude
    , end_latitude
    , end_longitude
  ) %>% 
  # need to combine the date and time to get the total time of encounter in mins
  mutate(
    end_datetime = ymd_hms(paste0(orcas$date, orcas$end_time))
    , start_datetime = ymd_hms(paste0(orcas$date, orcas$begin_time))
  ) %>%
  # the end_datetime needs to be greater than the start_datetime. 
  # if end_datetime < start_datetime then add a day to the end_datetime. 
  # (most likely this means that the end_time happened the next day.)
  mutate(
    end_datetime = if_else(condition = end_datetime < start_datetime
                           , true = end_datetime + days(1)
                           , false = end_datetime)
    , duration_mins = end_datetime - start_datetime
  ) %>% 
  # splitting out the list of different pods that were seen in the same encounter
  separate_longer_delim(
    cols = pods_or_ecotype
    , delim = regex(',|and')
  ) %>%
  # just some string cleaning
  mutate(
    pods_or_ecotype = str_trim(pods_or_ecotype)
    , pods_or_ecotype = str_remove(pods_or_ecotype, regex('Pod|pod|pods|Pods'))
    , pods_or_ecotype = str_remove(pods_or_ecotype, regex("'s"))
    , pods_or_ecotype = str_to_title(pods_or_ecotype)
  ) %>% 
  filter(
    !is.na(duration_mins)
    , !is.na(pods_or_ecotype)
  ) %>% 
  group_by(year, pods_or_ecotype) %>% 
  summarize(
    total_encounter_duration = sum(duration_mins)
  ) %>% 
  mutate(
    duration_rank = min_rank(total_encounter_duration)  
  ) %>% 
  ungroup() %>% 
  filter(
    duration_rank %in% c(1)
  ) %>% 
  ggplot(aes(color = pods_or_ecotype)) +
  geom_text_wordcloud(aes(label = pods_or_ecotype), size = 7) +
  # geom_text_wordcloud_area(aes(label = total_encounter_duration), nudge_y = 15 , size = 7)+
  facet_wrap(~year) +
  theme_solarized(light = FALSE) 

# Are there trends in where orca encounters occur over time?


# Which Orca/orcas was/were most commonly encountered? 



# Which other Orcas were also 'friendly'(Encountered most often)? 