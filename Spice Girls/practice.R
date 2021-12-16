# load libraries
library(tidyverse)

# all the spice girls are: Scary, Baby, Ginger, Posh, Sporty

# load in the data
studio_album_tracks <- readr::read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/studio_album_tracks.csv")
lyrics <- read_csv("https://github.com/jacquietran/spice_girls_data/raw/main/data/lyrics.csv")
# what do I want to look at with the spice girls?
# dance music? dance and sing music?
# common words/topics?
# who sings the most? 

# View(studio_album_tracks)
# View(lyrics)

dat <- studio_album_tracks %>% 
  group_by(key_mode) %>% 
  summarize(num_of_keys = n())

studio_album_tracks %>% 
  group_by(key_name) %>%
  summarize(average_dancability = mean(danceability)) %>% 
  ggplot() +
  geom_col(aes(x = key_name, y = average_dancability))

studio_album_tracks %>% 
  group_by(key_mode) %>%
  summarize(average_dancability = mean(danceability))%>% 
  ggplot(aes(x = reorder(key_mode, -average_dancability), y = average_dancability)) +
  geom_col()

# all the spice girls are: Scary, Baby, Ginger, Posh, Sporty
dat1 <- lyrics %>% 
  mutate(section_artist = str_replace_all(section_artist, '&', ','),
         section_artist = str_replace_all(section_artist, ' \\(', ''),
         section_artist = str_replace_all(section_artist, '\\)', ''),
         section_artist = str_replace_all(section_artist, 'with|and', ','),
         section_artist = str_replace_all(section_artist, '\\s', '')) %>% 
  separate(section_artist,c('singer1','singer2','singer3','singer4','singer5','singer6','singer7','singer8')) %>%
  mutate(singer_name = singer1) %>% 
  pivot_wider(names_from = singer_name,
              values_from = c('singer1','singer2','singer3','singer4','singer5','singer6','singer7','singer8'), 
              values_fill = 0,
              names_glue = '{.value}')

lyrics %>% 
  mutate(section_artist = str_replace_all(section_artist, '&', ','),
         section_artist = str_replace_all(section_artist, ' \\(', ''),
         section_artist = str_replace_all(section_artist, '\\)', ''),
         section_artist = str_replace_all(section_artist, 'with|and', ','),
         section_artist = str_replace_all(section_artist, '\\s', '')) %>% 
  mutate(Scary = case_when(
    str_detect(section_artist, "Scary") ~ 1,
    str_detect(section_artist, "All") ~ 1,
    TRUE ~ 0
  ),
  Sporty = case_when(
    str_detect(section_artist, "Sporty") ~ 1,
    str_detect(section_artist, "All") ~ 1,
    TRUE ~ 0
  )) %>% View()
  

  