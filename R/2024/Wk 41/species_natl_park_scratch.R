library(tidyverse)
library(ggwordcloud)
library(ggthemes)

# reading in the data
most_visited_nps_species_data <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-08/most_visited_nps_species_data.csv')

most_visited_nps_species_data %>% 
  View()


##### Graph/table 1: Number of Non/native Species in each park
non_or_native_species_by_park <- most_visited_nps_species_data %>%
  select(ParkName, Nativeness) %>% # maybe add the number of non/native per species per park
  mutate(
    Nativeness = replace_na(most_visited_nps_species_data$Nativeness, 'Unknown')
  ) %>% 
  group_by(ParkName, Nativeness) %>% 
  summarize(
    count_of_each = n()
  ) 

non_or_native_species_by_park %>% 
  ggplot(aes(x = Nativeness, y = count_of_each, fill = Nativeness)) +
  geom_col() +
  facet_wrap(~ParkName) +
  scale_y_continuous(trans='log10')

##### Graph/Table 2: Number of Rare and occasional species per park

abundance_categories <- c('Abundant', 'Common', 'Uncommon', 'Occacional', 'Rare', 'Unknown')

rare_or_occasional_species_per_park <- most_visited_nps_species_data %>%
  select(ParkName, Abundance) %>% # CategoryName,
  mutate(
    Abundance = factor(Abundance, levels = abundance_categories, ordered = TRUE)
    , Abundance = replace_na(most_visited_nps_species_data$Abundance, 'Unknown')
  ) %>%
  filter(!Abundance %in% c('Unknown')) %>% # not sure if I want to take out the Unknown category
  group_by(ParkName, Abundance) %>% 
  summarize(
    count = n()
  ) 

rare_or_occasional_species_per_park %>% 
  View()
  ggplot(aes(x = Abundance, y = count, fill = Abundance)) +
  geom_col() +
  facet_wrap(~ParkName) #+
  # scale_y_continuous(trans='log10')

##### Graph 3: Of the parks, what is their most rare CategoryName (Min(count(CategoryName)))

most_rare_category_by_park <- most_visited_nps_species_data %>%
  select(ParkName, Abundance, CategoryName) %>% 
  mutate(
    Abundance = replace_na(most_visited_nps_species_data$Abundance, 'Unknown')
  ) %>% 
  filter(
    Abundance %in% c('Rare')
  ) %>% 
  select(!Abundance) %>% 
  group_by(ParkName, CategoryName) %>% 
  mutate(
    count_of_each = n()
  ) %>% 
  distinct() %>% 
  ungroup() %>% 
  group_by(ParkName) %>% 
  slice_min(count_of_each, n = 1) 

most_rare_category_by_park %>% 
  ggplot(aes(label = CategoryName, color = CategoryName)) +
  geom_text_wordcloud(shape = 'diamond', size = 7) +
  facet_wrap(~ParkName) +
  theme_solarized(light = FALSE) +
  theme(
    text = element_text(family = "Optima")
    , face = 'bold'
  ) +
  labs(
    title = "The Most Rare Category per National Park"
  )


