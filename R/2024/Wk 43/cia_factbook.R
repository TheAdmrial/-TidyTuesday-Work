# reading in the packages
library(tidyverse)
library(pander)
#reading in the data
cia_factbook <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv')

View(cia_factbook)

# the data is for the year 2014 only. 
# also the EU is a country along with Germany, Italy, and Luxemborg

# Pandas table would be like the following:
# country area  birth_rate  death_rate  infant_mortality_rate internet_users  life_exp_at_birth maternal_mortality_rate net_migration_rate  population  population_growth_rate
#count
#mean
#std
#min
#25%
#median
#75%
#max
#NA's (count)
#TODO: recreate this table in R

# showing the desciptive stats of the data set
# describe_cia_factbook <- 
cia_factbook %>% 
  filter()
  summarize(
    across(
      `area`:`population_growth_rate`
      , .fns(
        list(
          count = n
          , mean = mean
          , std = sd
          , min = min
          # , '25%' = quantile(x = .x, probs = 0.25)
          # , median = median
          # , '75%' = quantile(x = .x, probs = 0.75)
          # , max = max
          # , n_miss = ~ sum(is.na(.x))
        )
      )
     )
  ) %>% 
  # summary() %>% 
  View()

  