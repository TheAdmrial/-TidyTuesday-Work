# reading in the packages
library(tidyverse)
library(pander)
#reading in the data
cia_factbook <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv')

View(cia_factbook)

# the data is for the year 2014 only. 
# also the EU is a country along with Germany, Italy, and Luxembourg

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

summary(cia_factbook)

#univariate analysis: charts
#area histogram
cia_factbook %>% 
  ggplot(aes(x = area)) +
  geom_histogram(fill="skyblue", color="black", bins=50)+
  labs(title = 'Distribution of Areas')
# box and whisker plot
cia_factbook %>% 
  ggplot(aes(x = area))+
  geom_boxplot(fill="skyblue", color="red") +
  labs(title = 'Distribution of Areas')

#birth rate
cia_factbook %>% 
  ggplot(aes(x = birth_rate)) +
  geom_histogram(fill="skyblue", color="black", bins=55)+
  labs(title = 'Distribution of Birth Rates')
# box and whisker plot
cia_factbook %>% 
  ggplot(aes(x = birth_rate))+
  geom_boxplot(fill="skyblue", color="red") +
  labs(title = 'Distribution of Areas')

#life expectancy at birth
cia_factbook %>% 
  ggplot(aes(x = life_exp_at_birth)) +
  geom_histogram(fill="skyblue", color="black", bins=55)+
  labs(title = 'Distribution of Life Expectancy at Birth')
# box and whisker plot
cia_factbook %>% 
  ggplot(aes(x = life_exp_at_birth))+
  geom_boxplot(fill="skyblue", color="red") +
  labs(title = 'Distribution of Areas')

# multivariate analysis
cia_factbook %>% 
  select(!country) %>% 
  pairs()

#bivariate analyses
# area x infant_mortality_rate
cia_factbook %>% 
  ggplot(aes(x = area, y = infant_mortality_rate)) +
  geom_point(color = "darkblue")+
  labs(title = 'Area vs Infant Mortality Rate') +
  scale_x_continuous(trans='log10')
  



# infant_mortlaity_rate x lif_exp_at_birth
cia_factbook %>% 
  mutate(
    continent = case_when(
      country %in% c('Russia'
                     , 'Turkey'
                     , 'France'
                     , 'Ukraine'
                     , 'Spain'
                     , 'Sweden'
                     , 'Germany'
                     , 'Finland'
                     , 'Norway') ~ 'Europe'
      , country %in% c(
        'Canada'
        , 'United States'
        , 'Greenland'
        , 'Mexico'
      ) ~ 'North America'
      
      , country %in% c(
        'China'
        , 'India'
        , 'Kazakhstan'
        , 'Saudi Arabia'
        , 'Iran'
        , 'Mongolia'
        , 'Pakistan'
        , 'Burma'
        , 'Afghanistan'
        , 'Yemen'
        , 'Thailand'
        , 'Turkmenistan'
        , 'Uzbekistan'
        , 'Iraq'
        , 'Japan'
        , 'Vietnam'
        , 'Malaysia'
      ) ~ 'Asia'
      
      , country %in% c(
        'Australia'
        , 'Indonesia'
        , 'Papua New Guinea'
      ) ~ 'Oceania'
      
      , country %in% c(
        'Argentina'
        , 'Brazil'
        , 'Peru'
        , 'Colombia'
        , 'Bolivia'
        , 'Venezuela'
        , 'Chile'
        , 'Paraguay'
      ) ~ 'South America'
      
      , country %in% c(
        'Algeria'
        , 'Congo, Democratic Republic of the'
        , 'Sudan'
        , 'Libya'
        , 'Chad'
        , 'Niger'
        , 'Angola'
        , 'Mali'
        , 'South Africa'
        , 'Ethiopia'
        , 'Mauritania'
        , 'Egypt'
        , 'Tanzania'
        , 'Nigeria'
        , 'Namibia'
        , 'Mozambique'
        , 'Zambia'
        , 'South Sudan'
        , 'Somalia'
        , 'Central Africal Republic'
        , 'Madagascar'
        , 'Bostwana'
        , 'Kenya'
        , 'Cameroon'
        , 'Morocco'
        , 'Zimbabwe'
        , 'Congo, Republic of the'
        , "Cote d'Ivoire"
      ) ~ 'Africa'
    )
  ) %>% 
  View() # start on row 70
  ggplot(aes(y = life_exp_at_birth, x = infant_mortality_rate)) +
  geom_point(color = "darkblue")+
  labs(title = 'Infant Mortality Rate vs Life Expectance at Birth'
       , subtitle = 'Taken from the CIA 2014 World Factbook'
       , x = 'Infant Mortality Rate (# deaths infants < 1 year / 1,000 births)' #number of deaths of infants under one year old per 1,000 live births)
       , y = 'Life Expectnace at Birth (Years)') +
  expand_limits(y = 45)






# population x life_exp_at_birth
cia_factbook %>% 
  ggplot(aes(y = life_exp_at_birth, x = population)) +
  geom_point(color = "darkblue")+
  labs(title = 'Area vs Infant Mortality Rate') +
  scale_x_continuous(trans='log10')

  