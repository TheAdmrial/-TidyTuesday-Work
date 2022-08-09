# loading in libraries
library(tidyverse)

# loading in the spiders data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# cleaning up the distribution column

spiders1 <- spiders %>% 
  separate(distribution, sep= ",", into = c("country1", "country2", 'country3', 'country4','country5','country6','country7','country8','country9','country10','country11','country12','country13','country14','country15','country16','country17','country18'), extra = 'merge')

spiders %>% 
  str_view(distribution)