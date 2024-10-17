# loading in libraries
library(tidyverse)

# loading in the spiders data
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# cleaning up the distribution column

spiders1 <- spiders %>% 
  separate(distribution, sep= "\\(", into = "distribution") %>% 
  separate(distribution, sep = "[[:punct:]]|[[:punct:]](?=\\s)|(?<=\\s)[[:alpha:]]{2}(?=\\s)", into = c('country1','country2','country3','country4','country5','country6','country7','country8','country9','country10','country11'))

str_view_all("Ukraine, Russia", "[:punct:](?=\\s)|(?<=\\s)[:alpha:]{2}(?=\\s)")
