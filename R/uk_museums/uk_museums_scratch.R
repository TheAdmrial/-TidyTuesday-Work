# uk museums scratch
# loading in libraries
library(tidyverse)
library(lubridate)

# reading in data for the week
dat <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# looking at the data initially
View(dat)
