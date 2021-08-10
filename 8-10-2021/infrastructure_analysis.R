#loading in libraries
library(tidyverse)

# loading in the data
investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/investment.csv')
chain_investment <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/chain_investment.csv')
ipd <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-10/ipd.csv')

View(investment)
View(chain_investment)
View(ipd)

unique(investment$category)
unique(investment$meta_cat)
# what does investments look like
investment %>% 
  ggplot(aes(x = year, y = gross_inv, color = meta_cat)) +
  geom_point() +
  geom_line(aes(group = meta_cat))

# looking at just energy related infrastructure aka electric power, natural gas/petroleum power, power 
investment %>% 
  filter(meta_cat %in% c("Electric power", "Natural gas /petroleum power","Power")) %>% 
  ggplot(aes(x = year, y = gross_inv, color = category)) +
  geom_point() +
  geom_line(aes(group = category))
