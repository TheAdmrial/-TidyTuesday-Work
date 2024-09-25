# 'importing' the libraries that I need
library(tidyverse)

#Some reading up on Chess is needed
# Here is an article about chess notation: https://www.chess.com/article/view/chess-notation
# Here is an article about opening names, codes, and their explanation: https://www.365chess.com/eco.php
# Here is an article about how to read time increments: https://chess.stackexchange.com/questions/18069/what-is-the-increment-in-chess
chess_col_types <-  list('c'
                     , 'l'
                     # , 'n'
                     # , 'n'
                     , 'i'
                     , 'f'
                     , 'f'
                     , 'f'
                     , 'c'
                     , 'i'
                     , 'c'
                     , 'i'
                     , 'c'
                     , 'c'
                     , 'f'
                     , 'i')
# reading in the chess data
# DONE: victory_status, winner, time_increment all need to be category columns
chess <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv'
                  , col_types = chess_col_types
                  , show_col_types = TRUE
                  , col_select = !c(start_time, end_time))

View(chess)

#### Cleaning the chess data
# TODO: need to convert the start_time and end_time to a datetime column
# When I tried to force read_csv to change the date, I kept getting NA values
# TODO: Add a time elapsed column 
# Can't add a duration column due to the start_time and end_time are the same number
# chess_dat <- 
  chess %>% 
    #getting the opening move from the list of moves
    mutate(
      opening_move = str_subset(chess$moves
                                , '^[:alnum:]{2,3}')
    ) %>% 
    View()
    
#### looking for the first move for white in the move string
str_view(chess$moves
         , '^[:alnum:]{2,3}'
         , html = FALSE)
  
  
  
  