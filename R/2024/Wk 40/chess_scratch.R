# 'importing' the libraries that I need
library(tidyverse)
library(ggthemes)
#Some reading up on Chess is needed
# Here is an article about chess notation: https://www.chess.com/article/view/chess-notation
# Here is an article about opening names, codes, and their explanation: https://www.365chess.com/eco.php
# Here is an article about how to read time increments: https://chess.stackexchange.com/questions/18069/what-is-the-increment-in-chess
chess_col_types <-  list('c' # game_id
                     , 'l' # rated
                     # , 'n' # start_time
                     # , 'n' # end_time
                     , 'i' # turns
                     , 'f' # victory_status
                     , 'f' # winner
                     , 'f' # time_increment
                     , 'c' # white_id
                     , 'i' # white_rating
                     , 'c' # black_id
                     , 'i' # black_rating
                     , 'c' # moves
                     , 'c' # opening_eco
                     , 'c' # opening_name
                     , 'i') # opening_ply
# reading in the chess data

chess <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-01/chess.csv'
                  # , col_types = chess_col_types
                  , show_col_types = TRUE
                  , col_select = !c(start_time, end_time)
                  )

View(chess)

#### looking for the first move for white in the move string
#### Testing the regex to find the first move
# filter to just ranchowangdu in white_id
# and filter to just ab2 in white_id

regex_testing <- chess %>%
  filter(
    white_id %in% c('ranchowangdu', 'ab2')
  ) %>% 
  head(6)

str_view(regex_testing$moves
         , '^[:alnum:]{2,3}'
         , html = FALSE)

#### Cleaning the chess data
# TODO: victory_status, winner, time_increment, opening_eco all need to be factor columns
chess_dat <- chess %>%
    #getting the opening move from the list of moves
    mutate(
      opening_move = str_extract(chess$moves
                                , '^[:alnum:]{2,3}')
      , victory_status = as.factor(chess$victory_status)
      , winner = as.factor(chess$winner)
      , time_increment = as.factor(chess$time_increment)
      , opening_eco = as.factor(chess$opening_eco)
      , international_title_white = case_when(
          white_rating >= 2500 ~ 'Grandmaster'
          , white_rating < 2500 & white_rating >= 2400 ~ 'International Master'
          , white_rating < 2400 & white_rating >= 2300 ~ 'FIDE Master'
          , white_rating < 2300 & white_rating >= 2200 ~ 'Candidate Master'
          , white_rating < 2200 & white_rating >= 2000 ~ 'Expert'
          , white_rating < 2000 & white_rating >= 1800 ~ 'Class A Category 1'
          , white_rating < 1800 & white_rating >= 1600 ~ 'Class B Category 2'
          , white_rating < 1600 & white_rating >= 1400 ~ 'Class C Category 3'
          , white_rating < 1400 & white_rating >= 1200 ~ 'Class D Category 4'
          , white_rating < 1200 & white_rating >= 1000 ~ 'Class E Category 5'
          , white_rating < 1000 ~ 'Novice'
        ), international_title_black = case_when(
          black_rating >= 2500 ~ 'Grandmaster'
          , black_rating < 2500 & black_rating >= 2400 ~ 'International Master'
          , black_rating < 2400 & black_rating >= 2300 ~ 'FIDE Master'
          , black_rating < 2300 & black_rating >= 2200 ~ 'Candidate Master'
          , black_rating < 2200 & black_rating >= 2000 ~ 'Expert'
          , black_rating < 2000 & black_rating >= 1800 ~ 'Class A Category 1'
          , black_rating < 1800 & black_rating >= 1600 ~ 'Class B Category 2'
          , black_rating < 1600 & black_rating >= 1400 ~ 'Class C Category 3'
          , black_rating < 1400 & black_rating >= 1200 ~ 'Class D Category 4'
          , black_rating < 1200 & black_rating >= 1000 ~ 'Class E Category 5'
          , black_rating < 1000 ~ 'Novice'
        )
    )
    
#### Exploritory Data Analysis
## Answer the following questions to start
# 1. What are the common opening moves? By rank? 
# I decided to map the white and black rankings against the FIDE Rank ranges
# https://en.wikipedia.org/wiki/Chess_rating_system

rank_levels <- factor(c('Grandmaster'
                        ,'International Master'
                        ,'FIDE Master'
                        ,'Candidate Master'
                        ,'Expert'
                        ,'Class A Category 1'
                        ,'Class B Category 2'
                        ,'Class C Category 3'
                        ,'Class D Category 4'
                        ,'Class E Category 5'
                        ,'Novice'))
## Most common first move
opening_moves_by_rank_white <- chess_dat %>%
  select(
    international_title_white
    , opening_move
  ) %>%
  mutate(
    international_title_white = factor(international_title_white
                                          , levels = rank_levels)
  ) %>% 
  group_by(international_title_white
           , opening_move) %>% 
  summarise(
    count_of_opening_moves = n()
  ) %>% 
  arrange(international_title_white
          , desc(count_of_opening_moves))  # %>% 
  # View()
## Most common first response
opening_moves_by_rank_black <- chess_dat %>%
  select(
  international_title_black
  , opening_move
  ) %>%
  mutate(
    international_title_black = factor(international_title_black
                                       , levels = rank_levels)
  ) %>% 
  group_by(international_title_black
           , opening_move) %>% 
  summarise(
    count_of_opening_moves = n()
  ) %>% 
  arrange(international_title_black
          , desc(count_of_opening_moves)) # %>% 
  # View()
# 2. How many turns does a game last based on player ranking? 

# 3. what move patterns explain the game outcome? 
  
#### Making a graph for both white and black
# TODO: color the top 5 values
# TODO: Log scale the Y-axis
top_moves <- c('e4' = '#1b9e77'
               , 'd4' = '#d95f02'
               , 'c4' = '#7570b3'
               , 'Nf3' = '#e7298a'
               , 'e3' = '#66a61e'
               , 'b3' = '#e6ab02'
               , 'g3' = '#a6761d')
opening_moves_by_rank_white %>% 
  ggplot(aes(x = reorder(opening_move, desc(count_of_opening_moves))
             , y = count_of_opening_moves
             , fill = opening_move)
         ) +
  geom_col() + 
  facet_wrap(~international_title_white) +
  labs(
    title = 'Most Common First Moves by White by Rank'
    , subtitle = 'These are the most common first moves made by white. \nThe rankings are based on the players white_rank.'
    , x = 'Chess Move (Chess Notation)'
    , y = 'Count of Chess Moves (log scaled)'
    , fill = 'Top 7 Moves'
    , caption = 'I logarithmically scaled the y-axis to show the comparitive count of\nchess moves across ranks. I also colored the most common moves\nmade to make them easy to identify.'
  ) +
  scale_y_continuous(trans='log10')+
  scale_fill_manual(values=top_moves)+
  guides(x = guide_axis(angle = 90))

### Making a graph for the first responding move (black)
top_moves <- c('e4' = '#1b9e77'
               , 'd4' = '#d95f02'
               , 'c4' = '#7570b3'
               , 'Nf3' = '#e7298a'
               , 'e3' = '#66a61e'
               , 'b3' = '#e6ab02'
               , 'g3' = '#a6761d')
opening_moves_by_rank_black %>% 
  ggplot(aes(x = reorder(opening_move, desc(count_of_opening_moves))
             , y = count_of_opening_moves
             , fill = opening_move)
  ) +
  geom_col() + 
  facet_wrap(~international_title_white) +
  labs(
    title = 'Most Common First Moves by White by Rank'
    , subtitle = 'These are the most common first moves made by white. \nThe rankings are based on the players white_rank.'
    , x = 'Chess Move (Chess Notation)'
    , y = 'Count of Chess Moves (log scaled)'
    , fill = 'Top 7 Moves'
    , caption = 'I logarithmically scaled the y-axis to show the comparitive count of chess moves across ranks. I also colored the most common moves made to make them easy to identify.'
  ) +
  scale_y_continuous(trans='log10')+
  scale_fill_manual(values=top_moves)+
  guides(x = guide_axis(angle = 90))