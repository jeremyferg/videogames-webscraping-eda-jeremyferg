################################################################################
################################################################################
################################################################################

#####################
### EDA GAME INFO ###
#####################

### libraries in use ###

library(tidyverse)

### database in use ###

game_awards <- read_csv('data/games_database/game_awards.csv')
award_category <- read_csv('data/games_database/award_category.csv')

esports <- read_csv('data/games_database/esports.csv')

game_info <- read_csv('data/games_database/game_info.csv')
games <- read_csv('data/games_database/games.csv')
game_genres <- read_csv('data/games_database/game_genres.csv')
game_modes <- read_csv('data/games_database/game_modes.csv')
game_series <- read_csv('data/games_database/game_series.csv')

publishers <- read_csv('data/games_database/publishers.csv')
publisher_stocks <- read_csv('data/games_database/publisher_stocks.csv')

game_reviews <- read_csv('data/games_database/game_reviews.csv')
game_review_companies <- read_csv('data/games_database/game_review_companies.csv')
game_reviewers <- read_csv('data/games_database/game_reviewers.csv')

### key variable ###

top_publishers <- c('nintendo', 'electronic arts', 'activision blizzard',
                    'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                    'bandai namco', 'capcom')

################################################################################

### get distribution of games by top publisher ###

publishers |> 
  #filter out only observations with the top publisher
  filter(publishers %in% top_publishers) |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  #specifically, we want distinct games/publishers
  distinct(pick(publisher_id, game_id), .keep_all = TRUE) |> 
  #mutating publishers into a factor to we can order the barplot better
  mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
  #we only need the plublisher name and the number of times that name shows up in
  #the df
  summarise(n = n(), .by = publishers) |> 
  ggplot(aes(publishers, n)) +
  geom_bar(fill = 'blue4', stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 10) +
  labs(y = 'Number of Games',
       x = 'Publishers',
       caption = 'Source: Wikipedia')

### get other distributions with a function (games distribution code is just) a
# simplified version of this function ###

get_game_info_distr <- function(some_var = game_series, var_id = series_id, 
                                var_title = series, nudge = 80, 
                                top_publisher = FALSE){
  
  #if you want to only analyze the top publishers, set top_publisher == TRUE
  if(top_publisher == TRUE){
    
    bar <-
      #again, very similar process as in the previous distribution code
      publishers |> 
      filter(publishers %in% top_publishers) |> 
      inner_join(game_info, join_by(publisher_id)) |>
      inner_join({{some_var}}, join_by({{var_id}})) |> 
      distinct(pick(publisher_id, game_id, {{var_id}}), .keep_all = TRUE) |> 
      mutate({{var_title}} := factor({{var_title}})|> fct_infreq() |> fct_rev(),
             {{var_title}} := fct_lump({{var_title}}, n = 15))
  }else{
    
    bar <-
      #the difference here is that we are no longer filtering for the top-publishers
      publishers |> 
      inner_join(game_info, join_by(publisher_id)) |>
      inner_join({{some_var}}, join_by({{var_id}})) |> 
      distinct(pick(publisher_id, game_id, {{var_id}}), .keep_all = TRUE) |> 
      mutate({{var_title}} := factor({{var_title}})|> fct_infreq() |> fct_rev(),
             {{var_title}} := fct_lump({{var_title}}, n = 15))
    
  } 
  
  bar |> 
    summarise(n = n(), .by = {{var_title}}) |> 
    ggplot(aes({{var_title}}, n)) +
    geom_bar(fill = 'blue4', stat = 'identity') +
    coord_flip() +
    geom_text(aes(label = signif(n)), nudge_y = nudge)
  
}

### game_series distributions ###

get_game_info_distr(some_var = game_series, var_id = series_id, 
                    var_title = series, nudge = 80, 
                    top_publisher = FALSE) +
  labs(x = 'Series',
       y = 'Number of Games',
       caption = 'Source: Wikipedia')

get_game_info_distr(some_var = game_series, var_id = series_id, 
                    var_title = series, nudge = 25, 
                    top_publisher = TRUE) +
  labs(x = 'Series',
       y = 'Number of Games',
       caption = 'Source: Wikipedia')

### game_genres distribution ###

get_game_info_distr(some_var = game_genres, var_id = genre_id, 
                    var_title = genres, nudge = 50, 
                    top_publisher = FALSE) +
  labs(x = 'Genres',
       y = 'Number of Games',
       caption = 'Source: Wikipedia')

get_game_info_distr(some_var = game_genres, var_id = genre_id, 
                    var_title = genres, nudge = 10, 
                    top_publisher = TRUE) +
  labs(x = 'Genres',
       y = 'Number of Games',
       caption = 'Source: Wikipedia')

### boxplot distribution of days since a game released, group by game modes ###

publishers |> 
  filter(publishers %in% top_publishers) |> 
  inner_join(game_info, join_by(publisher_id)) |>
  inner_join(game_modes, join_by(mode_id)) |> 
  distinct(pick(publisher_id, game_id, mode_id), .keep_all = TRUE) |> 
  mutate(modes = factor(modes)|> fct_infreq() |> fct_rev(),
         modes = fct_lump(modes, n = 2),
         days_since = as.numeric(today() - release),
         year = if_else(year('2017-12-31') < year(release), 
                        'Years 2018 - 2023',
                        'Years 2013 - 2017')) |> 
  ggplot(aes(year, days_since, fill = modes)) +
  geom_violin() + 
  labs(x = 'Years',
       y = 'Days Since Game Released',
       fill = 'Modes',
       caption = 'Source: Wikipedia')



################################################################################
################################################################################ 
################################################################################