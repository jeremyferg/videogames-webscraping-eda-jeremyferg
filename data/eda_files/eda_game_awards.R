################################################################################
################################################################################
################################################################################

#######################
### EDA GAME AWARDS ###
#######################

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

### finding the top publishers with the most winners ###

publishers |> 
  #only want the top publishers
  filter(publishers %in% top_publishers) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  filter(winner == TRUE) |> 
  mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
  #count how many wins each publisher has
  summarise(
    n = n(), 
    .by = publishers) |> 
  ggplot(aes(publishers, n)) +
  geom_bar(fill = 'blue4', stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 1) +
  labs(x = 'Number of Game Awards Wins',
       y = 'Publishers',
       caption = 'Source: Wikipedia')

### summary table of all top-publisher Game Awards wins ###

publishers |> 
  filter(publishers %in% top_publishers) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  inner_join(award_category, join_by(category_id)) |>
  inner_join(games, join_by(game_id)) |> 
  filter(winner == TRUE) |> 
  select(c(date, category, publishers, game)) |> 
  DT::datatable()

### similar to finding winners of Game Awards, now we only care about if a 
# top-publisher's game was nominated for an award ###

publishers |> 
  filter(publishers %in% top_publishers) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
  summarise(n = n(), .by = publishers) |> 
  ggplot(aes(publishers, n)) +
  geom_bar(fill = 'blue4', stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 3) +
  labs(x = 'Number of Game Awards Nominations',
       y = 'Publishers',
       caption = 'Source: Wikipedia')

### seeing how top publishers compare to everyone else when considering the number
# of wins over the last 10 years

publishers |> 
  #similar flow as previous code for finding winners
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_awards, join_by(game_id)) |> 
  inner_join(award_category, join_by(category_id)) |> 
  filter(winner == TRUE) |>  
  distinct(pick(game, category_id, date), .keep_all = TRUE) |> 
  select(c(date, game, publishers, category)) |> 
  mutate(category = factor(category)|> fct_infreq() |> fct_rev(),
         #changing all other publishers (besides the top publishers) to 'other'
         top_publishers = factor(if_else(publishers %in% top_publishers,
                                         publishers,
                                         'other'))) |> 
  summarise(n = n(), .by = c(category, top_publishers)) |> 
  
  mutate(top_publishers = fct_reorder(top_publishers, n)) |> 
  ggplot(aes(category, n, fill = top_publishers)) +
  #playing with the hues of the colors printed on the graph
  scale_fill_hue(l=40, c=30) +
  geom_bar(stat = 'identity', color = 'white') +
  scale_y_continuous(breaks = c(1:9)) +
  coord_flip() +
  labs(x = 'Game Awards Wins',
       y = 'Category',
       caption = 'Source: Wikipedia',
       fill = 'Top Category')

#table finding the correlation of the averaged adjusted close stock value and
# nominations
publishers |> 
  filter(publishers %in% top_publishers) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  summarise(
    mean_adjust = round(mean(adjusted), 2),
    .by = publishers, 
    nominations = n()) |> 
  mutate(correlation = cor(mean_adjust, nominations)) |> 
  DT::datatable()

################################################################################
################################################################################
################################################################################
