################################################################################
################################################################################
################################################################################

########################
### EDA GAME REVIEWS ###
########################

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

### distribution of game review scores ###

game_reviews |> 
  ggplot(aes(rating)) +
  geom_histogram(fill = 'blue4', 
                 color = 'white',
                 binwidth = 5, 
                 boundary = 100) +
  coord_cartesian(xlim = c(10, 100)) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  labs(x = 'Rating',
       y = '',
       caption = 'Source: OpenCritic')

### summary table for rating ###

game_reviews |> 
  filter(!is.na(rating)) |> 
  summarise(
    rating_mean = mean(rating),
    rating_median = median(rating),
    rating_sd = sd(rating),
    rating_iqr = IQR(rating)
  ) |> 
  knitr::kable() 

### ratings distribution for the top publishers ###

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |>
  filter(publishers %in% top_publishers) |> 
  
  ggplot(aes(rating)) +
  geom_histogram(fill = 'blue4', 
                 color = 'white', 
                 binwidth = 5, 
                 boundary = 100) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100)) +
  labs(x = 'Rating',
       y = '',
       caption = 'Source: OpenCritic')

### summary table of rating when address the top publishers ###

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |> 
  filter(publishers %in% top_publishers, !is.na(rating)) |> 
  summarise(
    rating_mean = mean(rating),
    rating_median = median(rating),
    rating_sd = sd(rating),
    rating_iqr = IQR(rating)
  ) |> 
  knitr::kable()

### finding how the top 10 companies game-review whise distribute their reviews
# across the top_publishers

publishers |> 
  #again, code is very similar to other variables
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_reviewers, join_by(reviewer_id)) |> 
  inner_join(game_review_companies, join_by(review_company_id)) |> 
  
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |> 
  mutate(review_company = fct_lump(factor(review_company), 10)) |> 
  filter(publishers %in% top_publishers, review_company != 'Other') |> 
  
  ggplot(aes(publishers, fill = review_company)) +
  scale_fill_hue(l=40, c=30) +
  geom_bar(color = 'white') +
  coord_flip() +
  labs(x = '',
       y = 'Publishers',
       fill = 'Review Company',
       caption = 'Source: OpenCritic')

### relationship between the adjusted close stock value and rating ###

games |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  
  distinct(pick(date, publisher_id), .keep_all = TRUE) |> 
  filter(publishers %in% top_publishers) |> 
  ggplot(aes(rating, adjusted)) +
  geom_point(alpha = .25) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Rating',
       y = 'Adjusted',
       caption = 'Sources: OpenCritic, Yahoo! Financne')

### rsimiliar code as previous scatterplot except group by publisher ###

games |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  
  distinct(pick(date, publisher_id), .keep_all = TRUE) |> 
  filter(publishers %in% top_publishers) |> 
  ggplot(aes(rating, adjusted)) +
  geom_point(alpha = .25) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(vars(publishers)) +
  labs(x = 'Rating',
       y = 'Adjusted',
       caption = 'Sources: OpenCritic, Yahoo! Financne')

#########################################################################