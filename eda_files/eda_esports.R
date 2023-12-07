################################################################################
################################################################################
################################################################################

###################
### EDA ESPORTS ###
###################

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

### function to create summary tables of esports ###

esports_sum_table <- function(some_var){
  
  publishers |> 
    #want to filter for the top publishers only
    filter(publishers %in% top_publishers) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    #game_id and date make esports distinct
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    #mutate(year = year(date)) |> 
    summarize(mean = round(mean({{some_var}}), 2),
              quantile_25 = quantile({{some_var}}, probs = c(.25)),
              median = median({{some_var}}),
              quantile_75 = quantile({{some_var}}, probs = c(.75)),
              total = sum({{some_var}}),
              .by = publishers,
              n = n())
  
}

#### esports earnings table ###
esports_sum_table(earnings) |> 
  DT::datatable()

### esports tournaments table ###
esports_sum_table(tournaments) |> 
  DT::datatable()

### esports players table ###
esports_sum_table(players) |> 
  DT::datatable()

### function for getting histogram distributions of esports ###

esports_histograms <- function(some_var = tournaments, binwidth = 1, 
                               xlim = c(0,20), 
                               top = TRUE, filter_less = NA){
  
  #if you want to only analyze top_publishers, set top to TRUE
  if(top == TRUE){ 
    
    histogram <-
      publishers |> 
      #merges a dataset for finding the sepecific histogram, filtering for
      # top publishers
      filter(publishers %in% top_publishers) |> 
      inner_join(game_info, join_by(publisher_id)) |> 
      inner_join(esports, join_by(game_id)) |> 
      distinct(pick(game_id, date), .keep_all = TRUE) |> 
      mutate(year = year(date))
  }else{
    
    histogram <-
      #same process as above, except do not filter on any publisher
      publishers |> 
      inner_join(game_info, join_by(publisher_id)) |> 
      inner_join(esports, join_by(game_id)) |> 
      distinct(pick(game_id, date), .keep_all = TRUE) |> 
      mutate(year = year(date))
    
  }
  
  #if you would like to decrease the number of outlines, set filter_less = 
  # some number
  if(!is.na(filter_less)){
    
    histogram <-
      histogram |> 
      filter({{some_var}} < filter_less)
    
  }
  
  #final graph making
  histogram |> 
    ggplot(aes({{some_var}})) +
    geom_histogram(fill = 'blue4', color = 'white', binwidth = binwidth, boundary = 0) +
    coord_cartesian(xlim = xlim)
  
}

### finding tournaments distribution ###

esports_histograms(some_var = tournaments, binwidth = 1, xlim = c(0,20), 
                   top = FALSE, filter_less = NA) + 
  labs(x = 'Tournaments',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle')

esports_histograms(some_var = tournaments, binwidth = 1, xlim = c(0,20), 
                   top = TRUE, filter_less = NA) + 
  labs(x = 'Tournaments',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle')

### finding players distribution ###

esports_histograms(some_var = players, binwidth = 5, xlim = c(0,200), 
                   top = FALSE, filter_less = NA) + 
  labs(x = 'Players',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle') 

esports_histograms(some_var = players, binwidth = 5, xlim = c(0,200), 
                   top = TRUE, filter_less = NA) + 
  labs(x = 'Players',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle')

### finding earnings distribution ###

esports_histograms(some_var = earnings, binwidth = 250, xlim = c(0,15000), 
                   top = FALSE, filter_less = 15250) +
  labs(x = 'Earnings',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle')

esports_histograms(some_var = earnings, binwidth = 250, xlim = c(0,15000), 
                   top = TRUE, filter_less = 15250) + 
  labs(x = 'Earnings',
       y = '',
       caption = 'Source: RAN.KIRSH - Kaggle')

### find the variations of meidan earnings for top publishers per year ###

publishers |> 
  #this code is fairly similar to the previosu function
  filter(publishers %in% top_publishers) |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(esports, join_by(game_id)) |> 
  distinct(pick(game_id, date), .keep_all = TRUE) |> 
  mutate(year = year(date)) |> 
  summarize(earnings_mean = mean(earnings),
            earnings_median = median(earnings),
            quantile_25 = quantile(earnings, probs = c(.25)),
            quantile_75 = quantile(earnings, probs = c(.75)),
            total_earnings = sum(earnings),
            #however, we're grouping by publishers and year this time, not just
            # publishers
            .by = c(publishers, year),
            n = n()) |>
  filter(earnings_median != max(earnings_median),
         earnings_mean != 0) |> 
  ggplot(aes(year, earnings_median)) +
  geom_vline(xintercept = 2013:2023) +
  geom_point(aes(color = publishers), alpha = .75) +
  coord_flip() +
  scale_x_continuous(breaks = c(2013:2023)) +
  labs(x = 'Year',
       y = 'Earning Median',
       color = 'Publishers',
       caption = 'Source: RAN.KIRSH - Kaggle')

### finding tournaments v players, grouping by year ###

esports |> 
  mutate(date = factor(year(date))) |> 
  ggplot(aes(players, tournaments)) +
  geom_point(alpha = .25) +
  geom_smooth(se = FALSE) + 
  facet_wrap(vars(date)) +
  labs(x = 'Players',
       y = 'Tournaments',
       caption = 'Source: RAN.RIRSH - Kaggle')

### finding earnings v adjusted, grouping by the top publishers ###

esports |> 
  #joining to get the correct dataset for analysis
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  distinct(date, publisher_id, .keep_all = TRUE) |> 
  select(!c(game_id, game, release,genre_id, series_id, mode_id)) |>
  #only focus on earning less than 50 000
  filter(earnings < 50000) |>
  
  ggplot(aes(adjusted, earnings)) +
  geom_point(aes(color = publishers, ), alpha = .3) +
  facet_wrap(vars(publishers)) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Adjusted',
       y = 'Earnings',
       caption = 'Source: RAN.RIRSH - Kaggle, Yahoo! Finance') +
  guides(color = FALSE)

### very similar coding as previous, except now we are looking at 
# tournaments V adjusted ###

esports |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  distinct(date, publisher_id, .keep_all = TRUE) |> 
  select(!c(game_id, game, release,genre_id, series_id, mode_id)) |>
  
  ggplot(aes(adjusted, tournaments)) +
  geom_point(aes(color = publishers), alpha = .3) +
  facet_wrap(vars(publishers)) +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = 'Adjusted',
       y = 'Tournaments',
       caption = 'Source: RAN.RIRSH - Kaggle') +
  guides(color = FALSE)

################################################################################
################################################################################
################################################################################