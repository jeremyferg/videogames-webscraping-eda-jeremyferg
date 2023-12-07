
###########################
###########################
#####    GAMES EDA    #####
###########################
###########################

library(tidyverse)

################################################################################

#####################
### GAME INFO EDA ###
#####################

for(csv in 
list.files(path="C:\\stat-301-1\\final-project-1-jeremyferg\\data\\games_database", pattern=NULL, all.files=FALSE, 
           full.names=FALSE)){
  
  read_csv(paste0('data\games_database'))
}


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





###### Q: Who published the most games?


#game counts#
publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  distinct(pick(publisher_id, game_id), .keep_all = TRUE) |> 
  mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
  summarise(n = n(), .by = publishers) |> 
  ggplot(aes(publishers, n)) +
  geom_bar(fill = 'blue4', stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 10)


#game counts by per publisher based on series#
publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |>
  inner_join(game_series, join_by(series_id)) |> 
  distinct(pick(publisher_id, game_id, series_id), .keep_all = TRUE) |> 
  filter(publishers == 'electronic arts') |>
  mutate(series = factor(series)|> fct_infreq() |> fct_rev(),
         series = fct_lump(series, n = 10)) |> 
  summarise(n = n(), .by = series) |> 
  ggplot(aes(series, n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 1)

## looking at genres (general) ##
publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |>
  inner_join(game_genres, join_by(genre_id)) |> 
  distinct(pick(publisher_id, game_id, genre_id), .keep_all = TRUE) |> 
  mutate(genres = factor(genres)|> fct_infreq() |> fct_rev(),
         genres = fct_lump(genres, n = 15)) |> 
  summarise(n = n(), .by = genres) |> 
  ggplot(aes(genres, n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 10)

## looking at genres per publisher ##

publisher_list <- c('nintendo', 'electronic arts', 'activision blizzard',
                    'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                    'bandai namco', 'capcom')

#test for-loop to make the publisher genres#
for(som_publisher in publisher_list[1:3]){
  
publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |>
  inner_join(game_genres, join_by(genre_id)) |> 
  distinct(pick(publisher_id, game_id, genre_id), .keep_all = TRUE) |> 
  filter(publishers == som_publisher)|> 
  mutate(genres = factor(genres)|> fct_infreq() |> fct_rev(),
         genres = fct_lump(genres, n = 7)) |> 
  summarise(n = n(), .by = genres) |> 
  ggplot(aes(genres, n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 10)
  
  ggsave(paste0('data/', som_publisher,'.png'))
  
}

### boxplot of game modes? ###

publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |>
  inner_join(game_modes, join_by(mode_id)) |> 
  distinct(pick(publisher_id, game_id, mode_id), .keep_all = TRUE) |> 
  mutate(modes = factor(modes)|> fct_infreq() |> fct_rev(),
         modes = fct_lump(modes, n = 2),
         #FIX THIS
         days_since = as.numeric(today() - release),
         year = if_else(year('2017-12-31') < year(release), 
                        'last_five',
                        'first_five')) |> 
  #summarise(n = n(), .by = modes) |> 
  ggplot(aes(year, days_since, fill = modes)) +
  geom_violin() #+
  #coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 10)
  
################################################################################
  
###################
### ESPORTS EDA ###
###################

###### Q: Who has the most esports earnings?

#summary statistics of esports earnings
publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(esports, join_by(game_id)) |> 
  distinct(pick(game_id, date), .keep_all = TRUE) |> 
  mutate(year = year(date)) |> 
  summarize(earnings_mean = mean(earnings),
            earnings_median = median(earnings),
    quantile_25 = quantile(earnings, probs = c(.25)),
            quantile_75 = quantile(earnings, probs = c(.75)),
    total_earnings = sum(earnings),
            .by = publishers,
    n = n())
  #ggplot(aes(date, earnings)) + 
  #geom_point() +
  #facet_wrap(~year, scales = 'free')

  
  #median earnings by year, group by publisher
  publishers |> 
    filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                             'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                             'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    summarize(earnings_mean = mean(earnings),
              earnings_median = median(earnings),
              quantile_25 = quantile(earnings, probs = c(.25)),
              quantile_75 = quantile(earnings, probs = c(.75)),
              total_earnings = sum(earnings),
              .by = c(publishers, year),
              n = n()) |>
    filter(earnings_median != max(earnings_median),
           earnings_mean != 0) |> 
    ggplot(aes(year, earnings_median)) +
    geom_vline(xintercept = 2013:2023) +
    geom_point(aes(color = publishers)) +
    coord_flip() 
  
    #facet_wrap(~publishers, scales = 'free')
  
  #tournament distribution
  publishers |> 
    #filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
    #                         'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
    #                         'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    #summarize(tournament_total = sum(tournaments),
    #          .by = c(year),
    #          n = n()) #|>
    #filter(earnings_median != max(earnings_median),
     #      earnings_mean != 0) |> 
    ggplot(aes(tournaments)) +
    geom_histogram(color = 'white', binwidth = 1, boundary = 1) +
    coord_cartesian(xlim = c(0,20))
    #geom_point(aes(color = publishers)) +
    
  #looking only at the main publishers
  publishers |> 
    filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                             'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                             'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    #summarize(tournament_total = sum(tournaments),
    #          .by = c(year),
    #          n = n()) #|>
    #filter(earnings_median != max(earnings_median),
    #      earnings_mean != 0) |> 
    ggplot(aes(tournaments)) +
    geom_histogram(color = 'white', binwidth = 1, boundary = 1) +
    coord_cartesian(xlim = c(0,20))
  
  publishers |> 
    #filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
    #                         'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
    #                         'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    #summarize(tournament_total = sum(tournaments),
    #          .by = c(year),
    #          n = n()) #|>
    #filter(earnings_median != max(earnings_median),
    #      earnings_mean != 0) |> 
    ggplot(aes(tournaments)) +
    geom_histogram(color = 'white', binwidth = 1, boundary = 1) +
    coord_cartesian(xlim = c(0,20))
  #geom_point(aes(color = publishers)) +
  
  #general publishers
  publishers |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    
    ggplot(aes(players)) +
    geom_histogram(color = 'white', binwidth = 5, boundary = 0) +
    coord_cartesian(xlim = c(0,200))
  
  
  #looking only at the main publishers player counts
  publishers |> 
    filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                             'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                             'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
 
    ggplot(aes(players)) +
    geom_histogram(color = 'white', binwidth = 5, boundary = 0) +
    coord_cartesian(xlim = c(0,200))
  
  #general publishers earnings
  publishers |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    filter(earnings < 15250) |> 
    
    ggplot(aes(earnings)) +
    geom_histogram(color = 'white', binwidth = 250, boundary = 0) +
    coord_cartesian(xlim = c(0,15000))
  
  
  #looking only at the main publishers earnings counts
  publishers |> 
    filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                             'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                             'bandai namco', 'capcom')) |> 
    inner_join(game_info, join_by(publisher_id)) |> 
    inner_join(esports, join_by(game_id)) |> 
    distinct(pick(game_id, date), .keep_all = TRUE) |> 
    mutate(year = year(date)) |> 
    filter(earnings < 15250) |> 
    
    ggplot(aes(earnings)) +
    geom_histogram(color = 'white', binwidth = 250, boundary = 0) #+
    coord_cartesian(xlim = c(0,200))
    
    #tournaments v players by year
    esports |> 
      mutate(date = factor(year(date))) |> 
      ggplot(aes(players, tournaments)) +
      geom_point(alpha = .25) +
      geom_smooth(se = FALSE) + 
      facet_wrap(vars(date))
    
    #for earnings
    esports |> 
      inner_join(games, join_by(game_id)) |> 
      inner_join(game_info, join_by(game_id)) |> 
      inner_join(publishers, join_by(publisher_id)) |> 
      inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
      distinct(date, publisher_id, .keep_all = TRUE) |> 
      select(!c(game_id, game, release,genre_id, series_id, mode_id)) |>
      filter(earnings < 50000) |>
      
      ggplot(aes(adjusted, earnings)) +
      geom_point(aes(color = publishers), alpha = .3) +
      facet_wrap(vars(publishers)) +
      geom_smooth(method = lm, se = FALSE)
    
    #for tournament
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
      geom_smooth(method = lm, se = FALSE)
    
                 
                 

    
################################################################################
  
#######################
### GAME AWARDS EDA ###
#######################

###### Q: Who has won the most Game Awards? 
  
### table of winners

publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  filter(winner == TRUE) |> 
  mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
  summarise(n = n(), .by = publishers) |> 
  ggplot(aes(publishers, n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 1)
  
### Game Awards Nominees?
  
  publishers |> 
    filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                             'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                             'bandai namco', 'capcom')) |> 
    inner_join(game_awards, join_by(publisher_id)) |> 
    mutate(publishers = factor(publishers)|> fct_infreq() |> fct_rev()) |> 
    summarise(n = n(), .by = publishers) |> 
    ggplot(aes(publishers, n)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    geom_text(aes(label = signif(n)), nudge_y = 3)

##what category were these winners in?

#filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
#                         'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
#                         'bandai namco', 'capcom')) 

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_awards, join_by(game_id)) |> 
  inner_join(award_category, join_by(category_id)) |> 
  filter(winner == TRUE) |>  
  distinct(pick(game, category_id, date), .keep_all = TRUE) |> 
  select(c(date, game, publishers, category)) |> 
  mutate(category = factor(category)|> fct_infreq() |> fct_rev(),
         top_games = factor(if_else(publishers %in% publisher_list,
                            publishers,
                            'other'))) |> 
  summarise(n = n(), .by = c(category, top_games)) |> 
  #arrange(desc(n)) |> 
  mutate(top_games = fct_reorder(top_games, n)) |> 
  ggplot(aes(category, n, fill = top_games)) +
  scale_fill_hue(l=40, c=30) +
  geom_bar(stat = 'identity', color = 'white') +
  scale_y_continuous(breaks = c(1:9)) +
  coord_flip()


###
### FOLLOW-UP: What specific categories?
# look specficially at nintendo

publishers |> 
  filter(publishers %in% c('nintendo', 'electronic arts', 'activision blizzard',
                           'square enix', 'sega', 'konami', 'ubisoft', 'take-two',
                           'bandai namco', 'capcom')) |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_awards, join_by(game_id)) |> 
  inner_join(award_category, join_by(category_id)) |> 
  distinct(pick(game, category_id, date), .keep_all = TRUE) |> 
  select(c(date, game, publishers, category)) |> 
  filter(publishers == 'nintendo') |> 
  mutate(category = factor(category)|> fct_infreq() |> fct_rev(), 
         category = fct_lump(category, n = 5)) |> 
  count(category, sort = TRUE) |> 
  #summarise(n = n(), .by = category) #|> 
  ggplot(aes(category, n)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  geom_text(aes(label = signif(n)), nudge_y = 1)

################################################################################

########################
### GAME REVIEWS EDA ###
########################

#distribution of game review scores
game_reviews |> 
  ggplot(aes(rating)) +
  geom_histogram(color = 'white', binwidth = 5, boundary = 100) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))
  

#stat summary of review scores
game_reviews |> 
  filter(!is.na(rating)) |> 
  summarise(
    rating_mean = mean(rating),
    rating_median = median(rating),
    rating_sd = sd(rating),
    rating_iqr = IQR(rating)
  )

## same things but for the specific publishers (very little difference)

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |>
  filter(publishers %in% publisher_list) |> 
  
  ggplot(aes(rating)) +
  geom_histogram(color = 'white', binwidth = 5, boundary = 100) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_x_continuous(breaks = c(10,20,30,40,50,60,70,80,90,100))

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |> 
  filter(publishers %in% publisher_list, !is.na(rating)) |> 
  summarise(
    rating_mean = mean(rating),
    rating_median = median(rating),
    rating_sd = sd(rating),
    rating_iqr = IQR(rating)
  )
  
publishers |> 
  filter(publishers %in% publisher_list) |> 
  inner_join(game_awards, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  summarise(
    mean_adjust = mean(adjusted),
    .by = publishers, 
    nominations = n()) |> 
  mutate(correlation = cor(mean_adjust, nominations)) |> 
  DT::datatable()

distinct(pick(date, publisher_id), .keep_all = TRUE) |> 
  ggplot(aes(count(publishers), adjusted)) +
  geom_point(alpha = .25) +
  geom_smooth(method = lm, se = FALSE)

#most reviews for each company

publishers |> 
  inner_join(game_info, join_by(publisher_id)) |> 
  inner_join(games, join_by(game_id)) |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_reviewers, join_by(reviewer_id)) |> 
  inner_join(game_review_companies, join_by(review_company_id)) |> 
  
  distinct(pick(reviewer_id, game_id), .keep_all = TRUE) |> 
  mutate(review_company = fct_lump(factor(review_company), 10)) |> 
  filter(publishers %in% publisher_list, review_company != 'Other') |> 
  
  ggplot(aes(publishers, fill = review_company)) +
  scale_fill_hue(l=40, c=30) +
  geom_bar(color = 'white') +
  coord_flip()

#stocks relational ratings general
games |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  
  distinct(pick(date, publisher_id), .keep_all = TRUE) |> 
  filter(publishers %in% publisher_list) |> 
  ggplot(aes(rating, adjusted)) +
  geom_point(alpha = .25) +
  geom_smooth(method = lm, se = FALSE)

#stocks relational ratings by publisher
games |> 
  inner_join(game_reviews, join_by(game_id)) |> 
  inner_join(game_info, join_by(game_id)) |> 
  inner_join(publishers, join_by(publisher_id)) |> 
  inner_join(publisher_stocks, join_by(publisher_id, date)) |> 
  
  distinct(pick(date, publisher_id), .keep_all = TRUE) |> 
  filter(publishers %in% publisher_list) |> 
  ggplot(aes(rating, adjusted)) +
  geom_point(alpha = .25) +
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(vars(publishers))
  
  