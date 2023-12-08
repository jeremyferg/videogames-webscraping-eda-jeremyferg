################################################################################
################################################################################
################################################################################


##########################################
##########################################
#####   Relational Database Editing  #####
##########################################
##########################################

################################################################################

##################
### LIBRARIES  ###
##################

library(tidyverse)
library(rvest)
library(tidyquant)
library(quantmod)

game_info_raw <- read_csv('data/raw/game_info_raw.csv')

################################################################################

##############################
### DEALING WITH GAME INFO ###
##############################

game_info <-
game_info_raw |> 
  filter(str_detect(release, '\\d{4}\\-\\d{2}\\-\\d{2}')) |> 
  #make release type date
  mutate(release = as_date(release)) |> 
  #get the specified 10-year period
  filter(release >= as_date('2013-06-30') &
           release <= as_date('2023-06-30')) |>
  
##########################
### DEALING WITH GAMES ###
##########################
  
#general clean up of the game names, also changing this variable name to 'game'
  mutate(name = str_remove(name, '\\(.*\\)'),
         name = str_trim(name, 'both'),
         name = tolower(name)) |> 
  rename(game = name)

###############################
### DEALING WITH PUBLISHERS ###
################################

game_info <-
  game_info |> 
  #there were many instances where a publisher had a string such as [WW]: or [JP]:
  # which indicates which publisher published the game in that industry. This detail
  # is not important to me in this analysis
  mutate(publishers = str_remove_all(publishers, '[A-Z]+\\:\\s'),
         #some publisher values also had the console name within its string, remove these
         publishers = str_remove_all(publishers, '(PS3?4?5?,?)|(Switch,?)|(Nintendo Switch,?)|
                                    |(Xbox One,?)|(Xbox 360,?)|(PlayStation Vita,?)|
                                    |(PlayStation 3?4?5?,?)|(Xbox Series X/S,?)|(3DS,?)'),
         publishers = str_trim(publishers, 'both')) |> 
  filter(publishers != '')  |> 
  #many values in publishers were near identical to other values. For example, 
  # there was an observation of both EA and Electronic Arts in the data. We want
  # to associate these groups with one company
  # due to limited time, i was able to only mutate publishers that i intended 
  # to analyze in the eda, but this list could be simplified further
  mutate(publishers = case_when(str_detect(publishers, 'EA ') ~ 'Electronic Arts',
                                str_detect(publishers, 'Bandai') ~ 'Bandai Namco',
                                str_detect(publishers, 'Capcom') ~ 'Capcom',
                                str_detect(publishers, 'Konami') ~ 'Konami',
                                #these companies are the core subsidaries of take-two
                                str_detect(publishers, '(2K)|(Rockstar Games)|(Zynga)|(Private Division)') ~ 'Take-Two',
                                str_detect(publishers, 'Sega') ~ 'Sega',
                                str_detect(publishers, 'Square.Enix') ~ 'Square Enix',
                                str_detect(publishers, '(Activision)|(Blizzard)') ~ 'Activision Blizzard',
                                .default = publishers),
  publishers = tolower(publishers))

################################
### DEALING WITH GAME SERIES ###
################################

game_info <-
game_info |> 
  #small mutations important for the eda
  mutate(series = if_else(series == 'DarkstalkersStreet Fighter',
                          'Darkstalkers,Street Fighter',
                          series),
         series = if_else(series == 'Warhammer 40' |series == '000',
                          'Warhammer 40000',
                          series),
         series = tolower(series)) |>
  separate_rows(sep = ',') 

###############################
### DEALING WITH GAME MODES ###
###############################

game_info <-
game_info |> 
  mutate(modes = tolower(modes),
         #we want to simply the number of distinct modes values we have in the dataset 
         modes = case_when(str_detect(modes, 'co') ~ 'cooperative',
                           str_detect(modes, '(single.*multi)|(multi.*single)|(both)') ~ 'single-player and multi-player',
                           str_detect(modes, '(online)|(mmo)') ~ 'online',
                           str_detect(modes, '(single)|(singer)') & !str_detect(modes, 'multi') ~ 'single-player',
                           str_detect(modes, '(multi?)|([1-4])|(two)') & !str_detect(modes, 'single') ~ 'multi-player',
                           .default = modes)) 

################################
### DEALING WITH GAME GENRES ###
################################

#NOTE: Order matters when mutating with string detects
# oftentimes, we are trying to mutate genres to a more common form. I chose to 
# mutate genres to specificity. For example, there is a genre called 'Stealth RPG', 
# I made sure that name was changed to 'stealth' instead of 'rpg', because stealth
# games are the lesser common genre
game_info <-
game_info |> 
mutate(genres = tolower(genres),
       genres = case_when(str_detect(genres, '(^action$)|(^action game$)|(^adventure$)|(^adventure game$)') ~'action-adventure',
                          str_detect(genres, '(tactic)|(strategy)|(turn-based)|(rogue)') ~ 'strategy',
                          str_detect(genres, 'puzzle') ~ 'puzzle',
                          str_detect(genres, 'survival') ~ 'survival',
                          #horror must come after survival
                          str_detect(genres, 'horror') ~ 'horror',
                          str_detect(genres, 'stealth') ~ 'stealth',
                          str_detect(genres, 'shoot') ~ 'shoot',
                          #platform needs to go after these previous ones
                          str_detect(genres, 'platform') ~ 'platform',
                          str_detect(genres, '(rhythm)|(music)') ~ 'rhythm',
                          #for beat em' ups, shoot em' ups and hack-n-slash
                          str_detect(genres, "('em)|(hack)") ~ "beat 'em up",
                          str_detect(genres, '(interactive)|(visual)|(point)|(narrative)|(graphic)|(text)') ~ 'interactive storytelling',
                          #fighting needs to come before rpg
                          str_detect(genres, 'fighting') ~ 'fighting',
                          str_detect(genres, '(role)|(rpg)') ~ 'role-playing',
                          str_detect(genres, 'racing') ~ 'racing',
                          str_detect(genres, 'sport') ~ 'sports',
                          str_detect(genres, 'party') ~ 'party',
                          str_detect(genres, 'arcade') ~ 'arcade',
                          str_detect(genres, 'card') ~ 'card',
                          str_detect(genres, 'city') ~ 'city builder',
                          #simulation has to go after racing and sports
                          str_detect(genres, 'sim') ~ 'simulation',
                          #action goes at the END, for action and adventure games
                          str_detect(genres, '(action)|(adver)') ~ 'action-adventure',
                          .default = genres))

##################################
### FINAL VERSION OF GAME_INFO ###
##################################

#after cleaning game_info, we create the additional data sets to create the unique 
#ideas of each of these variables
games <-
game_info |> 
  distinct(game) |> 
  rowid_to_column('game_id')

publishers <-
game_info |> 
  distinct(publishers) |> 
  rowid_to_column('publisher_id')

game_genres <-
game_info |> 
  distinct(genres) |> 
  rowid_to_column('genre_id')

game_series <-
game_info |> 
  distinct(series) |> 
  filter(!is.na(series)) |> 
  rowid_to_column('series_id')

game_modes <-
game_info |> 
  distinct(modes) |> 
  rowid_to_column('mode_id')

#replacing the literal names of variables with their IDs
game_info <-
game_info |> 
  inner_join(games, join_by(game)) |> 
  inner_join(publishers, join_by(publishers)) |> 
  inner_join(game_genres, join_by(genres)) |> 
  left_join(game_series, join_by(series)) |> 
  inner_join(game_modes, join_by(modes)) |> 
  select(!c(game, publishers, series, genres, modes))

################################################################################


######################################
### DEALING WITH PUBLISHERS STOCKS ###
######################################

#helper function to find the conversions of a specified currency conversion
#NOTE: when specifying the variable currency, the second currency is what you are 
# converting to (ex: from JPY to USD)
conversion_finder <- function(currency = 'JPYUSD', from_date = '2013-06-30', to_date = '2023-06-30'){
  #gets exchanges between the specified dates
  tq_get(paste0(currency, '=X'), get = 'stock.prices', from = from_date, to = to_date) |> 
    select(symbol, date, adjusted) |> 
    rename(exchange = adjusted, conversion = symbol) |> 
    mutate(conversion = str_remove(conversion, '=X'))
}

#currency conversions from JPY to USD
jpy_usd <- conversion_finder()
#currency conversions from EUR to USD
eur_usd <- conversion_finder('EURUSD')

#function that converts stocks of publishers originally in JPY
jpy_usd_converted_stocks <- 
  function(stock = '9766.T', company_name = 'Konami', 
           from_date = '2013-06-30', to_date = '2023-06-30'){
    #getting publisher stocks
    tq_get(stock, get = 'stock.prices', from = from_date, to = to_date) |> 
      #this inner_join is where the variable jpy_usd comes into play
      inner_join(jpy_usd |> 
                   select(date, exchange), 
                 join_by(date)) |> 
      mutate(across(open:adjusted, \(x) x*exchange),
             symbol = company_name) |> 
      select(!exchange) |> 
      rename(publisher = symbol)
  }

#function that converts stocks of publishers originally in EUD
eur_usd_converted_stocks <- 
  function(stock = 'UBI.PA', company_name = 'Ubisoft', 
           from_date = '2013-06-30', to_date = '2023-06-30'){
    
    tq_get(stock, get = 'stock.prices', from = from_date, to = to_date) |>
      #this inner_join is where the variable eur_usd comes into play
      inner_join(eur_usd |> select(date, exchange), join_by(date)) |> 
      mutate(across(open:adjusted, \(x) x*exchange),
             symbol = company_name) |> 
      select(!exchange) |> 
      rename(publisher = symbol)
  }

#need to use this for Konami, Square Enix, Sega, Bandai Namco, Capcom
jpy_usd_converted_stocks()
#need to convert ubisoft
eur_usd_converted_stocks()
#NOTE: Nintendo, Take-Two, and EA already use USD

#quick function to get the publisher_stocks
get_publisher_stocks <- function(){
  
  #empty data frame for rbinding stocks together
  publisher_stocks <- data.frame()
  
  #list of Japanese publisher names and their stock names
  jpy_stocks <- list(c('9684.T', 'Square Enix'), c('7832.T', 'Bandai Namco'),
                     c('6460.T', 'Sega'), c('9697.T', 'Capcom'), c('9766.T', 'Konami'))
  #list of American publisher names and their stock names
  usd_stocks <- list(c('NTDOY', 'Nintendo'), c('EA', 'Electronic Arts'),
                     c('TTWO', 'Take-Two'))
  
  #adding Japanese publisher stocks to publisher_stocks
  for(publisher in jpy_stocks){
    publisher_stocks <- rbind(publisher_stocks,
                              jpy_usd_converted_stocks(publisher[1], publisher[2]))
  }
  
  #adding American publisher stocks to publisher_stocks
  for(publisher in usd_stocks){
    
    publisher_stocks <- rbind(publisher_stocks,
                              tq_get(publisher[1], get = 'stock.prices', from = '2013-06-30', to = '2023-06-30') |> 
                                mutate(symbol = publisher[2]) |> 
                                rename(publisher = symbol))
  }
  
  #since there is only one eurusd publisher (ubisoft), rbinding is simple
  publisher_stocks <- rbind(publisher_stocks, eur_usd_converted_stocks())
  
  #lowering the publisher names for merging purpose 
  publisher_stocks <- 
    publisher_stocks |>
    mutate(publisher = tolower(publisher))
  
}

publisher_stocks <- 

get_publisher_stocks() |> 
  inner_join(publishers, join_by(publisher == publishers)) |> 
  select(!c('publisher')) |> 
  relocate(publisher_id, .after = date)

################################################################################

################################
### DEALING WITH GAME AWARDS ###
################################

#112 of our 556 games are missing from the games index (various reasons)
# Ok for our study concerns, but we can add these games to the games db
# in the future for more precision 

game_awards_raw <- read_csv('data/raw/game_awards_raw.csv')


get_game_awards <- function(){
  
  #removing unwanted categories, these categories are not nearly as relevant to our
  # analysis compared to others, so we will drop them
  removed_categories <- c('Best Student Game', 'Student Game Award',
                          'Best Adaptation', 'Best Remaster')
  
  #cleaning up the category names for simplification purposes
  category_cleanup <- list(c('Best Action/Adventure', 'Adventure'),
                           c('Best Game Direction', 'Developer'),
                           c('Best Indie Game', 'Independent'),
                           c('Best Mobile Game', 'Handheld'),
                           c('Best Multiplayer', '(Online)|(Multiplayer Game)'),
                           c('Best Action Game', 'Shooter'),
                           c('Best Score/Soundtrack', '(Score)|(Sound)'),
                           c('Best Sim/Strategy Game', 'Best Strategy'),
                           c('Best VR/AR Game', 'Best VR Game'),
                           c('Best Debut Indie Game', '(Debut)|(Fresh)'),
                           c("Players' Voice", 'Player'),
                           c('Games for Impact', 'Change')
  )
  
  game_awards <-
    game_awards_raw |>
    #a bit of clean up with the game names so we can merge on game
    mutate(game = str_trim(game, 'both'),
           game = tolower(game),
           category = str_remove(category, '\\[.*\\]'),
           category = str_replace(category, ' / ', '/')) |> 
    #removing unwanted categories
    filter(!(category %in% removed_categories)) 
  
  #simplifying category names
  for(some_category in category_cleanup){
    
    game_awards <-
      game_awards |> 
      mutate(category = if_else(str_detect(category, some_category[2]),
                                some_category[1],
                                category))
  }
  
  game_awards <-
    game_awards |>
    #merging on the games df with variable game
    inner_join(games, join_by(game)) |> 
    select(!c(game)) |> 
    relocate(game_id, .after = date) |>  
    #then merging on a merged version of the games df and game_info df to get 
    # publisher_id. we can't just merge game_awards on publishers because there are 
    # many instances where the publisher for game awards doesn't match the publisher 
    # in game_info
    inner_join(game_info |> inner_join(games, join_by(game_id)),
               join_by(game_id)) |> 
    distinct(date, game_id, category, .keep_all = TRUE) |> 
    select(!c(publisher, release, game, genre_id, series_id, mode_id))
  
}
##########################################
### DEALING WITH GAME AWARD CATEGORIES ###
##########################################

#once we've edited the games we want to see in game awards, finding the game award
# categories is simple
game_awards <- get_game_awards()

award_category <- 
  game_awards |> 
  distinct(category) |> 
  rowid_to_column('category_id')

#changing the categories column in game_awards to category_id
game_awards <-
  game_awards |> 
  inner_join(award_category, join_by(category)) |> 
  select(!c(category)) |> 
  relocate(c(category_id, publisher_id), .before = winner)


################################################################################

##################################
### DEALING WITH GAME REVIEWS  ###
##################################

game_reviews_raw <- read_csv('data/raw/game_reviews_raw.csv')

#############################
### GAME_REVIEW_COMPANIES ###
#############################

#this dataset was (thankfully) pretty clean and ready for the relational database
# so i had to do very little cleaning-wise
game_review_companies <- 
  game_reviews_raw |> 
  select(review_company) |>
  mutate(review_company = tolower(review_company)) |> 
  distinct() |> 
  rowid_to_column('review_company_id')

######################
### GAME_REVIEWERS ###
######################

game_reviewers <-
  game_reviews_raw |> 
  #some reviewer observations are NA, so we will just credit the review company
  mutate(reviewer = if_else(is.na(reviewer), review_company, reviewer),
         reviewer = tolower(reviewer),
         review_company = tolower(review_company)) |> 
  #the combination of review_company and reviewer is what makes the primary key
  distinct(pick(reviewer, review_company)) |> 
  arrange(reviewer) |> 
  inner_join(game_review_companies, join_by(review_company)) |> 
  rowid_to_column('reviewer_id')
#keep the review_company names for now for merging purposes later on


#There are a lot of smaller games in the raw game_reviews_raw df, I'm only really
# concerned about notable games for this EDA. Let's look at games that have
# more than 10 reviews

####################
### GAME REVIEWS ###
####################

game_reviews <- 
  game_reviews_raw |> 
  mutate(reviewer = if_else(is.na(reviewer), review_company, reviewer),
         reviewer = tolower(reviewer),
         review_company = tolower(review_company)) |> 
  #inner join on both reviewer and review_company
  inner_join(game_reviewers, join_by(reviewer, review_company)) |> 
  select(!c(reviewer, review_company)) |> 
  relocate(c(reviewer_id, review_company_id)) 

#finding the game ids for each reviewed game by merging
game_reviews <- 
  game_reviews |> 
  anti_join(game_reviews |> 
              count(game) |> 
              filter(n < 10), join_by(game)) |> 
  mutate(game = str_replace_all(game, '_', ' '),
         game = str_trim(game, 'both')) |> 
  inner_join(games, join_by(game)) |> 
  select(!c(game, review_company_id)) |> 
  relocate(date, game_id)

#now get rid of review companies from game_reviewers
game_reviewers <-
game_reviewers |> 
  select(!c(review_company))

################################################################################

#################################
### DEALING WITH ESPORTS_RAW  ###
#################################

esports_raw <- read_csv('data/raw/esports_raw.csv')

#we lose a lot of games when we merge on games because many of the games in the 
# esports_raw dataset weren't published after 2013. That's ok, we still get a 
# decent sample, enough for analysis
esports <-
  esports_raw |> 
  filter(Date >= as_date('2013-06-30') &
           Date <= as_date('2023-06-30')) |> 
  janitor::clean_names() |> 
  mutate(game = str_remove(game, '\\s\\(.*\\)'),
         game = tolower(game)) |>
  inner_join(games, join_by(game)) |> 
  select(!c(game)) |> 
  relocate(game_id, .after = date)

################################################################################

######################################
### WRITE ALL THE DATA SETS TO CSV ###
######################################

#related to game info
write_csv(game_info, 'game_info.csv')
write_csv(games, 'games.csv')
write_csv(publishers, 'publishers.csv')
write_csv(game_genres, 'game_genres.csv')
write_csv(game_series, 'game_series.csv')
write_csv(game_modes, 'game_modes.csv')
#related to esports
write_csv(esports, 'esports.csv')
#related to publisher stocks
write_csv(publisher_stocks, 'publisher_stocks.csv')
#related to game awards
write_csv(award_category, 'award_category.csv')
write_csv(game_awards, 'game_awards.csv')
#related to game reviews
write_csv(game_reviews, 'game_reviews.csv')
write_csv(game_reviewers, 'game_reviewers.csv')
write_csv(game_review_companies, 'game_review_companies.csv')

################################################################################

#############################
### CREATING THE CODEBOOK ###
#############################

games_database_codebook <-
tribble(
  ~variable, ~meaning,
  'release_date', 'First release date of a game',
  'game_id', 'Unique ID number of a game',
  'game', 'Title of the game',
  'publisher_id', 'Unique ID number of a publisher',
  'publishers', 'Name of a publisher',
  'series_id', 'Unique ID number of a series',
  'series', 'Name of a series',
  'genre_id', 'Unique ID number of a genre',
  'genre', 'Name of a genre',
  'mode_id', 'Unique ID number of a mode',
  'mode', 'Name of a mode',
  'date', 'Date of the observation record',
  'earnings', 'Total esports earnings of a game for a specified month',
  'players', 'Number of participating esports players for a game within a specified month',
  'tournaments', 'Number of esports tournaments for a game the past month',
  'open', 'Opening daily value of a stock',
  'high', 'Highest daily value of a stock',
  'low', 'Lowest daily value of a stock',
  'close', 'Closing daily value of a stock',
  'volume', 'Amount of shares traded on a specified date',
  'adjusted', 'Closing daily value after adjustments for splits and dividend distributions',
  'category_id', 'Unique ID number of all Game Awards category names',
  'category', 'Name of a Game Awards category',
  'winner', 'True if the observation won the specified Game Awards category',
  'review_company_id', 'Unique ID number of all game review companys/groups',
  'review_company', 'Name of the game review_company/group',
  'reviewer_id', 'Unique ID number of all game reviewers',
  'reviewer', 'Name of the reviewer',
  'rating', 'Game rating given by a reviewer (out of 100)',
  'rating_yesterday', 'The average rating of a game up to the date of the observation'
)

write_csv(games_database_codebook, 'games_database_codebook.csv')

################################################################################
################################################################################
################################################################################