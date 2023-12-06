
##########################################
##########################################
#####   Relational Database Editing  #####
##########################################
##########################################

library(tidyverse)
library(rvest)
library(tidyquant)
library(quantmod)
game_info_raw <- read_csv('data/raw/game_info_raw.csv')

#some release dates (50 games in total) weren't able to be corretly parsed because 
# they did not match the parsing in release_extractor().
#Im ok with dropping these games, as without a specific date, I can't accurate determine
# the game's short-term effect on stocks
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
  
  mutate(name = str_remove(name, '\\(.*\\)'),
         name = str_trim(name, 'both'),
         name = tolower(name)) |> 
  rename(game = name)

#############################################
### DEALING WITH PUBLISHERS (what a pain) ###
#############################################

game_info <-
  game_info |> 
  mutate(publishers = str_remove_all(publishers, '[A-Z]+\\:\\s'),
         publishers = str_remove_all(publishers, '(PS3?4?5?,?)|(Switch,?)|(Nintendo Switch,?)|
                                    |(Xbox One,?)|(Xbox 360,?)|(PlayStation Vita,?)|
                                    |(PlayStation 3?4?5?,?)|(Xbox Series X/S,?)|(3DS,?)'),
         publishers = str_trim(publishers, 'both')) |> 
  filter(publishers != '')  |> 
  mutate(publishers = case_when(str_detect(publishers, 'EA ') ~ 'Electronic Arts',
                                str_detect(publishers, 'Bandai') ~ 'Bandai Namco',
                                str_detect(publishers, 'Capcom') ~ 'Capcom',
                                str_detect(publishers, 'Konami') ~ 'Konami',
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
  mutate(series = if_else(series == 'DarkstalkersStreet Fighter',
                          'Darkstalkers,Street Fighter',
                          series),
         #only drops a handful of observations
         series = tolower(series)) |>
  separate_rows(sep = ',') 

###############################
### DEALING WITH GAME MODES ###
###############################

game_info <-
game_info |> 
  mutate(modes = tolower(modes),
         modes = case_when(str_detect(modes, 'co') ~ 'cooperative',
                           str_detect(modes, '(single.*multi)|(multi.*single)|(both)') ~ 'single-player and multi-player',
                           str_detect(modes, '(online)|(mmo)') ~ 'online',
                           str_detect(modes, '(single)|(singer)') & !str_detect(modes, 'multi') ~ 'single-player',
                           str_detect(modes, '(multi?)|([1-4])|(two)') & !str_detect(modes, 'single') ~ 'multi-player',
                           .default = modes)) 

################################
### DEALING WITH GAME GENRES ###
################################

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

game_info <-
game_info |> 
  inner_join(games, join_by(game)) |> 
  inner_join(publishers, join_by(publishers)) |> 
  inner_join(game_genres, join_by(genres)) |> 
  left_join(game_series, join_by(series)) |> 
  inner_join(game_modes, join_by(modes)) |> 
  select(!c(game, publishers, series, genres, modes))

################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################

##########################
### DEALING WITH GAMES ###
##########################

#the games are unique without lowercasing but consider lowercasing
games <-
game_info |> 
  mutate(name = str_remove(name, '\\(.*\\)'),
         name = str_trim(name, 'both'),
         name = tolower(name)) |> 
  distinct(name) |> 
  rowid_to_column('game_id')

################################################################################

################################
### DEALING WITH GAME SERIES ###
################################

#seem to be having that same problem we had with publishers were some series
# are glued together, really need to figure out how we can split it with like
# some random character

#UPDATE: to my knowledge, we're golden!

game_series <-
game_info |> 
  # 'DarkstalkersStreet Fighter' is so far the only series I could find still 
  # merged together
  mutate(series = if_else(series == 'DarkstalkersStreet Fighter',
                          'Darkstalkers,Street Fighter',
                          series),
         #only drops a handful of observations
         series = tolower(series)) |>
  separate_rows(sep = ',') |> 
  distinct(series) |> 
  filter(!is.na(series)) |> 
  rowid_to_column('series_id')

################################################################################

###############################
### DEALING WITH GAME MODES ###
###############################

#same problem as series and publishers
#this time we have a lot of the same mode but they're just written differently
game_modes <-
game_info |> 
  mutate(modes = tolower(modes),
         modes = case_when(str_detect(modes, 'co') ~ 'cooperative',
                           str_detect(modes, '(single.*multi)|(multi.*single)|(both)') ~ 'single-player and multi-player',
                           str_detect(modes, '(online)|(mmo)') ~ 'online',
                           str_detect(modes, '(single)|(singer)') & !str_detect(modes, 'multi') ~ 'single-player',
                           str_detect(modes, '(multi?)|([1-4])|(two)') & !str_detect(modes, 'single') ~ 'multi-player',
                           .default = modes)) |> 
  distinct(modes) |> 
  filter(!is.na(modes)) |> 
  rowid_to_column('mode_id')

#seeing the number of games in each more, would probably be beneficial clump these
# small-fries to 'other'
game_info |>
  mutate(modes = tolower(modes),
    modes = case_when(str_detect(modes, 'co') ~ 'cooperative',
                           str_detect(modes, '(single.*multi)|(multi.*single)|(both)') ~ 'single-player and multi-player',
                           str_detect(modes, '(online)|(mmo)') ~ 'online',
                           str_detect(modes, '(single)|(singer)') & !str_detect(modes, 'multi') ~ 'single-player',
                           str_detect(modes, '(multi?)|([1-4])|(two)') & !str_detect(modes, 'single') ~ 'multi-player',
                           .default = modes)) |> 
  summarise(.by = modes, n = n()) |> 
  print(n = 30)

################################################################################

################################
### DEALING WITH GAME GENRES ###
################################

#we need to identify which genres we want to look at (go off game awards?)

##UPDATE: basically just tried to clump groups enough to have some serious
# EDA, if given more time, I would continue to reduce the number of categories
# we have, but the top 15 or so is good for this analysis

game_genres <-
  game_info |> 
  select(genres) |> 
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
                            .default = genres)) |> 
  distinct() |> 
  arrange(genres) |> 
  rowid_to_column('genre_id')


game_genres |> 
  filter(str_detect(genres, 'sport'))
game_genres |> 
  filter(str_detect(genres, 'racing'))
game_genres |> 
  filter(str_detect(genres, '(action)|(adventure)'))
game_genres |> 
  filter(str_detect(genres, 'strategy'))
game_genres |> 
  filter(str_detect(genres, 'rpg'))
game_genres |> 
  filter(str_detect(genres, "shoot")) |> 
  print(n = 50)

#NOTE: order matters! I'm trying to do more specific genre cases first and then capture
# more general ones after that

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
                            #action goes at the END
                            str_detect(genres, '(action)|(adver)') ~ 'action-adventure',
                            .default = genres)) |> 
  summarise(.by = genres, n = n()) |> 
  arrange(desc(n)) |> 
  #filter(str_detect(genres, 'adventure')) |> 
  print(n = 50)

game_genres <-
game_genres |> 
  mutate(genres = case_when(str_detect(genres, '(^action$)|(^adventure$)') ~'action-adventure',
                            .default = genres))
  

################################################################################

#############################################
### DEALING WITH PUBLISHERS (what a pain) ###
#############################################

publishers <-
game_info |> 
  mutate(publishers = str_remove_all(publishers, '[A-Z]+\\:\\s'),
         publishers = str_remove_all(publishers, '(PS3?4?5?,?)|(Switch,?)|(Nintendo Switch,?)|
                                    |(Xbox One,?)|(Xbox 360,?)|(PlayStation Vita,?)|
                                    |(PlayStation 3?4?5?,?)|(Xbox Series X/S,?)|(3DS,?)'),
         publishers = str_trim(publishers, 'both')) |> 
  filter(publishers != '') |> 
  distinct(publishers)

#checking which of the main publishers need to be cleaned

publishers |> 
  filter(str_detect(publishers, 'Nintendo'))
publishers |> 
  filter(str_detect(publishers, 'EA ')) ##needs editing
publishers |> 
  filter(str_detect(publishers, 'Sega')) ##needs editing
publishers |> 
  filter(str_detect(publishers, 'Activision')) ##needs editing
publishers |> 
  filter(str_detect(publishers, 'Ubisoft'))
publishers |> 
  filter(str_detect(publishers, 'Konami')) ## needs editing
publishers |> 
  filter(str_detect(publishers, 'Square.Enix')) ##needs editing
publishers |> 
  filter(str_detect(publishers, 'Bandai')) ##needs editing
publishers |> 
  filter(str_detect(publishers, 'Capcom')) ##needs editing

#NOTE: there are certainly more instances where a subsidiary is observed in the data
# changing these publishers, however, is important for stock analysis


publishers <- 
  publishers |> 
  mutate(publishers = case_when(str_detect(publishers, 'EA ') ~ 'Electronic Arts',
                                str_detect(publishers, 'Bandai') ~ 'Bandai Namco',
                                str_detect(publishers, 'Capcom') ~ 'Capcom',
                                str_detect(publishers, 'Konami') ~ 'Konami',
                                str_detect(publishers, '(2K)|(Rockstar Games)|(Zynga)|(Private Division)') ~ 'Take-Two',
                                str_detect(publishers, 'Sega') ~ 'Sega',
                                str_detect(publishers, 'Square.Enix') ~ 'Square Enix',
                                str_detect(publishers, '(Activision)|(Blizzard)') ~ 'Activision Blizzard',
                                .default = publishers
                                ),
         publishers = tolower(publishers)) |> 
  distinct()

publishers |> 
  filter(str_detect(publishers, 'rock'))

##NOTE: take-two comprises (2K, Rockstar Games, Zynga, Private Division)
##NOTE: D3 Publisher is a part of Bandai Namco
#check square enix (Crystal Dynamics) and ubisoft (Blue Mammoth Games) subsidaries

publishers <-
  publishers|> 
  rowid_to_column('publisher_id')

################################################################################


######################################
### DEALING WITH PUBLISHERS STOCKS ###
######################################

#helper function to find the conversions
conversion_finder <- function(currency = 'JPYUSD', from_date = '2013-06-30', to_date = '2023-06-30'){
  tq_get(paste0(currency, '=X'), get = 'stock.prices', from = from_date, to = to_date) |> 
    select(symbol, date, adjusted) |> 
    rename(exchange = adjusted, conversion = symbol) |> 
    mutate(conversion = str_remove(conversion, '=X'))
}

jpy_usd <- conversion_finder()
eur_usd <- conversion_finder('EURUSD')

#for jpy to usd converted stocks
jpy_usd_converted_stocks <- 
  function(stock = '9766.T', company_name = 'Konami', 
           from_date = '2013-06-30', to_date = '2023-06-30'){
    
  tq_get(stock, get = 'stock.prices', from = from_date, to = to_date) |> 
      inner_join(jpy_usd |> select(date, exchange), join_by(date)) |> 
      mutate(across(open:adjusted, \(x) x*exchange),
             symbol = company_name) |> 
      select(!exchange) |> 
      rename(publisher = symbol)
  }

#for eud to usa converted stocks
eur_usd_converted_stocks <- 
  function(stock = 'UBI.PA', company_name = 'Ubisoft', 
           from_date = '2013-06-30', to_date = '2023-06-30'){
    
    tq_get(stock, get = 'stock.prices', from = from_date, to = to_date) |> 
      inner_join(eur_usd |> select(date, exchange), join_by(date)) |> 
      mutate(across(open:adjusted, \(x) x*exchange),
             symbol = company_name) |> 
      select(!exchange) |> 
      rename(publisher = symbol)
  }

#need to use this for Konami, Square Enix, Sega, Bandai Namco, Capcom
jpy_usd_converted_stocks()

#for ubisoft
eur_usd_converted_stocks()

#Nintendo and EA already use USD

publisher_stocks <- data.frame()

jpy_stocks <- list(c('9684.T', 'Square Enix'), c('7832.T', 'Bandai Namco'),
                c('6460.T', 'Sega'), c('9697.T', 'Capcom'), c('9766.T', 'Konami'))

for(publisher in jpy_stocks){
  publisher_stocks <- rbind(publisher_stocks,
    jpy_usd_converted_stocks(publisher[1], publisher[2]))
}

publisher_stocks <- rbind(publisher_stocks, eur_usd_converted_stocks())

usd_stocks <- list(c('NTDOY', 'Nintendo'), c('EA', 'Electronic Arts'),
                   c('TTWO', 'Take-Two'))

for(publisher in usd_stocks){
  
  publisher_stocks <- rbind(publisher_stocks,
  tq_get(publisher[1], get = 'stock.prices', from = '2013-06-30', to = '2023-06-30') |> 
    mutate(symbol = publisher[2]) |> 
    rename(publisher = symbol))
}

publisher_stocks <- 
publisher_stocks |>
  mutate(publisher = tolower(publisher))

################################
### DEALING WITH GAME AWARDS ###
################################

#112 or our 556 games are missing from the games index (various reasons)
# probably ok for our study concerns, but we can add these games to the games dg
# in the future for more percision 

game_awards <- read_csv('data/raw/game_awards.csv')

#removing unwanted categories
removed_categories <- c('Best Student Game', 'Student Game Award',
                        'Best Adaptation', 'Best Remaster')

#cleaning up the category names
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

game_awards_edited <-
  game_awards |> 
  mutate(game = str_trim(game, 'both'),
         game = tolower(game),
         category = str_remove(category, '\\[.*\\]'),
         category = str_replace(category, ' / ', '/')) |> 
  filter(!(category %in% removed_categories)) |> 
  inner_join(games, join_by(game)) |> 
  select(!c(game)) |> 
  relocate(game_id, .after = date)
################################################################################

### THE LONG AWAITED SOLUTION ###

game_awards |> 
  mutate(game = str_trim(game, 'both'),
         game = tolower(game),
         category = str_remove(category, '\\[.*\\]'),
         category = str_replace(category, ' / ', '/')) |> 
  filter(!(category %in% removed_categories)) |> 
  inner_join(games, join_by(game)) |> 
  select(!c(game)) |> 
  relocate(game_id, .after = date) |>  
  inner_join(game_info |> inner_join(games, join_by(name)),
             join_by(game_id)) |> 
  distinct(date, game_id, category, .keep_all = TRUE)
################################################################################
game_info |> 
  inner_join(games, join_by(name))

for(category_change in category_cleanup){
  game_awards_edited <-
    game_awards_edited |> 
    mutate(category = if_else(str_detect(category, category_change[2]),
                              category_change[1],
                              category))
}


## I was banking on the fact that the game names and publishers would match up well
# that help fairly true for game names, but publishers is a different story. Some of 
# the publishers are really just individual people, but other times the game loses 
# it's publisher after merging with publishers...
##solution <- merge games on game_info_raw (which will have publisher ids), then
# merge game_info_raw on game_awards and keep game_id and publisher_id (so dropping publishers
# from game awards entirely)

game_awards_edited <-
game_awards_edited |>
  separate_rows(publisher, sep = ',') |> 
  separate_rows(publisher, sep = '/') |> 
  mutate(publisher = tolower(publisher),
         publisher = str_remove(publisher, '‡'),
         publisher = str_trim(publisher, 'both')) |> 
  filter(publisher != '') |>  
  mutate(publisher = case_when(str_detect(publisher, 'square.enix') ~ 'square enix',
                               str_detect(publisher, 'nintendo') ~ 'nintendo',
                               str_detect(publisher, '(activision)|(blizzard)') ~ 'activision blizzard',
                               str_detect(publisher, '2k') ~ '2k',
                               str_detect(publisher, '^ea') ~ 'electronic arts',
                               str_detect(publisher, 'ubisoft') ~ 'ubisoft',
                               str_detect(publisher, 'bandai namco') ~ 'bandai namco',
                               str_detect(publisher, 'sonic') ~ 'sega',
                               .default = publisher)) |> 
  left_join(publishers, join_by(publisher == publishers)) 



#these are all the games that got dropped in the merging because they are not in
# the games df
uh_oh <-
  game_awards |> 
  mutate(game = str_trim(game, 'both'),
         game = tolower(game)) |> 
  filter(!(category %in% removed_categories)) |> 
  anti_join(games, join_by(game == name)) |> 
  distinct(game)

games |> 
  filter(str_detect(name, 'persona'))

################################################################################

##########################################
### DEALING WITH GAME AWARD CATEGORIES ###
##########################################

#once we've edited the games we want to see in game awards, finding the game award
# categories is simple

award_category <- 
  game_awards_edited |> 
  distinct(category) |> 
  rowid_to_column('category_id')


################################################################################

##################################
### DEALING WITH GAME REVIEWS  ###
##################################

game_reviews <- read_csv('data/raw/game_reviews.csv')

### game_review_companies ###

game_review_companies <- 
  game_reviews |> 
  select(review_company) |>
  mutate(review_company = tolower(review_company)) |> 
  distinct() |> 
  rowid_to_column('review_company_id')

### game_reviewers ###

game_reviewers <-
game_reviews |> 
  mutate(reviewer = if_else(is.na(reviewer), review_company, reviewer),
         reviewer = tolower(reviewer),
         review_company = tolower(review_company)) |> 
  distinct(pick(reviewer, review_company)) |> 
  arrange(reviewer) |> 
  inner_join(game_review_companies, join_by(review_company)) |> 
  rowid_to_column('reviewer_id') #|>
  #keep the review_company names for now for merging purposes
  #select(!c(review_company))


#we have to join on reviewer/review_company combinations to make this work, make
# sure to reflect this on the relational db design

#There are a whole lotta small-fry games in the raw game_reviews df, I'm only really
# concerned about notable games for this EDA. For now, let's look at games that have
# more than 10 reviews

### game reviews edited df ###

game_reviews_edited <- 
game_reviews |> 
  mutate(reviewer = if_else(is.na(reviewer), review_company, reviewer),
         reviewer = tolower(reviewer),
         review_company = tolower(review_company)) |> 
  inner_join(game_reviewers, join_by(reviewer, review_company)) |> 
  select(!c(reviewer, review_company)) |> 
  relocate(c(reviewer_id, review_company_id)) 
  #dealing withe the game ids
game_reviews_edited <- 
  game_reviews_edited |> 
  anti_join(game_reviews_edited |> 
              count(game) |> 
              filter(n < 10), join_by(game)) |> 
  mutate(game = str_replace_all(game, '_', ' '),
         game = str_trim(game, 'both')) |> 
  inner_join(games, join_by(game)) |> 
  select(!c(game)) |> 
  relocate(game_id)
  
game_reviews_edited 

#game_reviews_edited |> 
#  mutate(game = str_replace_all(game, '_', ' '),
#         game = str_trim(game, 'both')) |> 
#  anti_join(games, join_by(game == name)) |> 
#  distinct(game) |> 
#  arrange(game) 

#There are a whole lotta small-fry games in the raw game_reviews df, I'm only really
# concerned about notable games for this EDA. For now, let's look at games that have
# more than 10 reviews

#all games in the df games with 10+ reviews
game_reviews_edited |> 
  anti_join(game_reviews_edited |> 
            count(game) |> 
              filter(n < 10), join_by(game)) |> 
  mutate(game = str_replace_all(game, '_', ' '),
         game = str_trim(game, 'both')) |> 
  inner_join(games, join_by(game == name)) |> 
  distinct(game)

#all games with 10+ reviews
game_reviews_edited |> 
  anti_join(game_reviews_edited |> 
              count(game) |> 
              filter(n < 10), join_by(game)) |> 
  distinct(game)


#gotta get rid of that game_reviewers part
game_reviewers <-
game_reviewers |> 
  select(!c(review_company))

################################################################################

#############################
### DEALING WITH ESPORTS  ###
#############################

esports <- read_csv('data/raw/esports.csv')

#of course, some games in this list were released before my game releases, so
# just check if a game in esports doesn't match in games it's because that game
# is too old and not because I'm missing that game in games

#there are some instances where games are not matched because i removed the paranthesis
# in games, so just remove them in esports too

## UPDATE: we lose a lot of games when we merge because gamers apparently like to 
# play very old games. That's ok, we still get a decent sample, enough for analysis

esport_games <-
esports |> 
  filter(Date >= as_date('2013-06-30') &
           Date <= as_date('2023-06-30')) |> 
  janitor::clean_names() |> 
  mutate(game = str_remove(game, '\\s\\(.*\\)'),
         game = tolower(game)) |>
  inner_join(games, join_by(game)) |> 
  select(!c(game)) |> 
  relocate(game_id, .after = date)

esports |> 
  filter(Date >= as_date('2013-06-30') &
           Date <= as_date('2023-06-30')) |> 
  janitor::clean_names() |> 
  mutate(game = str_remove(game, '\\s\\(.*\\)'),
         game = tolower(game)) |> 
  anti_join(games, join_by(game)) |> 
  distinct(game) |> 
  print(n = 320)

esports |> 
  filter(Date >= as_date('2013-06-30') &
           Date <= as_date('2023-06-30')) |> 
  janitor::clean_names() |> 
  mutate(game = str_remove(game, '\\s\\(.*\\)')) |> 
  distinct(game)

#check Asuka 120% LimitOver BURNING Fest. (not sure if that percentage is something)

