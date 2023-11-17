
### Packages ----

library(tidyverse)
library(rvest)

#######################################
######    wiki links to games    ######
#######################################

#practicing with various 2013 games
games_2013_0toD <- read_html('https://en.wikipedia.org/wiki/Category:2013_video_games')

games_2013_0toD |> 
  html_elements('.mw-category-group') |> 
  html_elements('a') |> 
  html_attr('href')

###########################################
######    general scrape function    ######
###########################################

#gets video game info, given part of a link
get_games_info <- function(link){
  html <- read_html(paste0('https://en.wikipedia.org', link))
  
  new_column_names <- c('x', 'y')
  ##getting the table and the desired columns

#some of the links arent even video games, so we're doing a tryCatch to resolve
  #that
result <- tryCatch({
  
  html |> 
  html_element('.infobox') |> 
  html_table() |> 
  janitor::clean_names() |> 
  rename_all(~new_column_names) |> 
  filter(x == 'Publisher(s)' | x == 'Series' | 
             x == 'Release' |  x == 'Genre(s)' |
             x == 'Mode(s)') |> 
    add_row(x = 'Name', y = str_extract(link, '[^/wiki/].*'), .before = 1) |> 
   
   pivot_wider(
      names_from = 'x',
      values_from = 'y'
    ) |> 
    janitor::clean_names() |> 
    rename(publishers = publisher_s,
           genres = genre_s,
           modes = mode_s) |> 
    separate_rows(modes, sep = ", ") |> 
    separate_rows(genres, sep = ", ")

},

error = function(e) { NULL })
  
  if(!is.null(result)){ result }
  

}

################################################
######    cleaning those release dates    ######
################################################

#Many games have separate release dates for different world regions
#I am only concerned about the initial release
#release_cleaner extracts the initial release date

release_extractor <- function(df, var = release){
  
  if(!('release' %in% names(df))){
    df <-
    df |> 
      add_column(release = NA, .before = 'genres') |> 
      mutate(release = as.character(release))
  }
  
  df |> 
    mutate(
      release = case_when(  #\\d{1,2}
        str_detect(release, '[1-2]{1,2}\\s[A-Z]\\D{2,8}\\s\\d{4}') ~ as.character(dmy(str_extract(release, '[1-2]{1,2}\\s[A-Z]\\D{2,8}\\s\\d{4}'))),
        str_detect(release, '[A-Z][a-z]{1,8}\\s\\d{1,2},?\\s\\d{4}') ~ as.character(mdy(str_extract(release, '[A-Z][a-z]{1,8}\\s\\d{1,2},?\\s\\d{4}'))),
        .default = release))
  
    #mutate(release = str_extract({{var}},
    #                                 
    #                                '(?<=\\b[^\\d])January\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sJanuary,?\\s\\d{4}(?=\\D|$)|
    #                                |January\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sJanuary,?\\s\\d{4}(?=\\D|$)|
    #                                
    #                                |(?<=\\b[^\\d])February\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sFebruary,?\\s\\d{4}(?=\\D|$)|
    #                                |February\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sFebruary,?\\s\\d{4}(?=\\D|$)|             
    #                               
    #                                |(?<=\\b[^\\d])March\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sMarch,?\\s\\d{4}(?=\\D|$)|
    #                                |March\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|              
    #                                |\\d{1,2}\\sMarch,?\\s\\d{4}(?=\\D|$)|             
    #                                 
    #                                |(?<=\\b[^\\d])April\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sApril,?\\s\\d{4}(?=\\D|$)|
    #                                |April\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|              
    #                                |\\d{1,2}\\sApril,?\\s\\d{4}(?=\\D|$)|              
    #                                 
    #                                |(?<=\\b[^\\d])May\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sMay,?\\s\\d{4}(?=\\D|$)|
    #                                |May\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sMay,?\\s\\d{4}(?=\\D|$)|
    #                                
    #                                |(?<=\\b[^\\d])June\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sJune,?\\s\\d{4}(?=\\D|$)|
    #                                |June\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sJune,?\\s\\d{4}(?=\\D|$)|             
    #                                 
    #                                |(?<=\\b[^\\d])July\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sJuly,?\\s\\d{4}(?=\\D|$)|
    #                                |July\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sJuly,?\\s\\d{4}(?=\\D|$)|
    #                                 
    #                                |(?<=\\b[^\\d])August\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sAugust,?\\s\\d{4}(?=\\D|$)|
    #                                |August\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sAugust,?\\s\\d{4}(?=\\D|$)| 
    #                                 
    #                                |(?<=\\b[^\\d])September\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sSeptember,?\\s\\d{4}(?=\\D|$)|
    #                                |September\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sSeptember,?\\s\\d{4}(?=\\D|$)|
    #                                
    #                                |(?<=\\b[^\\d])October\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sOctober,?\\s\\d{4}(?=\\D|$)|
    #                                |October\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sOctober,?\\s\\d{4}(?=\\D|$)|
    #                                 
    #                                |(?<=\\b[^\\d])November\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sNovember,?\\s\\d{4}(?=\\D|$)|
    #                                |November\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sNovember,?\\s\\d{4}(?=\\D|$)|
    #                                 
    #                                |(?<=\\b[^\\d])December\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |(?<=\\b[^\\d])\\d{1,2}\\sDecember,?\\s\\d{4}(?=\\D|$)|
    #                                |December\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
    #                                |\\d{1,2}\\sDecember,?\\s\\d{4}(?=\\D|$)'),
    #       release = case_when(
    #         str_detect(release, '\\d{1,2}\\s\\D{3,9}\\s\\d{4}') ~ as.character(dmy(release)),
    #         str_detect(release, '\\D{3,9}\\s\\d{1,2},?\\s\\d{4}') ~ as.character(mdy(release)),
    #         .default = release))
}


####################################################
######    dealing with multiple publishers    ######
####################################################


#a character vector of all publishers
publisher_list <-
  
  pull((read_html('https://en.wikipedia.org/wiki/List_of_video_game_publishers') |> 
          html_elements('.wikitable') |>
          html_elements('tbody') |> 
          html_table())[[2]] |> 
         select(Publisher)) |> 
  tolower()

#NOTE: this process banks on the fact that publishers on the wiki page are linked
#      and therefore could be extracted into publisher_list

#helper function --> finds publishers directly from wikipage
is_publisher <- function(x, publisher_list){
  
  publisher <- c()
  for(element in x){
    element <- tolower(element)
  if(element %in% publisher_list){
    publisher <- c(publisher, element)
  }}
  
  #chance the developer and publisher are the same
  #publisher <- unique(publisher)
  
  paste0(publisher, collapse = ",")
}

publisher_cleaner <- function(df, link = '/wiki/Loop8:_Summer_of_Gods'){
  
  #finding all the title for the links in the infobox on the wikipage, which 
  #includes the publishers
  publisher <- read_html(paste0('https://en.wikipedia.org', link)) |> 
    html_elements('.infobox') |> 
    html_elements('.infobox-data') |> 
    html_elements('a') |> 
    html_attr('title')
  
  publisher <- c(publisher, read_html(paste0('https://en.wikipedia.org', link)) |> 
                   html_elements('.infobox') |> 
                   html_elements('.infobox-data') |> 
                   html_text())
  
  df |> 
    mutate(publishers = is_publisher(publisher, publisher_list)) |> 
    separate_rows(publishers, sep = ",")
}

########################################################
######    making the missing series columns NA    ######
########################################################

add_series <- function(df){
  
  if(!('series' %in% names(df))){
    
    df |> 
      add_column(series = NA, .after = 'publishers') |> 
      mutate(series = as.character(series))
  }
  else{df}
  
}

#######################################
######    get the wiki links!    ######
#######################################

games_2022_0toN <- read_html('https://en.wikipedia.org/wiki/Category:2022_video_games')
games_2022_0toN_p2 <- read_html('https://en.wikipedia.org/w/index.php?title=Category:2022_video_games&pagefrom=Nine+to+Five+%28video+game%29#mw-pages')

#############################
## get_games_list function ##
#############################

get_games_list <- function(link = '/wiki/Category:2022_video_games'){
  
  max_attempts <- 10  # Maximum number of attempts
  attempt <- 1       # Initialize attempt counter
  
  while (attempt <= max_attempts) {
    tryCatch({
  
  wikipage <- read_html(paste0('https://en.wikipedia.org', link))
  games_info_df <- data.frame()
  
  games_links <-  
    wikipage |> 
    html_elements('.mw-category-group') |> 
    html_elements('a') |> 
    html_attr('href')
  
  for(game in games_links){
    #get the game info
    #Sys.sleep(1)
      if(!is.null(get_games_info(game))){
        
        #printing it to see it go and go
        print(get_games_info(game))
        
        game_info <-
        get_games_info(game) |> 
          release_extractor() |> 
          publisher_cleaner(game) |> 
          add_series()
        
        games_info_df <- rbind(games_info_df, game_info)
      }
    next
    
  } #end of for loop
    
    if(((wikipage |> 
        html_elements('.mw-category-generated') |> 
        html_element('#mw-pages') |> 
        html_elements('a') |> 
        html_text())[2]) == 'next page'){
      
      
      print('ayo')
      
      next_page <- 
        ((wikipage |> 
           html_elements('.mw-category-generated') |> 
           html_element('#mw-pages') |> 
           html_elements('a') |> 
           html_attr('href'))[2])
      
      games_info_df <- rbind(games_info_df, get_games_list(next_page))
    }
    
    else if(((wikipage |> 
             html_elements('.mw-category-generated') |> 
             html_element('#mw-pages') |> 
             html_elements('a') |> 
             html_text())[3]) == "next page"){
      
      print('ayo')
      
      next_page <- 
        ((wikipage |> 
           html_elements('.mw-category-generated') |> 
           html_element('#mw-pages') |> 
           html_elements('a') |> 
           html_attr('href'))[3])
      
      games_info_df <- rbind(games_info_df, get_games_list(next_page))
    }
  
  # If the connection is successful, break out of the loop
  break
    }, error = function(e) {
      # Handle the error here, such as displaying a message
      print(paste("Connection attempt", attempt, "failed:", conditionMessage(e)))
      
      # Increment the attempt counter
      attempt <- attempt + 1
      
      # Wait for a specified delay before attempting the connection again
      Sys.sleep(10)  # Adjust the delay time (in seconds) as needed
    })
  }
  
  # If max_attempts reached without successful connection
  if (attempt > max_attempts) {
    print("Max attempts reached. Unable to establish a connection.")
  }
  
  distinct(games_info_df)
}

#############################
## for loop over each year ##
#############################


### trying individually first #################################################

games_2013 <-
  get_games_list('/wiki/Category:2013_video_games')

games_2014 <-
  get_games_list('/wiki/Category:2014_video_games')

games_2015 <-
  get_games_list('/wiki/Category:2015_video_games')

games_2016 <-
  get_games_list('/wiki/Category:2016_video_games')

games_2017 <-
  get_games_list('/wiki/Category:2017_video_games')

games_2018 <-
  get_games_list('/wiki/Category:2018_video_games')

games_2019 <-
  get_games_list('/wiki/Category:2019_video_games')

games_2020 <-
  get_games_list('/wiki/Category:2020_video_games')

games_2021 <-
  get_games_list('/wiki/Category:2021_video_games')

games_2022 <-
  get_games_list('/wiki/Category:2022_video_games')

games_2023 <-
  get_games_list('/wiki/Category:2023_video_games')

### now the for loop ##########################################################


videogame_info <- data.frame()

videogame_info <-
for( year in c(2013:2023)){
  videogame_info <- rbind(videogame_info, 
                          get_games_list(rlang::englue('/wiki/Category:{year}_video_games')))
}

###############################################################################

### TryCatch function ####

max_attempts <- 5  # Maximum number of attempts
attempt <- 1       # Initialize attempt counter

while (attempt <= max_attempts) {
  tryCatch({
    
    
    
    # If the connection is successful, break out of the loop
    break
  }, error = function(e) {
    # Handle the error here, such as displaying a message
    print(paste("Connection attempt", attempt, "failed:", conditionMessage(e)))
    
    # Increment the attempt counter
    attempt <- attempt + 1
    
    # Wait for a specified delay before attempting the connection again
    Sys.sleep(5)  # Adjust the delay time (in seconds) as needed
  })
}

# If max_attempts reached without successful connection
if (attempt > max_attempts) {
  print("Max attempts reached. Unable to establish a connection.")
}


###############################################################
#######    check how all the functions work together!    ######
###############################################################

test_df <- data.frame()

get_games_info('/wiki/Loop8:_Summer_of_Gods') |> 
  release_extractor() |> 
  publisher_cleaner('/wiki/Loop8:_Summer_of_Gods') |> 
  add_series()

get_games_info('/wiki/Battlefield_4') |> 
  release_extractor() |>
  publisher_cleaner('/wiki/Battlefield_4') |> 
  add_series()

test_html <- read_html('https://en.wikipedia.org/wiki/Miss_Kobayashi%27s_Dragon_Maid')
test_html2 <- read_html('https://en.wikipedia.org/wiki/Miss_Kobayashi%27s_Dragon_Maid:_Burst_Forth!!_Choro-gon_Breath')
test_html3 <- read_html('https://en.wikipedia.org/wiki/Loop8:_Summer_of_Gods')

test_html |> 
  html_elements('.infobox') |> 
  html_table() |> 
  janitor::clean_names()  

test_html2 |> 
  html_elements('.infobox') |> 
  html_table() |> 
  rename_all(~new_column_names) 

test_html |> 
  html_element('.infobox') |> 
  html_table() |> 
  janitor::clean_names() |> 
  rename_all(~new_column_names) |> 
  filter(x == 'Publisher(s)' | x == 'Series' | 
           x == 'Release' |  x == 'Genre(s)' |
           x == 'Mode(s)') |> 
  add_row(x = 'Name', y = str_extract('Miss_Kobayashi%27s_Dragon_Maid', '[^/wiki/].*'), .before = 1)

get_games_info('/wiki/Aquamarine_(video_game)')  |> 
  publisher_cleaner('/wiki/Aquamarine_(video_game)')

get_games_list('https://en.wikipedia.org/wiki/Category:2022_video_games')

read_html(paste0('https://en.wikipedia.org', link)) |> 
  html_elements('.infobox') |> 
  html_elements('.infobox-data') |> 
  html_text()

#################################################
## new publisher list, a lot more observations ##
#################################################

new_publisher_list <-
pull((read_html('https://www.gamesdatabase.org/publishers') |> 
  html_elements('.NoWrap') |> 
  html_table())[[1]] |> 
  select(X2) |> 
  mutate(X2 = tolower(X2)))



test_og_publisher_list |> 
  filter(publishers != "") |> 
  semi_join(test_new_publisher_list |> filter(publishers != ""),
             join_by(publishers))

test <-
distinct(test |> 
  separate_rows(genres, sep = ", "))

test |> 
  filter(is.na(release))

get_games_info('/wiki/20_Minutes_Till_Dawn') |> 
  select(release)

get_games_info('/wiki/Endling:_Extinction_is_Forever') |> 
  select(release)
  
  mutate(
  release = case_when(
    str_detect(release, '(?<=\\s|^)\\d{1,2}\\s\\D{3,9}\\s\\d{4}\\b') ~ as.character(mdy(str_extract(release, '(?<=\\s|^)\\d{1,2}\\s\\D{3,9}\\s\\d{4}\\b'))),
    str_detect(release, '\\b\\D{3,9}\\s\\d{1,2},?\\s\\d{4}\\b') ~ as.character(mdy(str_extract(release, '\\b\\D{3,9}\\s\\d{1,2},?\\s\\d{4}\\b'))),
    .default = release)) |> 
  select(release)


  
  
get_games_info('/wiki/Apico') |> 
  mutate(
    release = case_when(
    str_detect(release, '\\d{1,2}\\s[A-Z]\\D{2,8}\\s\\d{4}') ~ as.character(dmy(str_extract(release, '\\d{1,2}\\s[A-Z]\\D{2,8}\\s\\d{4}'))),
    str_detect(release, '[A-Z][a-z]\\D{1,7}\\s\\d{1,2},?\\s\\d{4}') ~ as.character(mdy(str_extract(release, '[A-Z][a-z]\\D{1,7}\\s\\d{1,2},?\\s\\d{4}'))),
    .default = release))

test_new_publisher_list |> 
  select(release)

str_extract('Windows, Switch, PlayStation 4, Xbox OneJuly 19, 2022PS5, Xbox', '[A-Z][a-z]{1,8}\\s\\d{1,2},?\\s\\d{4}')

paste0('https://en.wikipedia.org', 
       ((read_html('https://en.wikipedia.org/wiki/Category:2022_video_games') |> 
    html_elements('.mw-category-generated') |> 
    html_element('div') |> 
    html_elements('a') |> 
    html_attr('href'))[2])
)

game_test <-
get_games_info('/wiki/Redshirt_(video_game)')  
  if(!('release' %in% names(game_test))){
    game_test |> 
      add_column(release = NA, .before = 'genres') |> 
      mutate(release = as.character(release))
  }
  

get_games_info('/wiki/Oxenfree_II:_Lost_Signals') |> 
  release_extractor() |> 
  publisher_cleaner('/wiki/Oxenfree_II:_Lost_Signals') |> 
  add_series



##############################
######    some notes    ######
##############################

# ideally, we can make genre_s and mode_s of type factor, watch for variations
# with this variable

# there are going to be different writings for release, make sure there are no NAs
# for starters and then try to convert the variables into type date

# a lot of publishers aren't publishing because they are either not in the publisher
# list of they are not linked as my publisher function suggests. Try scraping for both
# links and text and then using is_publisher() on both of them
  

