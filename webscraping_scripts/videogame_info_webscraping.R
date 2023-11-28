
#####################################
#####################################
#####   Game Info Web Scraping  #####
#####################################
#####################################

################################################################################

#########################
#####   Libraries   #####
#########################

library(tidyverse)
library(rvest)

################################################################################

################################################
######    Get Wikipedia links to games    ######
################################################

###########################################
######    General scrape function    ######
###########################################

#gets video game info, given part of a link
get_games_info <- function(link){
  html <- read_html(paste0('https://en.wikipedia.org', link))
  
  new_column_names <- c('x', 'y')
  ##etting the table and the desired columns

#some of the links aren't even video games (some are links to other media that the
# game is based off of), so we're using a tryCatch to resolve this issue
result <- tryCatch({
  
  html |> 
  html_element('.infobox') |> 
  html_table() |> 
  janitor::clean_names() |> 
  rename_all(~new_column_names) |> 
  #getting the specified variables from the table read-in
  filter(x == 'Publisher(s)' | x == 'Series' | 
             x == 'Release' |  x == 'Genre(s)' |
             x == 'Mode(s)') |> 
    add_row(x = 'Name', y = str_extract(link, '[^/wiki/].*'), .before = 1) |> 
   
   pivot_wider(
      names_from = 'x',
      values_from = 'y'
    ) |> 
    janitor::clean_names() |> 
    #mutate(name = str_replace_all(name, '%22', '"'),
    #       name = str_replace_all(name, '%3F', '?'),
    #       name = str_replace_all(name, '%27', "'"),
    #       name = str_replace_all(name, 'é')) |> 
    #cleaning up the column names a bit more
    rename(publishers = publisher_s,
           genres = genre_s,
           modes = mode_s) |> 
    #games with multiple rows and genres are separated by a ','
    # creating a new row for these different values
    separate_rows(modes, sep = ", ") |> 
    separate_rows(genres, sep = ", ")

},

error = function(e) { NULL })
  
  if(!is.null(result)){ result }
  

}

##########################################
######    Cleaning release dates    ######
##########################################

#Many games have separate release dates for different world regions
# I am only concerned about the initial release
# release_extractor extracts the initial release date
release_extractor <- function(df){
  
  #concerned about the possibility of there not being a release date on the 
  # Wikipedia page
  if(!('release' %in% names(df))){
    df <-
    df |> 
      add_column(release = NA, .before = 'genres') |> 
      mutate(release = as.character(release))
  }
  
  
  df |> 
    mutate(
      #these two cases should be able to capture all the release dates (revise later)
      release = case_when(  
        str_detect(release, '\\d\\d?\\s[A-Z]\\D{2,8},?\\s\\d{4}') 
          ~ as.character(dmy(str_extract(release, '\\d\\d?\\s[A-Z]\\D{2,8},?\\s\\d{4}'))),
        str_detect(release, '[A-Z][a-z]{1,8}\\s\\d{1,2}[st]?[rd]?[th]?,?\\s\\d{4}') 
          ~ as.character(mdy(str_extract(release, '[A-Z][a-z]{1,8}\\s\\d{1,2}[st]?[rd]?[th]?,?\\s\\d{4}'))),
        str_detect(release, ':') 
          ~ str_remove(release, '^.*:\\s'),
        str_detect(release, '[A-Z][a-z]{1,8},?\\s\\d{4}') 
          ~ as.character(dmy(paste0('1 ', str_extract(release, '[A-Z][a-z]{1,8},?\\s\\d{4}')))),
        .default = release
        ))
  
}


####################################################
######    Dealing with multiple publishers    ######
####################################################


#a character vector of all publishers
publisher_list <-
  
  pull((read_html('https://en.wikipedia.org/wiki/List_of_video_game_publishers') |> 
          html_elements('.wikitable') |>
          html_elements('tbody') |> 
          html_table())[[2]] |> 
         select(Publisher)) |> 
  tolower()

#NOTE: this process banks on the fact that publishers on the Wikipedia page are 
# hyper linked and therefore can be extracted into publisher_list
# this is NOT the case for all games --> must revise this process

#helper function --> finds publishers directly from Wikipedia usually the previously
# made publisher_list
is_publisher <- function(x, publisher_list){
  
  publisher <- c()
  for(element in x){
    element <- tolower(element)
  if(element %in% publisher_list){
    publisher <- c(publisher, element)
  }}

  paste0(publisher, collapse = ",")
}

publisher_cleaner <- function(df, link = '/wiki/Loop8:_Summer_of_Gods'){
  
  #finding all the titles for the links in the infobox on the Wikipage, which 
  # (should) include the publishers
  #publisher <- unique(
  #  c(read_html(paste0('https://en.wikipedia.org', link)) |> 
  #  html_elements('.infobox') |> 
  #  html_elements('.infobox-data') |> 
  #  html_elements('a') |> 
  #  html_attr('title'),
    
  #  read_html(paste0('https://en.wikipedia.org', link)) |> 
  #    html_elements('.infobox') |> 
  #    html_elements('td') |> 
  #    html_text()
  #))
  
  publisher <-
  read_html(paste0('https://en.wikipedia.org', link)) |> 
    html_element('.infobox') |> 
    html_elements('tr') |> 
    html_text2(preserve_nbsp = TRUE)
  
  
  publisher_index <- which(grepl("Publisher\\(s\\)", publisher))
  
  publisher <- publisher[publisher_index]
  
  publisher <- gsub('\\.mw-parser-output .plainlist ol,.mw-parser-output .plainlist ul\\{line-height:inherit;list-style:none;margin:0;padding:0\\}\\.mw-parser-output .plainlist ol li,.mw-parser-output .plainlist ul li\\{margin-bottom:0\\}\\\n',
                    '',
                    publisher)
  
  publisher <- gsub('Publisher\\(s\\)\\\t',
                    '',
                    publisher)
  
  
  
  #attempting to try to resolve the missing links problem, these lines of code don't
  # affect the output at the moment
  #publisher <- c(publisher, read_html(paste0('https://en.wikipedia.org', link)) |> 
  #                 html_elements('.infobox') |> 
  #                 html_elements('.infobox-data') |> 
  #                 html_text())
  
  df |> 
    #
    mutate(publishers = publisher,
           publishers = str_remove_all(publishers, "[A-Z][A-Z]+\\:\\s?"),
           publishers = str_remove_all(publishers, '\\(.*\\)')) |> 
    separate_rows(publishers, sep = "\\\n") |> 
    filter(publishers != '')
  
 # df |> 
    #
  #  mutate(publishers = is_publisher(publisher, publisher_list)) |> 
   # separate_rows(publishers, sep = ",")
}

########################################################
######    Making the missing series columns NA    ######
########################################################

#Many games are not part of a game series, if this is the case, series was not
# created as a column in get_games_info, we'll do that process here
add_series <- function(df, link = '/wiki/Loop8:_Summer_of_Gods'){
  
  if(!('series' %in% names(df))){
    
    df |> 
      add_column(series = NA, .after = 'publishers') |> 
      mutate(series = as.character(series))
  }
  else{df}
  
  ### CHECK IF IT WILL STAY NA IF SERIE(S) ISNT IN THERE
 # series <-
  #  read_html(paste0('https://en.wikipedia.org', link)) |> 
  #  html_element('.infobox') |> 
  #  html_elements('tr') |> 
  #  html_text2(preserve_nbsp = TRUE)
  
  
#  genre_index <- which(grepl("Genre\\(s\\)", genre))
  
#  genre <- genre[genre_index]
  
#  genre <- gsub('\\.mw-parser-output.*\\{margin-bottom:0\\}\\\n',
#                '',
#                genre)
  
  
  
#  genre <- gsub('Genre\\(s\\)\\\t',
#                '',
#                genre)
  
#  df |> 
    #
#    mutate(genres = genre,
#           genres = str_remove_all(genres, "\\[[0-9]*\\]"),
#           genres = str_remove_all(genres, '\\(.*\\)')) |> 
#    separate_rows(genres, sep = ",") |> 
#    separate_rows(genres, sep = '\\\n')
  
#}
  
  
  
}

#########################################
######    Cleaning up the names    ######
#########################################

#Some of the games use special characters (like & or ?) that the Wikipedia link
# converts to html-readable text
# this function converts these characters back and gets rid of the '_' between words

name_cleaner <- function(df){
  
  df |> 
    mutate(name = str_replace_all(name, '%22', '"'),
           name = str_replace_all(name, '%3F', '?'),
           name = str_replace_all(name, '%27', "'"),
           name = str_replace_all(name, '%C3%A9', 'é'),
           name = str_replace_all(name, '%2B', '+'),
           name = str_replace_all(name, '%26', '&'),
           name = str_replace_all(name, '_', ' ')
           )
}

######################################
######    Clean those genres    ######
######################################

genre_cleaner <- function(df, link = '/wiki/Loop8:_Summer_of_Gods'){
  

  genre <-
    read_html(paste0('https://en.wikipedia.org', link)) |> 
    html_element('.infobox') |> 
    html_elements('tr') |> 
    html_text2(preserve_nbsp = TRUE)
  
  
  genre_index <- which(grepl("Genre\\(s\\)", genre))
  
  genre <- genre[genre_index]
  
  genre <- gsub('\\.mw-parser-output.*\\{margin-bottom:0\\}\\\n',
                    '',
                    genre)
  
  
  
  genre <- gsub('Genre\\(s\\)\\\t',
                    '',
                    genre)
  
  df |> 
    #
    mutate(genres = genre,
           genres = str_remove_all(genres, "\\[[0-9]*\\]"),
           genres = str_remove_all(genres, '\\(.*\\)')) |> 
    separate_rows(genres, sep = ",") |> 
    separate_rows(genres, sep = '\\\n')
  
}

################################################################################

#######################################
#####   get_games_list function   #####
#######################################

get_games_list <- function(link = '/wiki/Category:2022_video_games'){
  
  #NOTE: when I initially tried running this function, I would get timeout errors
  # since I was trying to read in so many html pages at once, I used a tryCatch
  # template from Chat GPT that, if there is an error, pauses the process for 10
  # seconds before restarting, this can happen 10 times before the function terminates
  # itself
  
  max_attempts <- 10  # Maximum number of attempts
  attempt <- 1       # Initialize attempt counter
  
  while (attempt <= max_attempts) {
    tryCatch({
  
  wikipage <- read_html(paste0('https://en.wikipedia.org', link))
  
  #empty dataset to rbind everything together
  games_info_df <- data.frame()
  
  #getting the games links from the year-overview pages
  games_links <-  
    wikipage |> 
    html_elements('.mw-category-group') |> 
    html_elements('a') |> 
    html_attr('href')
  
  for(game in games_links){
    #get the game info
    
      #again, some of these links aren't actually games, checking to make sure
      if(!is.null(get_games_info(game))){
        
        #printing this part of the process just for a visual effect to see that 
        # the function is looping correctly over each game
        print(get_games_info(game))
        
        #using all the cleaning functions
        game_info <-
        get_games_info(game) |> 
          release_extractor() |> 
          publisher_cleaner(game) |> 
          add_series() |> 
          name_cleaner() |> 
          genre_cleaner()
        
        games_info_df <- rbind(games_info_df, game_info)
      }
    next
    
  } #end of for-loop
    
  #in the games-by-year overview page, there could be occurrences of additional 
  # pages with more games for that year, if these if-statements find another page,
  # the function will iterate over itself again
    if(((wikipage |> 
        html_elements('.mw-category-generated') |> 
        html_element('#mw-pages') |> 
        html_elements('a') |> 
        html_text())[2]) == 'next page'){
      
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
  
  #there is the possibility that some games are duplicated (same game on different
  # pages)
  distinct(games_info_df)
}

################################################################################

#######################################
#####   Putting it all together   #####
#######################################

#empty dataset that will store all games
game_info <- data.frame()

game_info <-
rbind(game_info,
      get_games_list('/wiki/Category:2013_video_games'),
      get_games_list('/wiki/Category:2014_video_games'),
      get_games_list('/wiki/Category:2015_video_games'),
      get_games_list('/wiki/Category:2016_video_games'),
      get_games_list('/wiki/Category:2017_video_games'),
      get_games_list('/wiki/Category:2018_video_games'),
      get_games_list('/wiki/Category:2019_video_games'),
      get_games_list('/wiki/Category:2020_video_games'),
      get_games_list('/wiki/Category:2021_video_games'),
      get_games_list('/wiki/Category:2022_video_games'),
      get_games_list('/wiki/Category:2023_video_games')
      )


for( year in c(2013:2023)){
  game_info <- rbind(game_info, 
                          get_games_list(rlang::englue('/wiki/Category:{year}_video_games')))
}

write_csv(game_info, 'data\raw\game_info.csv')

################################################################################

test_2023 <- get_games_list('/wiki/Category:2023_video_games')


get_games_info('/wiki/Karateka_(video_game)') |> 
  #select(release)
  release_extractor() #|> 
  publisher_cleaner('/wiki/Karateka_(video_game)') |> 
  add_series() |> 
  name_cleaner() |> 
  genre_cleaner()

test_uh2 <-  
unlist(test_uh |> 
  distinct(release))

print(test_uh2)

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
  