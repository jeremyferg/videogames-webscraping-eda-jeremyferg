################################################################################
################################################################################
################################################################################

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
      #these four cases capture most the release dates
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
  else{
    df |> 
      category_cleaner(var = series, col_str = "^Series", link)
    
    
  }
  
}

#########################################
######    Cleaning up the names    ######
#########################################

#Some of the games use special characters (like & or ?) that the Wikipedia link
# converts to html-readable text
# this function converts these characters back and gets rid of the '_' between words

special_chr = list(c('%22', '"'), c('%3F', '?'), c('%27', "'"), c('%C3%A9', 'é'),
                   c('%2B', '+'), c('%26', '&'), c('%CF%87', 'χ'), c('%C5%8D', 'ō'),
                   c('%E2%80%93', '–'), c('%E2%80%99', "'"), c('%C5%AB', 'ū'),
                   c('%C3%A6', 'æ'), c('%E2%99%AF', '♯'), c('%C3%BB', 'û'),
                   c('%C3%97', '×'), c('%C3%B6', 'ö'), c('%E2%88%92', '−'),
                   c('_', ' ')
                   )

name_cleaner <- function(df){
  
  for(translation in special_chr){
    
  df <-
  df |> 
    mutate(name = str_replace_all(name, translation[1], translation[2]))
  }
  
  df
}

################################################################################

############################################
######    General Category Cleaner    ######
############################################

category_cleaner <- function(df, var = publishers, col_str = "Publisher\\(s\\)", 
                             link = '/wiki/Loop8:_Summer_of_Gods'){
  
  
  category <-
    read_html(paste0('https://en.wikipedia.org', link)) |> 
    html_element('.infobox') |> 
    html_elements('tr') |> 
    html_text2(preserve_nbsp = TRUE)
  
  category_index <- which(grepl(col_str, category))
  
  category <- category[category_index]
  
  category <- gsub('\\.mw-parser-output.*\\{.*\\}',
                '',
                category)
  
  
  
  category <- gsub(paste0(col_str, '\\\t'),
                '',
                category)
  
  df |> 
    
    mutate({{var}} := category,
           {{var}} := str_remove_all({{var}}, '\\[.*\\]'),
           {{var}} := str_remove_all({{var}}, '\\(.*\\)')) |> 
    separate_rows({{var}}, sep = ",") |> 
    separate_rows({{var}}, sep = '\\\n') |> 
    filter({{var}} != '') |>
    mutate({{var}} := trimws({{var}}, 'both'))
  
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
          category_cleaner(var = publishers, col_str = "Publisher\\(s\\)", link = game) |> 
          add_series(link = game) |> 
          name_cleaner() |> 
          category_cleaner(var = genres, col_str = "Genre\\(s\\)", link = game) |> 
          category_cleaner(var = modes, col_str = "Mode\\(s\\)", link = game)
        
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


for( year in c(2013:2023)){
  game_info <- rbind(game_info, 
                          get_games_list(rlang::englue('/wiki/Category:{year}_video_games')))
}

write_csv(game_info, 'data/raw/game_info_raw.csv')

################################################################################
################################################################################
################################################################################
  