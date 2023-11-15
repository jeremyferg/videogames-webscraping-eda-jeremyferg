
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
    separate_rows(mode_s, sep = ", ")
}

################################################
######    cleaning those release dates    ######
################################################

#Many games have separate release dates for different world regions
#I am only concerned about the initial release
#release_cleaner extracts the initial release date

release_extractor <- function(df, var = release){
  df |> 
    mutate(release = str_extract({{var}},
                                     
                                    '(?<=\\b[^\\d])January\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sJanuary,?\\s\\d{4}(?=\\D|$)|
                                    |January\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sJanuary,?\\s\\d{4}(?=\\D|$)|
                                    
                                    |(?<=\\b[^\\d])February\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sFebruary,?\\s\\d{4}(?=\\D|$)|
                                    |February\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sFebruary,?\\s\\d{4}(?=\\D|$)|             
                                   
                                    |(?<=\\b[^\\d])March\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sMarch,?\\s\\d{4}(?=\\D|$)|
                                    |March\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|              
                                    |\\d{1,2}\\sMarch,?\\s\\d{4}(?=\\D|$)|             
                                     
                                    |(?<=\\b[^\\d])April\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sApril,?\\s\\d{4}(?=\\D|$)|
                                    |April\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|              
                                    |\\d{1,2}\\sApril,?\\s\\d{4}(?=\\D|$)|              
                                     
                                    |(?<=\\b[^\\d])May\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sMay,?\\s\\d{4}(?=\\D|$)|
                                    |May\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sMay,?\\s\\d{4}(?=\\D|$)|
                                    
                                    |(?<=\\b[^\\d])June\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sJune,?\\s\\d{4}(?=\\D|$)|
                                    |June\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sJune,?\\s\\d{4}(?=\\D|$)|             
                                     
                                    |(?<=\\b[^\\d])July\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sJuly,?\\s\\d{4}(?=\\D|$)|
                                    |July\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sJuly,?\\s\\d{4}(?=\\D|$)|
                                     
                                    |(?<=\\b[^\\d])August\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sAugust,?\\s\\d{4}(?=\\D|$)|
                                    |August\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sAugust,?\\s\\d{4}(?=\\D|$)| 
                                     
                                    |(?<=\\b[^\\d])September\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sSeptember,?\\s\\d{4}(?=\\D|$)|
                                    |September\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sSeptember,?\\s\\d{4}(?=\\D|$)|
                                    
                                    |(?<=\\b[^\\d])October\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sOctober,?\\s\\d{4}(?=\\D|$)|
                                    |October\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sOctober,?\\s\\d{4}(?=\\D|$)|
                                     
                                    |(?<=\\b[^\\d])November\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sNovember,?\\s\\d{4}(?=\\D|$)|
                                    |November\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sNovember,?\\s\\d{4}(?=\\D|$)|
                                     
                                    |(?<=\\b[^\\d])December\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |(?<=\\b[^\\d])\\d{1,2}\\sDecember,?\\s\\d{4}(?=\\D|$)|
                                    |December\\s\\d{1,2},?\\s\\d{4}(?=\\D|$)|
                                    |\\d{1,2}\\sDecember,?\\s\\d{4}(?=\\D|$)'))
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
  
  df |> 
    mutate(publisher_s = is_publisher(publisher, publisher_list)) |> 
    separate_rows(publisher_s, sep = ",")
}

###############################################################
#######    check how all the functions work together!    ######
###############################################################

get_games_info('/wiki/Loop8:_Summer_of_Gods') |> 
  release_extractor() |> 
  publisher_cleaner('/wiki/Loop8:_Summer_of_Gods')

get_games_info('/wiki/Battlefield_4') |> 
  release_extractor() |> 
  publisher_cleaner('/wiki/Battlefield_4')


##############################
######    some notes    ######
##############################

# not all games have a series column, make sure you add that in

# ideally, we can make genre_s and mode_s of type category, watch for variations
# with this variable

# there are going to be different writings for release, make sure there are no NAs
# for starters and then try to convert the variables into type date
  

