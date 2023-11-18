
############################################
############################################
#####   The Game Awards Web scraping   #####
############################################
############################################

#########################
#####   Libraries   #####
#########################

library(tidyverse)
library(rvest)

################################################################################

############################
##### HELPER FUNCTIONS #####
############################

#helper function to find the date of the game awards
get_award_date <- function(wikipage){
  mdy(str_extract(
    read_html(wikipage) |> 
      html_element('.infobox-data') |> 
      html_text(),
    
    '[A-Z][a-z]{1,8}\\s\\d{1,2},?\\s\\d{4}'
    
  ))
}

################################################################################

######################################
##### FUNCTION FOR 2014 AND 2015 #####
######################################

game_awards_2014_2015 <- 
  function(wikipage = 'https://en.wikipedia.org/wiki/The_Game_Awards_2014'){

#initial scraped_table, but the observations are very disorganized
# no defined column
scraped_table <-
  (
    read_html(wikipage) |> 
      html_element('.wikitable') |> 
      html_elements('tbody') |> 
      html_table()
  )[[1]] |> 
  janitor::clean_names() |> 
  add_row(game_of_the_year = 'Game of the Year', 
          developer_of_the_year = 'Developer of the Year',
          .before = 1)

#making scraped_table into one column
# very helpful when trying to rearrange the columns so we have cateogories on 
# one side and everything else on the other
scraped_table_one_col <-
  scraped_table |> 
  rename(
    developer_of_the_year = 'game_of_the_year',
    game_of_the_year  = 'developer_of_the_year') |> 
  relocate(game_of_the_year) |> 
  rbind(scraped_table) |> 
  select(game_of_the_year)

#cleaning up even further
scraped_table <-
  scraped_table_one_col |> 
  filter(!str_detect(game_of_the_year, '‡')) |> 
  rowid_to_column("id") |> 
  
  inner_join(
    scraped_table_one_col |> 
      filter(str_detect(game_of_the_year, '‡')) |> 
      rowid_to_column("id"),
    join_by(id)
    
  ) |> 
  select(game_of_the_year.x, game_of_the_year.y) |> 
  
  separate_rows(game_of_the_year.y, sep = '\n')  |> 
  separate(game_of_the_year.y, 
           into = c('game', 'publisher'), 
           sep = '– ', 
           fill = 'left') |> 
  mutate(winner = if_else(str_detect(publisher, '‡'),
                          TRUE, 
                          FALSE),
         date = get_award_date(wikipage = wikipage)) |> 
  rename(category = 'game_of_the_year.x')

scraped_table

}

################################################################################

awards_2014_2015 <-
game_awards_2014_2015('https://en.wikipedia.org/wiki/The_Game_Awards_2015')

################################################################################

####################################
##### FUNCTION FOR 2016 - 2022 #####
####################################

game_awards_2016_2022 <- 
  function(wikipage = 'https://en.wikipedia.org/wiki/The_Game_Awards_2016'){
    
    #initial scraped_table, but the observations are very disorganized
    # no defined column
    scraped_table <-
      (
        read_html(wikipage) |> 
          html_element('.wikitable') |> 
          html_elements('tbody') |> 
          html_table()
      )[[1]] |> 
      janitor::clean_names() |> 
      add_row(game_of_the_year = 'Game of the Year', 
              best_game_direction = 'Best Game Direction',
              .before = 1)
    
    #making scraped_table into one column
    # very helpful when trying to rearrange the columns so we have categories on 
    # one side and everything else on the other
    scraped_table_one_col <-
      scraped_table |> 
      rename(
        best_game_direction = 'game_of_the_year',
        game_of_the_year  = 'best_game_direction') |> 
      relocate(game_of_the_year) |> 
      rbind(scraped_table) |> 
      select(game_of_the_year)
    
    #cleaning up even further
    scraped_table <-
      scraped_table_one_col |> 
      filter(!str_detect(game_of_the_year, '‡')) |> 
      rowid_to_column("id") |> 
      
      inner_join(
        scraped_table_one_col |> 
          filter(str_detect(game_of_the_year, '‡')) |> 
          rowid_to_column("id"),
        join_by(id)
        
      ) |> 
      select(game_of_the_year.x, game_of_the_year.y) |> 
      
      separate_rows(game_of_the_year.y, sep = '\n')  |> 
      separate(game_of_the_year.y, 
               into = c('game', 'publisher'), 
               sep = '– ', 
               extra = 'merge',
               fill = 'left') |> 
      mutate(winner = if_else(str_detect(publisher, '‡'),
                              TRUE, 
                              FALSE),
             date = get_award_date(wikipage = wikipage)) |> 
      rename(category = 'game_of_the_year.x')
    
    scraped_table
    
  }

################################################################################

## getting 2016 - 2022 data

awards_2016_2022 <- data.frame()

for( year in c(2016:2022)){
    awards_2016_2022 <-
  rbind(awards_2016_2022, game_awards_2016_2022(
      rlang::englue('https://en.wikipedia.org/wiki/The_Game_Awards_{year}'))
      )
  }

################################################################################

## put the two datasets together

game_awards_dataset <-
rbind(awards_2014_2015, awards_2016_2022) |> 
  separate_rows(publisher, sep = ' /') |> 
  relocate(date)


#####################
#####   NOTES   #####  
#####################

## the way adding the row developer of the year during this process will only 
##  work for years 2014 and 2015 (this category was dropped after)
##  current solution: add a variable in the function that specifies what the
##  name of the category should be

## '‡' indicates winners. There are no winners for 2023, so will either need to
##  find another way to detect the string, or drop the year entirely

## that '‡' is still at the end of winning publishers in the dataset, make sure
## to get rid of that when you get the chance

## a lot of the game categories in the dataset could be merged into one category
## will have to pick and choose how i do this


  