
############################################
############################################
#####   The Game Awards Web scraping   #####
############################################
############################################

################################################################################

#########################
#####   Libraries   #####
#########################

library(tidyverse)
library(rvest)

################################################################################

############################################
#####   Find date of the Game Awards   #####
############################################

#the first (and only) date in the infobox is the date of the Game Awards
# lucky for us, this date is always formatted the same
get_award_date <- function(wikipage){
  mdy(str_extract(
    read_html(wikipage) |> 
      html_element('.infobox-data') |> 
      html_text(),
    
    '[A-Z][a-z]{1,8}\\s\\d{1,2},?\\s\\d{4}'
    
  ))
}

################################################################################

#########################################################
#####   Get Game Awards results for 2014 and 2015   #####
#########################################################

#the first two columns of the awards/nominees table of 2014 and 2015 are different
# compared to the other years, so these years have their own, slightly modified version
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
          #the difference between 2014/2015 and the other years is developer_of_the_year
          # being the name of the second column
          #after 2014, this category was dropped, so this name does not exist 
          # for the other years
          developer_of_the_year = 'Developer of the Year',
          .before = 1)

#making scraped_table into one column
# very helpful when trying to rearrange the columns so we have categories on 
# one side and winners/nominees on the other
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
  #joining a column of only award categories with a column of only winners/nominees
  inner_join(
    scraped_table_one_col |> 
      filter(str_detect(game_of_the_year, '‡')) |> 
      rowid_to_column("id"),
    join_by(id)
    
  ) |> 
  #game_of_the_year.x is a column of the award categories
  #game_of_the_year.y is a column of the winners/nominees
  select(game_of_the_year.x, game_of_the_year.y) |> 
  
  #separate the winners/nominees into their own row
  separate_rows(game_of_the_year.y, sep = '\n')  |> 
  #winners/nominees are written as both the game and the publisher, so separate
  # these two values into different columns
  separate(game_of_the_year.y, 
           into = c('game', 'publisher'), 
           sep = '– ', 
           fill = 'left') |> 
  #'‡' indicates a winner and is used in `winner` to indicate whether or not the
  #' game won the category in that year
  mutate(winner = if_else(str_detect(publisher, '‡'),
                          TRUE, 
                          FALSE),
         date = get_award_date(wikipage = wikipage)) |> 
  rename(category = 'game_of_the_year.x')

scraped_table

}

################################################################################

###########################################################
#####   Putting the 2014/2015 function all together   #####
###########################################################

awards_2014_2015 <-
  rbind(game_awards_2014_2015(),
game_awards_2014_2015('https://en.wikipedia.org/wiki/The_Game_Awards_2015')
)

################################################################################

#############################################################
#####   Get Game Awards results for 2016 through 2022   #####
#############################################################

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
              #recall developper of the year was discontinued after 2015
              # best_game_direction replaces developer_of_the_year as the initial
              # name of the second column for the next years
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
      #joining a column of only award categories with a column of only winners/nominees
      inner_join(
        scraped_table_one_col |> 
          filter(str_detect(game_of_the_year, '‡')) |> 
          rowid_to_column("id"),
        join_by(id)
        
      ) |>
      #game_of_the_year.x is a column of the award categories
      #game_of_the_year.y is a column of the winners/nominees
      select(game_of_the_year.x, game_of_the_year.y) |> 
      
      #separate the winners/nominees into their own row
      separate_rows(game_of_the_year.y, sep = '\n')  |> 
      #winners/nominees are written as both the game and the publisher, so separate
      # these two values into different columns
      separate(game_of_the_year.y, 
               into = c('game', 'publisher'), 
               sep = '– ', 
               extra = 'merge',
               fill = 'left') |> 
      #'‡' indicates a winner and is used in `winner` to indicate whether or not the
      #' game won the category in that year
      mutate(winner = if_else(str_detect(publisher, '‡'),
                              TRUE, 
                              FALSE),
             date = get_award_date(wikipage = wikipage)) |> 
      rename(category = 'game_of_the_year.x')
    
    scraped_table
    
  }

################################################################################

###################################################################
#####   Putting the 2016 through 2022 function all together   #####
###################################################################

#creating an empty dataset to rbind each year onto
awards_2016_2022 <- data.frame()

for( year in c(2016:2022)){
    awards_2016_2022 <-
  rbind(awards_2016_2022, game_awards_2016_2022(
      rlang::englue('https://en.wikipedia.org/wiki/The_Game_Awards_{year}'))
      )
  }

################################################################################

#################################################
#####   Putting the two datasets together   #####
#################################################

game_awards <-
rbind(awards_2014_2015, awards_2016_2022) |> 
  #games with multiple publishers are separated by /
  separate_rows(publisher, sep = ' /') |> 
  #putting date in the front of the tibble
  relocate(date)

write_csv(game_awards, 'game_awards.csv')


################################################################################

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
  