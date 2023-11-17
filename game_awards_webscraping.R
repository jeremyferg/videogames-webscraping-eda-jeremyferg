
###########################################
###########################################
#####   The Game Awards Webscraping   #####
###########################################
###########################################

#########################
#####   Libraries   #####
#########################

library(tidyverse)
library(rvest)

#########################

wikipage_2023 <- read_html('https://en.wikipedia.org/wiki/The_Game_Awards_2023')
wikipage_2022 <- read_html('https://en.wikipedia.org/wiki/The_Game_Awards_2022')
wikipage_2014 <- read_html('https://en.wikipedia.org/wiki/The_Game_Awards_2014')

test <-
(
wikipage_2014 |> 
  html_element('.wikitable') |> 
  html_elements('tbody') |> 
  html_table()
)[[1]]

test <-
test |> 
  janitor::clean_names() |> 
  add_row(game_of_the_year = names(test)[1], 
          developer_of_the_year = names(test)[2],
           .before = 1)
  
test2 <-
  test |> 
    rename(
           developer_of_the_year = 'game_of_the_year',
           game_of_the_year  = 'developer_of_the_year') |> 
    relocate(game_of_the_year) |> 
    rbind(test) |> 
    select(game_of_the_year)

 
test2 |> 
  filter(!str_detect(game_of_the_year, '‡')) |> 
  rowid_to_column("id") |> 
  inner_join(
    test2 |> 
      filter(str_detect(game_of_the_year, '‡')) |> 
      rowid_to_column("id"),
    join_by(id)
  ) 

#####################
#####   NOTES   #####  
#####################

## the way adding the row developer of the year during this process will only 
##  work for years 2014 and 2015 (this category was dropped after)
##  current solution: add a variable in the function that specifies what the
##  name of the category should be

## '‡' indicates winners. There are no winners for 2023, so will either need to
##  find another way to detect the string, or drop the year entirely


  