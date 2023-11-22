
##############################################
##############################################
#####   OpenCritic Reviews Web Scraping  #####
##############################################
##############################################

################################################################################

#########################
#####   Libraries   #####
#########################

library(tidyverse)
library(rvest)

################################################################################

##################################
#####   Getting game links   #####
##################################

#finds all the reviewed-game links
get_game_links <-
function(link = 'https://opencritic.com/browse/all/2013/date?page=', 
         n = 1){
  
  page_link <- read_html(paste0(link, n))
  
  #used to merge all links into one c()
  links_list <- c()
  
  links_list <- c(links_list,
  page_link |> 
    html_elements('.desktop-game-display') |> 
    html_elements('a') |> 
    html_attr('href')
  )
  
  #if the next page number has game reviews on it...
  if(length(page_link |> 
            html_elements('.desktop-game-display') |> 
            html_elements('a'))
    > 0)
  #...iterate over to the next page of game reviews
  {links_list <- c(links_list, get_game_links(link, n+1))}
  
links_list
}

#used to merge all game links into one c() 
game_links <- c()

#finding the game links for games released between 2013 and 2023
for(year in c(2013:2023)){

  game_links <- c(game_links,
                  get_game_links(link = 
                                   rlang::englue('https://opencritic.com/browse/all/{year}/date?page='))
  )
}
  
################################################################################

############################################
##### Functions for making the dataset #####
############################################

#################################
#####   Get the game name   #####
#################################

#gets the title of the game by extracting the name from the review link
get_game_title <- function(link){
  
  #we want the text between these two parts of the link
  removed_parts <- c('https\\:\\//opencritic\\.com\\/game\\/\\d*\\/', '\\/reviews\\?page=')
  
  game_title <- str_remove(link, removed_parts[1])
  game_title <- str_remove(game_title, removed_parts[2])
  
  #the links replace spaces with '-', so i'm replacing '-' with '_'
  game_title <- str_replace_all(game_title, '-', '_')
  
  game_title
}

##################################################
#####   Getting ratings table for one game   #####
##################################################

ratings_date_tibble <- 
  function(link = 'https://opencritic.com/game/15131/super-mario-rpg/reviews?page=', 
           n = 1){
  
  #dataframe that the function can rbind over
  ratings_dates_tibble <- data.frame()
  
  #we create a tibble with columns `reviewer`, `review_company`, `rating`, and
  # `date`
  ratings_dates_tibble <- rbind(ratings_dates_tibble,
    (tibble(
      
      reviewer = c(
        read_html(paste0(link, n)) |> 
          html_elements('.col.author-info') |> 
          html_elements('.author-name') |> 
          html_text()
      ),
      
      review_company = c(
        read_html(paste0(link, n)) |> 
          html_elements('.col.author-info') |> 
          html_elements('.outlet-name') |> 
          html_text()
      ),
      
      rating = c(
        read_html(paste0(link, n)) |> 
          html_elements('app-review-row') |> 
          html_elements('.d-flex') |> 
          html_elements('span') |> 
          html_text()
      ),
      
      date = c(
        read_html(paste0(link, n)) |> 
          html_elements('app-review-row') |> 
          html_elements('.d-flex') |> 
          html_elements('.text-right.date-block') |> 
          html_text()
      ))))
  
  #if the next page number has reviews on it...
  if(length(read_html(
    paste0(link, n+1)) |> 
            html_elements('app-review-row'))
     > 0)
    
  #...iterate over the next page of reviews
  {ratings_dates_tibble <- rbind(ratings_dates_tibble,
                             ratings_date_tibble(link, n+1))}

  
  #getting rid of any of the ratings that used star images on the page
  ratings_dates_tibble |> 
    filter(str_detect(rating, '[1|5]0?') & !str_detect(rating, '[A-Z|a-z]'))
           
  
  }

#####################################
#####   Avg ratings yesterday   #####
#####################################        

#this function needs to be called after we have all ratings for the game

#finds the average reviewer score based on all the ratings that were made before
# the date of a given observation
avg_rating_yesterday <- function(review_tibble){
  
  #finding the unique dates that reviews were made
  #NOTE: the tibble is arranged by date later to make this method work
  ratings_dates <- 
    unlist(review_tibble |> 
    distinct(as.character(date)))

  #the first date in ratings_dates should have a `rating_yesterday` of 0
  # so, first we set rating_yesterday equal to 0 if the observation has the first
  # date and NA if the observation as a date other than the first date
  review_tibble <-
  review_tibble |> 
    mutate(rating_yesterday = if_else(date == date(ratings_dates[1]), 0, NA))
  
  #removing the first date from ratings_dates
  ratings_dates <- ratings_dates[-1]
  
  #finding rating_yesterday for all other dates
  for(some_date in ratings_dates){
    
    da_mean <-
      unlist(review_tibble |> 
               filter(date < date(some_date)) |> 
               summarise(
                 mean = mean(rating)
               ))
    
  review_tibble <-
    review_tibble |> 
      mutate(rating_yesterday = if_else(date == some_date, round(da_mean, 2), rating_yesterday))
      
  }
  
  review_tibble
  
}

#######################################
#####   Putting it all together   #####
#######################################

#dataframe that the for-loop can rbind over
ratings_data <- data.frame()

#for every game between 2013-2023...
for(game in game_links){
  
  review_link = paste0('https://opencritic.com', game, '/reviews?page=')
  
  
  ratings_data <- 
    rbind(ratings_data,
          #...get the information specified above
          ratings_date_tibble(link = review_link) |> 
            mutate(game = get_game_title(review_link),
                   #changing the ratings so they are XX out of 100
                   rating = (if_else(str_detect(rating, '\\%'),
                                     as.double(str_extract(rating, '\\d\\d?')),
                                     ((as.double(str_extract(rating, "\\d+\\.?\\d*"))/
                                         (as.double(str_extract(rating, "[1|5]0?\\.?0?$")))
                                     ))*100)),
                   date = mdy(date),
                   #most reviewer observations have \n at the end of their string, removing this
                   reviewer = str_extract(reviewer, '[^\n]+')) |> 
            #MUST arrange by date before using avg_rating_yesterday()
            arrange(date) |> 
            avg_rating_yesterday()
                        
                        )
}

write_csv(ratings_data, 'game_reviews.csv')

read_csv('game_reviews.csv')

################################################################################

#####################################
#####   Testing on Elden Ring   #####
#####################################

ratings_date_tibble(link = 'https://opencritic.com/game/12090/elden-ring/reviews?page=') |> 
  mutate(game = get_game_title('https://opencritic.com/game/12090/elden-ring/reviews?page='),
         #changing the ratings so they are XX out of 100
         rating = (if_else(str_detect(rating, '\\%'),
                           as.double(str_extract(rating, '\\d\\d?')),
                           ((as.double(str_extract(rating, "\\d+\\.?\\d*"))/
                               (as.double(str_extract(rating, "[1|5]0?\\.?0?$")))
                           ))*100)),
         date = mdy(date),
         #most reviewer observations have \n at the end of their string, removing this
         reviewer = str_extract(reviewer, '[^\n]+')) |> 
  #must arrange by date before using avg_rating_yesterday()
  arrange(date) |> 
  avg_rating_yesterday()
