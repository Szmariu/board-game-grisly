### Libraries
library(tidyverse)
library(ggplot2)

### Import data
reviews <- 'data/bgg-13m-reviews.csv' %>%
  read_csv()

games <- 'data/games_detailed_info.csv' %>%
  read_csv()

### Format the data 
# Drop the index
colnames(reviews)

reviews <- reviews %>% 
  select(-X1)


# Rename and drop some variables
colnames(games)

games <- games %>%
  select(
    "Abstract Game Rank", 
    "Board Game Rank",              
    "Children's Game Rank",  
    "Family Game Rank",            
    "Party Game Rank",             
    "Strategy Game Rank",
    "Thematic Rank",               
    "War Game Rank",                             
    "Ratings average" = "average",                      
    "Complexity" = "averageweight",                
    "Geekscore" = "bayesaverage",
    "Category" = "boardgamecategory",
    "Mechanics" = "boardgamemechanic",           
    'Publisher' = "boardgamepublisher",
    'Description' = "description",                 
    "id",        
    "image",                    
    "maxplayers",                  
    "maxplaytime",                 
    "minage",     
    "minplayers",          
    "minplaytime",    
    "numcomments",    
    "numweights",
    "owned",         
    "playingtime",                 
    "name" = "primary",                     
    "Ratings standard deviation" = "stddev",                      
    "suggested_num_players", 
    "suggested_playerage",         
    "thumbnail",          
    "trading",                      
    "usersrated",        
    "wanting",
    "wishing", 
    "yearpublished"
  )
