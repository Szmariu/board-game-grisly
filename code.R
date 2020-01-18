### Libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(cowplot)
library(gganimate)
library(ggridges)
library(viridis)
library(ggtech) # For the airbnb theme
library(extrafont) # To add the font for Airbnb theme
library(GGally) # Grapihical correlogram

# Download this fonts and install manually
# https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Medium%203.46.45%20PM.ttf
# https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Bold%203.46.45%20PM.ttf

# Import all fonts, takes a few minutes but removes the annoying warnings
# font_import()
# font_import(pattern = 'Circular', prompt=FALSE)
# loadfonts(device = "win")

# Lets set the theme to this nice one from Airbnb
# Colors: c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051")
theme_set(theme_airbnb_fancy())
pink = "#FF5A5F"
orange = "#FFB400"
blueGreen = "#007A87"
flesh = "#FFAA91"
purple = "#7B0051"

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
    "Abstract_Rank" = "Abstract Game Rank", 
    "Rank" = "Board Game Rank",              
    "Childrens_Rank" = "Children's Game Rank",  
    "Family_Rank" = "Family Game Rank",            
    "Party_Rank" = "Party Game Rank",             
    "Strategy_Rank" = "Strategy Game Rank",
    "Thematic_Rank" = "Thematic Rank",               
    "War_Game_Rank" = "War Game Rank",                             
    "Ratings_average" = "average",                      
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
    "Ratings_std_dev" = "stddev",                      
    "suggested_num_players", 
    "suggested_playerage",         
    "thumbnail",          
    "trading",                      
    "usersrated",        
    "wanting",
    "wishing", 
    "yearpublished"
  )
