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


# Basic filters
# Lets remove some outliers to make life easier
# Some outliers are ok, eg. for most popular games the "wishing" will be very high

games %>%
  ggplot(aes(x = yearpublished)) +
  geom_density()

games %>%
  ggplot(aes(x = maxplayers)) +
  geom_density()

games %>%
  ggplot(aes(x = maxplaytime)) +
  geom_density()

games %>%
  ggplot(aes(x = minplaytime)) +
  geom_density()

games %>%
  ggplot(aes(x = playingtime)) +
  geom_density()

games <- games %>% 
  filter(yearpublished > 1950,
         maxplayers < 20,
         maxplaytime < 600,
         maxplaytime < 600,
         playingtime < 600,
         suggested_num_players < 20)

scale_color_tech()

# Create binary variables 
# A game can have multiple main categories, so we can't create one column
games$isAbstract <- as.integer( !is.na(games$Abstract_Rank) )
games$isChildrens <- as.integer( !is.na(games$Childrens_Rank) )
games$isFamily <- as.integer( !is.na(games$Family_Rank) )
games$isParty <- as.integer( !is.na(games$Party_Rank) )
games$isThematic <- as.integer( !is.na(games$Thematic_Rank) )
games$isWarGame <- as.integer( !is.na(games$War_Game_Rank) )


# Questions:
# 



plot <- games %>%
  select( "Rank",
         "Ratings_average",
         "Complexity",
         "Geekscore",
         "minage",               
         "minplayers",           
         "minplaytime",          
         "playingtime",         
         "Ratings_std_dev",   
         "usersrated",           
         "yearpublished") %>%
  ggpairs(progress = TRUE,
          axisLabels = 'none') + 
  theme(text = element_text(size = 10))

# Takes a long while to save
plot %>% 
  ggsave('001.jpg', .)


# Year
games %>%
  ggplot(aes(x = yearpublished)) +
  geom_density(color = orange, fill = orange, alpha = 0.5)   

# Average ratings vs year (>2000)
plot <- games %>% 
  filter(yearpublished > 2009) %>%
  ggplot(aes(x = Ratings_average)) +
  geom_density(color = orange, fill = orange, alpha = 0.5) +  
  labs(title = 'Year: {floor(frame_time)}', x = 'Ratings', y = 'Density') +
  transition_time(yearpublished) +
  ease_aes('linear') 

plot %>% animate(width = 1200, height = 800)

anim_save('001.gif', path = paste0(getwd(), '/plots'))



# Average ratings vs year (>2000)
plot <- games %>% 
  filter(yearpublished > 1999,
         Complexity > 0.5) %>%
  ggplot(aes(x = Complexity)) +
  geom_histogram(color = pink, fill = pink, alpha = 0.5) +  
  labs(title = 'Year: {floor(frame_time)}', x = 'Complexity', y = 'Count') +
  transition_time(yearpublished) +
  ease_aes('linear') 

plot %>% animate(width = 1200, height = 800)

anim_save('002.gif', path = paste0(getwd(), '/plots'))








