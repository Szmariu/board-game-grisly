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
         playingtime < 600)

# Creat a binned complexity
games %>%
  ggplot(aes(x = Complexity)) +
  geom_histogram(bins = 60)

# Values are from 1-5, but there is very little after 4.5
games$ComplexityBinned <- games$Complexity %>% 
  cut(
    breaks = c(-Inf, 1.5, 2.5, 3.25, Inf),
    labels = c(1, 2, 3, 4))

table(games$ComplexityBinned)

# Create binary variables 
# A game can have multiple main categories, so we can't create one column
games$isAbstract <- as.integer( !is.na(games$Abstract_Rank) )
games$isChildrens <- as.integer( !is.na(games$Childrens_Rank) )
games$isFamily <- as.integer( !is.na(games$Family_Rank) )
games$isParty <- as.integer( !is.na(games$Party_Rank) )
games$isStrategy <- as.integer( !is.na(games$Strategy_Rank) )
games$isThematic <- as.integer( !is.na(games$Thematic_Rank) )
games$isWarGame <- as.integer( !is.na(games$War_Game_Rank) )

# Some overlap between categories
a <- games %>%
  group_by(isAbstract,
           isChildrens,
           isFamily,
           isParty,
           isStrategy,
           isThematic,
           isWarGame) %>%
  summarise(n = n()) %>%
  arrange(desc(n))

# Convert category ranks to quantiles
games$quantileAbstract <- games %>%
  .$Abstract_Rank / max(games$Abstract_Rank, na.rm = TRUE)

games$quantileChildrens <- games %>%
  .$Childrens_Rank / max(games$Childrens_Rank, na.rm = TRUE)

games$quantileFamily <- games %>%
  .$Family_Rank / max(games$Family_Rank, na.rm = TRUE)

games$quantileParty <- games %>%
  .$Party_Rank / max(games$Party_Rank, na.rm = TRUE)

games$quantileStrategy <- games %>%
  .$Strategy_Rank / max(games$Strategy_Rank, na.rm = TRUE)

games$quantileThematic <- games %>%
  .$Thematic_Rank / max(games$Thematic_Rank, na.rm = TRUE)

games$quantileWarGame <- games %>%
  .$War_Game_Rank / max(games$War_Game_Rank, na.rm = TRUE)


# Find the primary category
# How? By the lowest quantile, eg. the most important in this catergory
find_lowest_quantile <- function(row, output) {
  abstract = row["quantileAbstract"]
  family = row["quantileChildrens"]
  child = row["quantileFamily"]
  party = row["quantileParty"]
  strategy = row["quantileStrategy"]
  thematic = row["quantileThematic"]
  war = row["quantileWarGame"]
  
  lowest <- min(
    abstract,
    family,
    child,
    party,
    strategy,
    thematic,
    war,
    na.rm = TRUE)
  
  case_when(
    lowest == abstract ~ 'Abstract',
    lowest == family ~ 'Family',
    lowest == child ~ "Children",
    lowest == party ~ 'Party',
    lowest == strategy ~ 'Strategy',
    lowest == thematic ~ 'Thematic',
    lowest == war ~ 'War Game',
    is.na(lowest) ~ 'Other'
  ) %>%
    return()
}

# Takes a while
games$cat <- games %>%
  apply(1, find_lowest_quantile) %>%
  as.factor()

# A nice distribution, a lot of other 
games$cat %>% table()


# Questions:
# Does time increase with complexity?
# Rank vs popularity






# 001 - Lets look at the dataset
games %>%
  select( "Rank",
          "Ratings_average") %>%
  ggpairs(progress = TRUE,
          axisLabels = 'none',
          mapping = aes(color = games$ComplexityBinned),
          legend = 1) + 
  theme(text = element_text(size = 10))

my_dens <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_density_ridges(aes(y = games$ComplexityBinned, fill = games$ComplexityBinned), scale = 3, rel_min_height = 0.01, alpha = 0.7, color = NA ) + 
    scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051")) 
}

my_dots <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.1) + 
    scale_color_tech(theme="airbnb") 
}

plot <- games %>%
  select( "Rank",
          "Ratings_average",
          "yearpublished",
          "usersrated",
          "playingtime",   
          "usersrated") %>%
  ggpairs(
    mapping = aes(color = games$ComplexityBinned, fill = games$ComplexityBinned),
    diag = list(continuous = my_dens),
    lower = list(continuous = my_dots),
    progress = TRUE,
    axisLabels = 'none',
    legend = 1) + 
  theme(text = element_text(size = 10)) +
  labs(title = '')

plot %>% 
  ggsave('001.jpg', 
         .,
         path = paste0(getwd(), '/plots'),
         height = 9,
         width = 16)






# 002 - Boardgames per year
plot <- games %>%
  ggplot(aes(x = yearpublished)) +
  geom_density(color = orange, fill = orange, alpha = 0.5) +
  labs(title = 'Board games published per year', x = 'Year', y = 'Density')

plot %>% 
  ggsave('002.jpg', 
         . ,
         path = paste0(getwd(), '/plots'),
         width = 14,
         height = 13)






# 003 - Average ratings vs year (>2009)
plot <- games %>% 
  filter(yearpublished > 2009) %>%
  ggplot(aes(x = Ratings_average)) +
  geom_density(color = orange, fill = orange, alpha = 0.5) +  
  labs(title = 'Ratings of games published in: {floor(frame_time)}', x = 'Ratings', y = 'Density') +
  transition_time(yearpublished) +
  ease_aes('linear') 

plot %>% animate(width = 1920, height = 1080)

anim_save('003.gif', path = paste0(getwd(), '/plots'))





# 004 - Complexity vs rating
plot <- games %>%
  ggplot(aes(x = Ratings_average, y = ComplexityBinned, fill = ComplexityBinned)) +
  geom_density_ridges(scale = 3, rel_min_height = 0.01, alpha = 0.7, color = NA) + 
  scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051")) +
  scale_x_continuous(limits = c(3, 9.3)) +
  labs(title = 'Does the complexity of the game infuence the ratings?',
       x = 'Ratings',
       y = 'Complexity of the game',
       fill = 'Complexity')

plot %>% 
  ggsave('004.jpg', 
         .,
         path = paste0(getwd(), '/plots'),
         width = 14,
         height = 13)






# 005 - Rank vs average ratings
plot <- games %>% 
  ggplot(aes(x = Ratings_average, y = Rank)) +
  geom_point(aes(colour = Rank), alpha = 0.2) +
  scale_color_gradient(low = orange, high = purple) +
  labs(title = 'Does the average rating influence the final rank?',
       subtitle = 'Chicken Drumstick distribution',
       x = 'Average rating',
       y = 'Overall rank') +
  theme(legend.position = 'none') +
  scale_y_reverse()

plot %>% 
  ggsave('005.jpg', 
         . ,
         path = paste0(getwd(), '/plots'),  
         height = 7,
         width = 15)



# 006 Rank vs number of ratings
plot <- games %>% 
  ggplot(aes(x = log10(usersrated), y = Rank)) +
  geom_point(aes(colour = Rank), alpha = 0.3) + 
  scale_color_gradient(low = orange, high = purple) +
  labs(title = 'Does the number of user ratings influence the final rank?',
       subtitle = 'Jaws distribution',
       x = 'Log 10 of the number of ratings ',
       y = 'Overall rank') +
  theme(legend.position = 'none') +
  scale_y_reverse()

plot %>% 
  ggsave('006.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         height = 7,
         width = 15)




# 007 rank vs category
games$isTop <- ifelse(games$Rank < 300, 1, 0)

plot <- games %>%
  ggplot(aes(x = Rank, fill = cat)) +
  geom_density(alpha = 0.7, color = NA) + 
  facet_wrap('cat') + 
  scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051", "#FF5A5F", "#FFB400", "#007A87")) +
  theme(legend.position = 'none') +
  labs(title = 'Does the main category of the game infuence the rank?',
       subtitle = 'All games',
       x = 'Rank (lower is better)',
       y = 'Density')

plot %>% 
  ggsave('007-01.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         width = 14,
         height = 13)

plot <- games %>%
  filter(isTop == 1) %>%
  ggplot(aes(x = Rank, fill = cat)) +
  geom_histogram(alpha = 0.7, color = NA, bins = 30) + 
  facet_wrap('cat') + 
  scale_fill_manual(values = c("#FF5A5F", "#FFB400", "#007A87", "#FFAA91", "#7B0051", "#FF5A5F", "#FFB400", "#007A87")) +
  theme(legend.position = 'none') +
  labs(title = 'Does the main category of the game infuence the rank?',
       subtitle = 'Top 300 games',
       x = 'Rank (lower is better)',
       y = 'Count')

plot %>% 
  ggsave('007-02.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         width = 14,
         height = 13)


# See the top 1% of abstract games, and their rank
games %>%
  filter(quantileAbstract < 0.01 ) %>%
  {paste0(.$name, ' ' , .$Rank)}




# 008 Wishing vs rank
plot <- games %>%
  filter(wishing > 10) %>%
  ggplot(aes(x = log10(wishing), y = Rank)) +
  geom_point(aes(colour = Rank), alpha = 0.3) + 
  geom_smooth(color = purple) + 
  scale_color_gradient(low = orange, high = purple) +
  labs(title = 'Does the number of user wishlisting influence the final rank?',
       subtitle = 'Removed obeservations with < 10 users',
       x = 'Log 10 of the number of users wishlisting',
       y = 'Overall rank') +
  theme(legend.position = 'none') +
  scale_y_reverse()

plot %>% 
  ggsave('008.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         width = 14,
         height = 13)






# 009 - what does not influence 
# % of written comments
plot <- games %>%
  filter(numcomments / usersrated < 1) %>% 
  ggplot(aes(x = numcomments / usersrated , y = Rank)) +
  geom_point(aes(colour = Rank), alpha = 0.5) + 
  scale_color_gradient(low = orange, high = purple) +
  labs(title = '% of ratings with written reviews',
       x = 'Number of comments / Number of ratings',
       y = 'Overall rank') +
  theme(legend.position = 'none') +
  scale_y_reverse()

plot %>% 
  ggsave('009-01.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         width = 8,
         height = 9)


# Std dev 
plot <- games %>%
  ggplot(aes(x = Ratings_std_dev, y = Rank))+
  geom_point(aes(colour = Rank), alpha = 0.5) + 
  scale_color_gradient(low = orange, high = purple) +
  labs(title = 'How controversial the game is',
       x = 'Standard deviation of ratings',
       y = 'Overall rank') +
  theme(legend.position = 'none') +
  scale_y_reverse()

plot %>% 
  ggsave('009-02.jpg', 
         . , 
         path = paste0(getwd(), '/plots'),
         width = 8,
         height = 9)





####### to be used
# Distribution of reviews
reviews %>%
  ggplot(aes(x = rating)) +
  geom_histogram(breaks = 0:10)


games %>%
  filter(Ratings_std_dev > 0.75,
         Ratings_std_dev < 2.5) %>%
  ggplot(aes(x = Ratings_std_dev, y = Rank)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) + 
  scale_fill_gradient(high = orange, low = purple)


# Add something with ratings
# Use  geom_point(shape = ".")