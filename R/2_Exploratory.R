library(dplyr)
library(tidyverse)
library(ggplot2)
player <- read_csv("data/processed/All_Players_Infor.csv")
team <- read_csv("data/processed/All_Teams_Infor.csv")

# Exploratory [team] data
#The purpose of these steps is to find the important factors(variables) which influence the game outcomes.

#1- Create new variables based on existing variables in each game,
#and remove some unnecessary variables. 
team <- team%>%
  mutate(PTS_Per_G = PTS / G,
         TRB_Per_G = TRB / G,
         AST_Per_G = AST / G,
         STL_Per_G = STL / G,
         BLK_Per_G = BLK / G,
         TOV_Per_G = TOV / G,) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(ORB:PTS,G)) %>%
  select(-c(FG,FGA,x3P,x3PA,x2P,x2PA,FT,FTA))

#2-Find the relationships between wins and some factors.

# 2-1 The relationship between wins and 3 points shooting.
team %>% 
  ggplot(aes(x3Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$x3Pp, team$W, method = "pearson")

#The team with the higher three-point percentage is more likely to win.
#Since the Pearson coefficient is 0.542, 
#the three-point shooting rate has a moderate impact on the wins.

# 2-2 The relationship between wins and 2 points shooting.

team %>% 
  ggplot(aes(ORtg, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$ORtg, team$W, method = "pearson")


team %>% 
  ggplot(aes(DRtg, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$DRtg, team$W, method = "pearson") 









