library(dplyr)
library(tidyverse)
library(ggplot2)

player <- read_csv("data/processed/All_Players_Infor.csv")

#1- Create new variables based on existing variables in each game,and remove some unnecessary variables. 

player <- player%>%
  mutate(PTS_Per_G = PTS / G,
         TRB_Per_G = TRB / G,
         AST_Per_G = AST / G,
         STL_Per_G = STL / G,
         BLK_Per_G = BLK / G,
         TOV_Per_G = TOV / G,) %>%
  mutate_if(is.numeric, round, digits = 3) %>%
  select(-c(ORB:PTS)) %>%
  select(-c(FG,FGA,x3P,x3PA,x2P,x2PA,FT,FTA))

# Observe the relationship between Effective Field Goal Percentage and Points Per Game

player%>% 
  ggplot(aes(eFGp, PTS_Per_G)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player$eFGp, player$PTS_Per_G, method = "pearson")
#The Pearson coefficient is 0.285, 
##so there is a weak positive correlation between Effective Field Goal Percentage and Points Per Game.


player1 %>% group_by(Pos) %>%
  summarise(mn = mean(x2Pp, na.rm = TRUE), 
            sd = sd(x2Pp, na.rm = TRUE), 
            count = n()) 

