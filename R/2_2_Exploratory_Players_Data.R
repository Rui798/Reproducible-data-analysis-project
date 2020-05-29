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
  select(-c(ORB:PTS)) 

# 1-2 Filter players with low Minutes Played than average

player1 <- player%>%
  mutate(MP_z = (MP - mean(MP)) / sd(MP), 
         MP_category = if_else(condition = MP_z < 0,true = "L", false = "H"))%>%
  filter(MP_category == "H") 

##Since we want to choose elite athletes (staring players), 
##and higher Minutes Played is the way to indicate that the athletes have experiences and good performances. 
##Therefore,the first step is to filter the players with low Minutes Played than average (2018-19 season).


# 2 Observe the relationship and Pearson coefficient
##2-1 Effective Field Goal Percentage and Points Per Game

player1%>% 
  ggplot(aes(PTS_Per_G, eFGp)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$eFGp, player1$PTS_Per_G, method = "pearson")

#The Pearson coefficient is 0.045, 
##there is barely no relationship between Effective Field Goal Percentage and Points Per Game.

player1 %>% group_by(Pos) %>%
  summarise(mn = mean(eFGp, na.rm = TRUE), 
            sd = sd(eFGp, na.rm = TRUE), 
            count = n()) 
#Players have not much difference in Effective Field Goal Percentage, 
##and the centers' eFGp is higher than other positions.

# 2-2 The relationship between Points Per Game and Total Rebounds Per Game.

player1%>% 
  group_by(Pos)%>%
  ggplot(aes(TRB_Per_G,PTS_Per_G,color = Pos, shape = Pos)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$TRB_Per_G, player1$PTS_Per_G, method = "pearson")

player1 %>% group_by(Pos) %>%
  summarise(mn = mean(TRB_Per_G, na.rm = TRUE), 
            sd = sd(TRB_Per_G, na.rm = TRUE), 
            count = n()) 
#Overall, the more rebounds per game, the higher the Points Per Game .
#The Pearson coefficient is 0.448, indicating that the number of rebounds moderately impacts the points.
#Most of rebounds are handled by Centers and Power Forwards.

# 2-3 The relationship between Points Per Game and Assists Per Game.

player1%>% 
  group_by(Pos)%>%
  ggplot(aes( AST_Per_G,PTS_Per_G,color = Pos, shape = Pos)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$PTS_Per_G, player1$AST_Per_G, method = "pearson")

player1 %>% group_by(Pos) %>%
  summarise(mn = mean(AST_Per_G, na.rm = TRUE), 
            sd = sd(AST_Per_G, na.rm = TRUE), 
            count = n()) 
#Overall, the more Assists per game, the higher the Points Per Game .
#The Pearson coefficient is 0.561, indicating that the number of Assists has significant impact on points.
#Most assists are given by Point Guards.

# 2-4 The relationship between Points Per Game and Steals Per Game.

player1%>% 
  group_by(Pos)%>%
  ggplot(aes(STL_Per_G,PTS_Per_G,color = Pos, shape = Pos)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$STL_Per_G, player1$PTS_Per_G, method = "pearson")

player %>% group_by(Pos) %>%
  summarise(mn = mean(STL_Per_G, na.rm = TRUE), 
            sd = sd(STL_Per_G, na.rm = TRUE), 
            count = n()) 
#Overall, the more Steals per game, the higher the Points Per Game .
#The Pearson coefficient is 0.457, indicating that the number of Steals moderately impacts the points.
#Most steals come from PG. 
#From the standard deviation it can be seen that the difference in steal levels between players is large.

# 2-5 The relationship between Points Per Game and Blocks Per Game.

player1%>% 
  group_by(Pos)%>%
  ggplot(aes( PTS_Per_G, BLK_Per_G,color = Pos, shape = Pos)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$PTS_Per_G, player1$BLK_Per_G, method = "pearson")

player1 %>% group_by(Pos) %>%
  summarise(mn = mean(BLK_Per_G, na.rm = TRUE), 
            sd = sd(BLK_Per_G, na.rm = TRUE), 
            count = n()) 
#Overall, the more Blocks per game, the higher the Points Per Game .
#The Pearson coefficient is 0.193, indicating that the number of Blocks weakly impacts the points.
#Obviously, most Blocks come from centers. 

# 2-6 The relationship between Points Per Game and Free Throw Percentage.

player1%>% 
  group_by(Pos)%>%
  ggplot(aes( PTS_Per_G,FTp,color = Pos, shape = Pos)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(player1$PTS_Per_G, player1$FTp, method = "pearson")

player1 %>% group_by(Pos) %>%
  summarise(mn = mean(FTp, na.rm = TRUE), 
            sd = sd(FTp, na.rm = TRUE), 
            count = n()) 
#Overall, the more Free Throw Percentage, the higher the Points Per Game .
#The Pearson coefficient is 0.272, indicating that the Free Throw Percentage has weak impact on points.
#The players' free throws are close to each other, their are all around 75%.   

#3 Export new tables

write_csv(x = player1, path = "data/processed/3_Player_Candidate.csv") 
#To facilitate modeling and regression


