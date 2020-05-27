library(dplyr)
library(tidyverse)
library(ggplot2)

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

#The teams with the higher three-point percentage is more likely to win.
#Since the Pearson coefficient is 0.542, the three-point shooting rate has moderate impact on wins.

# 2-2 The relationship between wins and Field Goal Percentage.

team %>% 
  ggplot(aes(FGp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$FGp, team$W, method = "pearson")

#The teams with the higher Field Goal Percentage is more likely to win.
#Since the Pearson coefficient is 0.61, the Field Goal Percentage has moderate impact on wins.

# 2-3 The relationship between wins and 2-Point Field Goal Percentage.

team %>% 
  ggplot(aes(x2Pp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$x2Pp, team$W, method = "pearson")

#The teams with the higher 2-Point Field Goal Percentage is more likely to win.
#Since the Pearson coefficient is 0.60, the 2-Point Field Goal Percentage has higher impact on wins than 3-points shooting.

# 2-4 The relationship between wins and Effective Field Goal Percentage.

team %>% 
  ggplot(aes(eFGp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$eFGp, team$W, method = "pearson")

#The teams with the higher Effective Field Goal Percentage is more likely to win.
#Since the Pearson coefficient is 0.78, the Effective Field Goal Percentage has strong impact on wins.

# 2-5 The relationship between wins and True Shooting Percentage.
team %>% 
  ggplot(aes(TSp, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$TSp, team$W, method = "pearson")

#The teams with the higher Effective True Shooting Percentage is more likely to win.
#Since the Pearson coefficient is 0.76, the True Shooting Percentage has strong impact on wins.

##From the above analysis,it can be seen that the shooting ability will greatly affect the game outcomes. 
##The higher the shooting percentage, the greater the possibility of wining. 
##Since the Pearson coefficient of "Effective Field Goal Percentage" has the most strongly related to the game result,
##this item will be an important reference when selecting star players.

# 2-6 The relationship between wins and Turnover Percentage Per Game.
team %>% 
  ggplot(aes(TOV_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$TOV_Per_G, team$W, method = "pearson")
mean(team$TOV_Per_G)
sd(team$TOV_Per_G)

#The Turnover Percentage Per Game has weak impact on wins.
#But overall, the fewer turnover, the greater the chance of winning. 
##In addition, most teams have about 14 turnovers per game, with a standard deviation of 1.03, 
##which indicate the number of turnovers of each team are similar. 
##Therefore, players who have made too many mistakes should be ruled out when selecting players. 

# 2-7 The relationship between wins and Total Rebounds Per Game.
team %>% 
  ggplot(aes(TRB_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$TRB_Per_G, team$W, method = "pearson")
#The teams with the more Total Rebounds Per Game is more likely to win.
#Since the Pearson coefficient is 0.529, the Total Rebounds has moderate impact on wins.

# 2-8 The relationship between wins and Assists Per Game.
team %>% 
  ggplot(aes(AST_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$AST_Per_G, team$W, method = "pearson")
#The teams with the more Assists Per Game is more likely to win.
#Since the Pearson coefficient is 0.502, the Assists has moderate impact on wins.

# 2-9 The relationship between wins and Steals Per Game.
team %>% 
  ggplot(aes(STL_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$STL_Per_G, team$W, method = "pearson")
##The Turnover has weak impact on wins.

# 2-10 The relationship between wins and Blocks Per Game.
team %>% 
  ggplot(aes(BLK_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$BLK_Per_G, team$W, method = "pearson")
###The teams with the more Blocks Per Game is more likely to win.
#Since the Pearson coefficient is 0.444, the Blocks has moderate impact on wins.

# 2-11 The relationship between wins and Points.
team %>% 
  ggplot(aes(PTS_Per_G, W)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$PTS_Per_G, team$W, method = "pearson")
###The teams get more Points Per Game is more likely to win.
#Since the Pearson coefficient is 0.661, the Points has moderate impact on wins.





