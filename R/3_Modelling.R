library(tidyverse)
library(broom)
library(car)

team <- read_csv("data/processed/All_Teams_Infor.csv")
player1 <- read_csv("data/processed/Player_Candidate.csv") 



#Divide Players by Positions

list(player1$Pos)

C <- player1 %>%
  filter(Pos == "C")
PF <- player1 %>%
  filter(Pos == "PF") 
PG <- player1 %>%
  filter(Pos == "PG")
SF <- player1 %>%
  filter(Pos == "SF")
SG <- player1 %>%
  filter(Pos == "SG")

##since we need 5 staring players for each position. We need to do regress individually.

#

fit_player <- lm( PTS_Per_G ~ eFGp + STL_Per_G + AST_Per_G + TRB_Per_G + BLK_Per_G , data = player1)
tidy(fit_player, conf.int = TRUE)

#The unit of eFGp is unified with other variables to make the model more reasonable.

player1 <- player1%>%
  mutate(eFGp = eFGp*100)

#test the model again

fit_player <- lm( PTS_Per_G ~ eFGp + STL_Per_G + AST_Per_G + TRB_Per_G + BLK_Per_G , data = player1)
tidy(fit_player, conf.int = TRUE)

