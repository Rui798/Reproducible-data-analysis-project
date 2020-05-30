library(tidyverse)
library(broom)
library(car)
library(ggplot2)

team <- read_csv( "data/processed/3_All_Teams_Infor_Per_G.csv" )

# 1-Bulid a Multiple Linear Model

#Previously in the exploratory team data,
##these variables (except for turnover) all show a positive correlation with winning, 
##and turnover has a negative correlation with winning.
##Therefore, I use these variables as explanatory variables(x) to build a multiple linear model 
##to observe their impact on winning(y).

fit_t <- lm( W ~  eFGp + PTS_Per_G + STL_Per_G + AST_Per_G + TRB_Per_G + BLK_Per_G +TOV_Per_G , data = team)
tidy(fit_t, conf.int = TRUE)

#By the tidyverse output of this model, it can be seen that the results of the model do not make sense.
##So, it should be considered whether the choice of independent variables is unreasonable, and look for another reasonable variable.

team %>% 
  ggplot(aes(eFGp, PTS_Per_G)) +
  geom_point() +
  geom_smooth(method = "lm") 

cor(team$eFGp, team$PTS_Per_G, method = "pearson")

#Effective Field Goal Percentage is positively correlated with points per game, and the correlation is high (0.777).
##Therefore, try to replace the effective field goal percentage with the points per game, 
##and then check whether the model is reasonable.


# 2- Changed Model

fit_t <- lm( W ~  PTS_Per_G + STL_Per_G + AST_Per_G + TRB_Per_G + BLK_Per_G +TOV_Per_G , data = team)
tidy(fit_t, conf.int = TRUE)

#this model seens more reasonable.

# 3-Independence of Observations

car::durbinWatsonTest(fit_t)

#The Durbin-Watson statistic of the model is 1.13(less than 2),
##which indicates that there is no correlation between residuals.

# 4- Linearity
car::avPlots(fit_t)

# The plot is exported and saved in "figs/3_Modelling_Linearity"
##There is a linear relationship between the response variable (Wins) and each explanatory variable.
###Except turnover, other variables show the positive linear relationship.

# 5- Detecting Outliers

std_res <- rstandard(fit_t) 
points <- 1:length(std_res)

ggplot(data = NULL, aes(x = points, y = std_res)) + 
  geom_point() + 
  ylim(c(-4,4)) + 
  geom_hline(yintercept = c(-3, 3), colour = "red", linetype = "dashed")

#There is no significant outliers.

# 6- Leverage points

hats <- hatvalues(fit_t)

hat_labels <- if_else(hats >= 0.4, paste(points), "") 
ggplot(data = NULL, aes(x = points, y = hats)) + 
  geom_point() + 
  geom_text(aes(label = hat_labels), nudge_y = 0.05)

#There is no value higher than 1, and most of them are within 0.4 
##There are only 3 points higher than 0.4 which are 5,9(highest),and 30.

# 7-Influential Points [Cook's distance]
cook <- cooks.distance(fit_t) 
ggplot(data = NULL, aes(x = points, y = cook)) + geom_point()

cook_labels <- if_else(cook >= 0.2, paste(points), "")
ggplot(data = NULL, aes(x = points, y = cook)) + 
  geom_point() + 
  geom_text(aes(label = cook_labels))

# 7-2 Point No.5 showed both in Leverage point and Influential Point, so I need to check it.

ggplot(team, aes(x = PTS_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
# P5 - High Leverage, Low Influential

ggplot(team, aes(x = STL_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
# P5 - Low Leverage, Low Influential

ggplot(team, aes(x = AST_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
# P5 - Low Leverage, Low Influential

ggplot(team, aes(x = TRB_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
# P5 - Low Leverage, Low Influential

ggplot(team, aes(x = BLK_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
## P5 - Low Leverage, Low Influential

ggplot(team, aes(x = TOV_Per_G, y = W))+
  geom_point()+
  geom_smooth(method = "lm", colour = "red")+
  geom_text(aes(label = cook_labels), nudge_y = 0.5)
# P5 - Low Leverage, Low Influential

##Therefore, P.5 do not need to be remove.

# 8- Homoscedasticity

res <- residuals(fit_t)
fitted <- predict(fit_t)

ggplot(data = NULL, aes(x = fitted, y = res)) + 
  geom_point(colour = "dodgerblue") + 
  geom_smooth(se = FALSE, colour = "orange")

#The data shows homoscedasticity since the residuals are randomly distributed.

# 9- Normality of Residuals

ggplot(data = NULL, aes(sample = res)) + 
  stat_qq() + 
  stat_qq_line()
#The residuals are normally distributed.

# 10- Multicollinearity

pairs(formula = ~PTS_Per_G + STL_Per_G + AST_Per_G + TRB_Per_G + BLK_Per_G +TOV_Per_G, data = team)
sqrt(car::vif(fit_t))

# From the plot and VIF("figs/3_Modelling_Multicollinearity") we can see 
##that there is not multicollinearity between the explanatory variables.


# 11- Model Testing

team <- team %>%
  mutate(exp_t_w = predict(fit_t))

ggplot(team, aes(exp_t_w, W, label = Team)) + 
  geom_point(colour = "dodgerblue") + 
  geom_text(nudge_x = 2, nudge_y = 1.5, cex = 2.5) + 
  geom_abline( colour = "red")+
  xlab("Expect Wins")+
  ylab("Wins")

# Apply Model [Player]

player1 <- read_csv("data/processed/3_Player_Candidate.csv") 

# 12- Adjust players' exploratory variables
player1$PTS_Per_G = player1$PTS_Per_G*10
player1$STL_Per_G = player1$STL_Per_G*10
player1$AST_Per_G = player1$AST_Per_G*10
player1$TRB_Per_G = player1$TRB_Per_G*10
player1$BLK_Per_G = player1$BLK_Per_G*10
player1$TOV_Per_G = player1$TOV_Per_G*10

player1$salary = player1$salary/1000000

# Since all exploratory variables in the team are the total number of teams, 
##we need to adjust the exploratory variables of players.
##Generally, a team will have 10 players participating in a game

# 13- Merge exp_t_w with players data
player1 <- player1 %>%
  mutate(exp_t_w = predict(fit_t, newdata = player1),
         exp_t_wA = exp_t_w * (60.8/370.2)) #Adjust players' expected wins index according to the teams' index.

player2<- player1 %>%
  select(player_name, Pos, salary, exp_t_wA) %>% 
  arrange(desc(exp_t_wA), salary) %>%
  filter(exp_t_wA >= 21)

write_csv(player2, "data/processed/4_Regression_Player2.csv")

#The lowest expect win of all teams is 21.05, 
##which filters out the players who below this standard.

# 14- Divide Players by Positions

list(player2$Pos)

C <- player2 %>%
  filter(Pos == "C")
PF <- player2 %>%
  filter(Pos == "PF") 
PG <- player2 %>%
  filter(Pos == "PG")
SF <- player2 %>%
  filter(Pos == "SF")
SG <- player2 %>%
  filter(Pos == "SG")

##since we need 5 staring players for each position. We need to do regress individually.

###Staring Player of Center

head(C)

C %>%
  ggplot(aes(x = salary, y = exp_t_wA, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_t_wA > 45,as.character(player_name),'')),
            nudge_x = 1,nudge_y = 2) +
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Wins")

#Staring Player of Power Forward
head(PF)

PF %>%
  ggplot(aes(x = salary, y = exp_t_wA, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_t_wA > 30,as.character(player_name),'')),
            nudge_x = 2,nudge_y = 2) +
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Wins")

#Staring Player of Small Forward
head(SF)

SF %>%
  ggplot(aes(x = salary, y = exp_t_wA, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_t_wA > 33,as.character(player_name),'')),
            nudge_x = -1) +
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Wins")

#Staring Player of Point Guard
head(PG)

PG %>%
  ggplot(aes(x = salary, y = exp_t_wA, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_t_wA > 30,as.character(player_name),'')),
            nudge_x = -3) +
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Wins")


#Staring Player of Shooting Guard
head(SG)

SG %>%
  ggplot(aes(x = salary, y = exp_t_wA, color = player_name)) +
  geom_point() + 
  geom_text(aes(label=ifelse(exp_t_wA > 22.3 ,as.character(player_name),'')),
            nudge_x = -2) +
  theme(legend.position = "none")+
  xlab("Salary (Millions)") +
  ylab("Expected Wins")

# Generate The Final Player List

Final_L <- player2%>%
  filter(player_name%in% c("KarlAnthony Towns",
                          "John Collins",
                          "Kawhi Leonard",
                          "James Harden",
                          "Luka Doncic"))%>%
  arrange(desc(salary))

write_csv(Final_L,"data/processed/final_player_list.csv")









         