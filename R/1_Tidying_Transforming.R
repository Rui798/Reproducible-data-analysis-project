library(tidyverse) 
library(dplyr)
library(ggplot2)
library(broom)


# Read and save all data in R
p_st <- read_csv("data/raw/2018-19_nba_player_statistics.csv")
p_sal <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
t_st1 <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv")
t_st2 <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv")
payroll <-read_csv("data/raw/2019-20_nba_team-payroll.csv")

#Check the Data
head(p_st)
tail(p_st)
str(p_st)

head(p_sal)
tail(p_sal)
str(p_sal)

head(t_st1)
tail(t_st1)
str(t_st1)

head(t_st2)
tail(t_st2)
str(t_st2)

head(payroll)
tail(payroll)
str(payroll)

# Data Transformation

#1- Rename all variables which contain symbols
p_st <- rename(p_st, FGp = 'FG%', x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', 
               x2PA = '2PA', x2Pp = '2P%', eFGp = 'eFG%', FTp = 'FT%')
t_st1 <- rename(t_st1, x3PAr = "3PAr", TSp= "TS%" , eFGp = "eFG%", TOVp = "TOV%",
                ORBp = "ORB%", FT_FGA = "FT/FGA", DRBp = "DRB%" )
t_st2 <- rename(t_st2, FGp ='FG%', x3P = '3P', x3PA = '3PA', x3Pp = '3P%', x2P = '2P', 
                x2PA = '2PA', x2Pp = '2P%', FTp = 'FT%')

#2- Replace and delete unrecognized symbols

# Change "salary"'s class (payroll)
payroll$salary <- str_remove_all(payroll$salary, "\\$")
payroll$salary <- str_remove_all(payroll$salary, "\\,")
payroll$salary <- str_remove_all(payroll$salary, "\\,")
payroll$salary <- as.numeric(payroll$salary)


# Adjust players' name
p_st$player_name <- p_st$player_name %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "")%>%
  stringr::str_replace_all(pattern = "\\-", replacement = "")%>%
  stringr::str_replace_all(pattern = "\\'", replacement = "")

p_sal$player_name <- p_sal$player_name %>%
  stringi::stri_trans_general("Latin-ASCII") %>%
  stringr::str_replace_all(pattern = "\\.", replacement = "")%>%
  stringr::str_replace_all(pattern = "\\-", replacement = "")%>%
  stringr::str_replace_all(pattern = "\\'", replacement = "")

# 3- Dealing missing value

sum(is.na(p_sal))
sum(is.na(p_st))
sum(is.na(payroll))
sum(is.na(t_st1))
sum(is.na(t_st2))

#Delet missing value in player salaries(p_sal)
which(is.na(p_sal), arr.ind = TRUE)
p_sal<- select(p_sal, player_id, player_name, salary)
sum(is.na(p_sal))

# Delet missing value in team statistics 1(t_st1)
which(is.na(t_st1), arr.ind = TRUE)
t_st1<- select(t_st1, -c(X23:25))
sum(is.na(t_st1))

#Deal missing values in player statistics (p_st)
sum(is.na(p_st))
which(is.na(p_st), arr.ind = TRUE)
naniar::vis_miss(p_st)
p_st <- replace_na(p_st,list(FGp = 0, x3Pp = 0, x2Pp = 0, eFGp = 0, FTp = 0 ))
sum(is.na(p_st))

#check missing values
sum(is.na(p_sal))
sum(is.na(p_st))
sum(is.na(payroll))
sum(is.na(t_st1))
sum(is.na(t_st2))

#4- Deal duplications in player statistics
# Find the players who played in mutliple teams in one season (p_st)
p_st <-p_st %>%
  distinct(player_name, .keep_all = TRUE)

# write new dataframes into csv.

write_csv(p_sal,"data/processed/1_2018-19_nba_player_salaries.csv")
write_csv(payroll,"data/processed/1_2019-20_nba_team-payroll.csv")
write_csv(p_st,"data/processed/1_2018-19_nba_player_statistics.csv")
write_csv(t_st1,"data/processed/1_2018-19_nba_team_statistics_1.csv")
write_csv(t_st2,"data/processed/1_2018-19_nba_team_statistics_2.csv")

#Combine players' data
p <- inner_join(x = p_st, y = p_sal, 
                by = "player_name") 
p <- select(p, -c(GS, player_id))

write_csv(p,"data/processed/All_Players_Infor.csv")

#Combine teams' data
t <- t %>%
  full_join(x = t_st1, y = t_st2,
               by = c("Team")) %>%
  select(-c(Rk.x,Rk.y,MP))
sum(is.na(t))
write_csv(t,"data/processed/All_Teams_Infor.csv")
