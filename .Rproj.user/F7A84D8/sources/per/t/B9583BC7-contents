install.packages("ggplot2", "tidyvers")
library(tidyverse,ggplot2)

# read and save all data in R
p_st <- read_csv("data/raw/2018-19_nba_player_statistics.csv")
p_sal <- read_csv("data/raw/2018-19_nba_player-salaries.csv")
t_st1 <- read_csv("data/raw/2018-19_nba_team_statistics_1.csv")
t_st2 <- read_csv("data/raw/2018-19_nba_team_statistics_2.csv")
payroll <-read_csv("data/raw/2019-20_nba_team-payroll.csv")

#rename all variables which contain %
p_st <- rename(p_st, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, 
               x2PA = `2PA`, x2Pp = `2P%`, eFGp = `eFG%`, FTp = `FT%`)
t_st1 <- rename(t_st1, x3PAr = "3PAr", TSp= "TS%" , eFGp = "eFG%", TOVp = "TOV%",
                ORBp = "ORB%", FT_FGA = "FT/FGA", DRBp = "DRB%" )
t_st2 <- rename(t_st2, FGp = `FG%`, x3P = `3P`, x3PA = `3PA`, x3Pp = `3P%`, x2P = `2P`, 
                x2PA = `2PA`, x2Pp = `2P%`, FTp = `FT%`)

#delet missing value in player salaries(p_sal)
which(is.na(p_sal), arr.ind = TRUE)
p_sal<- select(p_sal, player_id, player_name, salary)
sum(is.na(p_sal))
write_csv(p_sal,"data/processed/1_2018-19_nba_player_salaries_no_na.csv")

# delet missing value in team statistics 1(t_st1)
which(is.na(t_st1), arr.ind = TRUE)
t_st1<- select(t_st1, -c(X23:25))
sum(is.na(t_st1))
write_csv(t_st1,"data/processed/1_2018-19_nba_team_statistics_1_no_na.csv")

#deal missing values in player statistics (p_st)
sum(is.na(p_st))
which(is.na(p_st), arr.ind = TRUE)
naniar::vis_miss(p_st)
p_st <- replace_na(p_st,list(FGp = "0", x3Pp = "0", x2Pp = "0", eFGp = "0", FTp = "0" ))
sum(is.na(p_st))
write_csv(t_st1,"data/processed/1_2018-19_nba_player_statistics.csv")

# find the duplications in player statistics (p_st)
p_st_d <- p_st %>% 
  group_by(player_name, Age) %>%  
  mutate(index = n()) %>%
  filter(index > 1) %>%
  ungroup()


