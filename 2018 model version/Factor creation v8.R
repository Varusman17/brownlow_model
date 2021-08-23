##############################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
# Update: v5 changes
#         - BR polling history
##############################################################################

# install.packages(c("devtools","reshape2","tidyr","eeptools"))

rm(list = ls())
gc()

library(fitzRoy)
library(reshape2)
library(tidyr)
library(dplyr)
library(rvest)
library(sqldf)
library(eeptools)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\"

# Load data

all_data <- readRDS(file=paste0(model_dir,"all_data_restrict_v3.RDS"))
# View(all_data)

br_raw <- read.csv(file=paste0(data_dir,"brownlow_final.csv"), header=TRUE, sep=",")
# head(br_raw)

br_summ <- br_raw[,c("Season","Player","Team","Games_played","Games_polled","Total_3_vote_games","Total_2_vote_games","Total_1_vote_games","Total_Votes")]
# View(br_summ)

# Rank the brownlow votes information

br_summ <- br_summ %>% arrange(Season, desc(Total_Votes)) %>% group_by(Season) %>% mutate(Season_BR_rank = row_number(desc(Total_Votes)))
br_summ <- br_summ %>% arrange(Season, Team, desc(Total_Votes)) %>% group_by(Season, Team) %>% mutate(Team_BR_rank = row_number(desc(Total_Votes)))
br_summ$Season_more_than_5_BR <- ifelse(br_summ$Total_Votes>4, 1, 0)
br_summ$Season_more_than_10_BR <- ifelse(br_summ$Total_Votes>9, 1, 0)
br_summ$Season_more_than_15_BR <- ifelse(br_summ$Total_Votes>14, 1, 0)
br_summ$Season_more_than_20_BR <- ifelse(br_summ$Total_Votes>19, 1, 0)

View(br_summ)

all_data_ess <- 
  sqldf('SELECT A.*
        , B.Team_BR_rank AS Prev_season_team_BR_rank
        , B.Season_BR_rank AS Prev_season_BR_rank
        , B.Season_more_than_5_BR AS Prev_season_more_than_5_BR
        , B.Season_more_than_10_BR AS Prev_season_more_than_10_BR
        , B.Season_more_than_15_BR AS Prev_season_more_than_15_BR
        , B.Season_more_than_20_BR AS Prev_season_more_than_20_BR
        FROM all_data A
        LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.Season = B.Season + 2
        WHERE A.Season IN ("2017") AND A.Player IN ("Dyson Heppell","Cale Hooker","Michael Hurley","Tom Bellchambers")
        ')

# View(all_data_ess)

all_data_br <- 
sqldf('SELECT A.*
              , B.Team_BR_rank AS Prev_season_team_BR_rank
              , B.Season_BR_rank AS Prev_season_BR_rank
              , B.Season_more_than_5_BR AS Prev_season_more_than_5_BR
              , B.Season_more_than_10_BR AS Prev_season_more_than_10_BR
              , B.Season_more_than_15_BR AS Prev_season_more_than_15_BR
              , B.Season_more_than_20_BR AS Prev_season_more_than_20_BR
      FROM all_data A
      LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.Season = B.Season + 1
      ')

all_data_br <- all_data_br[!(all_data_br$Season=="2017" & all_data_br$Player=="Dyson Heppell"),]
all_data_br <- all_data_br[!(all_data_br$Season=="2017" & all_data_br$Player=="Cale Hooker"),]
all_data_br <- all_data_br[!(all_data_br$Season=="2017" & all_data_br$Player=="Michael Hurley"),]
all_data_br <- all_data_br[!(all_data_br$Season=="2017" & all_data_br$Player=="Tom Bellchambers"),]

all_data <- rbind(all_data_br,all_data_ess)
View(all_data)

all_data <-
  sqldf('SELECT A.*
        , B.Games_played
        , B.Games_polled
        , B.Total_Votes
        FROM all_data A
        LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.Season = B.Season
        ')
  
all_data$Prev_season_top_5_BR_rank <- ifelse(all_data$Prev_season_BR_rank <6, 1, 0)
all_data$Prev_season_top_10_BR_rank <- ifelse(all_data$Prev_season_BR_rank <11, 1, 0)
all_data$Prev_season_top_20_BR_rank <- ifelse(all_data$Prev_season_BR_rank <21, 1, 0)
all_data$Prev_season_top_30_BR_rank <- ifelse(all_data$Prev_season_BR_rank <31, 1, 0)
all_data$Prev_season_top_50_BR_rank <- ifelse(all_data$Prev_season_BR_rank <51, 1, 0)
# Calculate player age

all_data$Player_age <- floor(age_calc(all_data$DOB,all_data$Date, units = "years"))
View(all_data)
# Calculate AA and top 50 ratings for previous season as well

all_australian <- read.csv(file=paste0(data_dir,"all_australian_team.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
afl_top_50 <- read.csv(file=paste0(data_dir,"afl_players_top_50.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)

all_data <-
  sqldf('SELECT A.*
              , B.AA_team as Prev_season_AA_team
              , B.AA_squad AS Prev_season_AA_squad

         FROM all_data A
         LEFT JOIN all_australian B
         ON A.Player = B.Player
         AND A.Season = B.Season + 1
        
        ')
all_data$Prev_season_AA_squad <- ifelse(is.na(all_data$Prev_season_AA_squad),0,all_data$Prev_season_AA_squad)
all_data$Prev_season_AA_team <- ifelse(is.na(all_data$Prev_season_AA_team),0,all_data$Prev_season_AA_team)

all_data <-
  sqldf('SELECT A.*
        , B.AFL_player_top_50_flag as Prev_season_AFL_player_top_50_flag
        , B.AFL_player_top_50_rank as Prev_season_AFL_player_top_50_rank
        FROM all_data A
        LEFT JOIN afl_top_50 B
        ON A.Player = B.Player
        AND A.Season = B.Season + 1
        
        ')
all_data$Prev_season_AFL_player_top_50_flag <- ifelse(is.na(all_data$Prev_season_AFL_player_top_50_flag&all_data$Season>2014),0,all_data$Prev_season_AFL_player_top_50_flag)

# Rank the factors on a team / match / season level

# SC scores
all_data <- all_data %>% arrange(Match_id, desc(SC)) %>% group_by(Match_id) %>% mutate(Match_SC_rank = row_number(desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_SC_rank = row_number(desc(SC)))
all_data <- all_data %>% arrange(Season, desc(SC), desc(AF)) %>% group_by(Season) %>% mutate(Season_SC_rank = order(desc(SC), desc(AF)))

#AF scores
all_data <- all_data %>% arrange(Match_id, desc(AF)) %>% group_by(Match_id) %>% mutate(Match_AF_rank = row_number(desc(AF)))
all_data <- all_data %>% arrange(Match_id, Team, desc(AF)) %>% group_by(Match_id,Team) %>% mutate(Team_AF_rank = row_number(desc(AF)))
all_data <- all_data %>% arrange(Season, desc(SC), desc(AF)) %>% group_by(Season) %>% mutate(Season_AF_rank = order(desc(AF), desc(SC)))

# Contested possessions
all_data <- all_data %>% arrange(Match_id, desc(Contested_possessions),desc(SC)) %>% group_by(Match_id) %>% mutate(Match_CP_rank = order(desc(Contested_possessions), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Contested_possessions),desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_CP_rank = order(desc(Contested_possessions), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Contested_possessions), desc(SC)) %>% group_by(Season) %>% mutate(Season_CP_rank = order(desc(Contested_possessions), desc(SC)))

# Uncontested possessions
all_data <- all_data %>% arrange(Match_id, desc(Uncontested_possessions),desc(SC)) %>% group_by(Match_id) %>% mutate(Match_UP_rank = order(desc(Uncontested_possessions), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Uncontested_possessions),desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_UP_rank = order(desc(Uncontested_possessions), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Uncontested_possessions), desc(SC)) %>% group_by(Season) %>% mutate(Season_UP_rank = order(desc(Uncontested_possessions), desc(SC)))

# Disposals
all_data <- all_data %>% arrange(Match_id, desc(Disposals), desc(SC)) %>% group_by(Match_id) %>% mutate(Match_D_rank = order(desc(Disposals), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Disposals), desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_D_rank = order(desc(Disposals), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Disposals), desc(SC)) %>% group_by(Season) %>% mutate(Season_D_rank = order(desc(Disposals), desc(SC)))

# Effective disposals
all_data <- all_data %>% arrange(Match_id, desc(Effective_disposals), desc(SC)) %>% group_by(Match_id) %>% mutate(Match_ED_rank = order(desc(Effective_disposals), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Effective_disposals), desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_ED_rank = order(desc(Effective_disposals), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Effective_disposals), desc(SC)) %>% group_by(Season) %>% mutate(Season_ED_rank = order(desc(Effective_disposals), desc(SC)))

# Kicks
all_data <- all_data %>% arrange(Match_id, desc(Kicks), desc(SC)) %>% group_by(Match_id) %>% mutate(Match_K_rank = order(desc(Kicks), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Kicks), desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_K_rank = order(desc(Kicks), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Kicks), desc(SC)) %>% group_by(Season) %>% mutate(Season_K_rank = order(desc(Kicks), desc(SC)))

# Handballs
all_data <- all_data %>% arrange(Match_id, desc(Handballs), desc(SC)) %>% group_by(Match_id) %>% mutate(Match_H_rank = order(desc(Handballs), desc(SC)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Handballs), desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_H_rank = order(desc(Handballs), desc(SC)))
all_data <- all_data %>% arrange(Season, desc(Handballs), desc(SC)) %>% group_by(Season) %>% mutate(Season_H_rank = order(desc(Handballs), desc(SC)))

# Marks
all_data <- all_data %>% arrange(Match_id, desc(Marks)) %>% group_by(Match_id) %>% mutate(Match_M_rank = dense_rank(desc(Marks)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Marks)) %>% group_by(Match_id,Team) %>% mutate(Team_M_rank = dense_rank(desc(Marks)))
all_data <- all_data %>% arrange(Season, desc(Marks), desc(SC)) %>% group_by(Season) %>% mutate(Season_M_rank = order(desc(Marks), desc(SC)))

# Goals
all_data <- all_data %>% arrange(Match_id, desc(Goals), desc(SC)) %>% group_by(Match_id) %>% mutate(Match_G_rank = dense_rank(desc(Goals)))
all_data <- all_data %>% arrange(Match_id, Team, desc(Goals), desc(SC)) %>% group_by(Match_id,Team) %>% mutate(Team_G_rank = dense_rank(desc(Goals)))
all_data <- all_data %>% arrange(Season, desc(Goals), desc(SC)) %>% group_by(Season) %>% mutate(Season_G_rank = order(desc(Goals), desc(SC)))

# Proportion of goals kicked for team
all_data$Goals <- as.integer(all_data$Goals)
all_data$Team_goals <- as.integer(all_data$Team_goals)
all_data <- all_data %>% mutate(Prop_goals = Goals / Team_goals)
all_data <- filter(all_data, Season > 2011)

# Assign a player id and then a record id for tracking purposes
player_id <- data.frame(unique(all_data$Player))
names(player_id)[1] <- c("Player")
player_id <- player_id %>% arrange(Player) %>% mutate(Player_id = order(Player))
all_data <- inner_join(x=all_data,y=player_id,by=c("Player"))

# View(all_data)

# Add more player level features
all_data <- all_data %>% group_by(Player_id) %>% mutate(First_season = min(Season))
all_data$Num_seasons <- all_data$Season - all_data$First_season + 1

# Compute the avg number of times a player has been selected to AA squad / team

player <- all_data %>% distinct(Season,Player,AA_squad,AA_team,AFL_player_top_50_flag, Num_seasons,Prev_season_more_than_5_BR, Prev_season_more_than_10_BR, Prev_season_more_than_15_BR, Prev_season_more_than_20_BR, Games_played, Games_polled, Total_Votes)
player$AFL_player_top_50_flag <- ifelse(is.na(player$AFL_player_top_50_flag),0,player$AFL_player_top_50_flag)
player$Games_played <- ifelse(is.na(player$Games_played),0,player$Games_played)
player$Games_polled <- ifelse(is.na(player$Games_polled),0,player$Games_polled)
player$Total_Votes <- ifelse(is.na(player$Total_Votes),0,player$Total_Votes)
player <- player %>% arrange(Player,Season) %>% mutate(Num_AA_team = cumsum(AA_team))
player <- player %>% arrange(Player,Season) %>% mutate(Num_AA_squad = cumsum(AA_squad))
player <- player %>% arrange(Player,Season) %>% mutate(Num_AFL_top_50 = cumsum(AFL_player_top_50_flag))
player$Avg_AA_team <- player$Num_AA_team / player$Num_seasons
player$Avg_AA_squad <- player$Num_AA_squad / player$Num_seasons

# Compute a player's previous brownlow polling history

player$Prev_season_more_than_5_BR <- ifelse(is.na(player$Prev_season_more_than_5_BR),0,player$Prev_season_more_than_5_BR)
player$Prev_season_more_than_10_BR <- ifelse(is.na(player$Prev_season_more_than_10_BR),0,player$Prev_season_more_than_10_BR)
player$Prev_season_more_than_15_BR <- ifelse(is.na(player$Prev_season_more_than_15_BR),0,player$Prev_season_more_than_15_BR)
player$Prev_season_more_than_20_BR <- ifelse(is.na(player$Prev_season_more_than_20_BR),0,player$Prev_season_more_than_20_BR)
player <- player %>% arrange(Player,Season) %>% mutate(Num_season_more_than_5_BR = cumsum(Prev_season_more_than_5_BR))
player <- player %>% arrange(Player,Season) %>% mutate(Num_season_more_than_10_BR = cumsum(Prev_season_more_than_10_BR))
player <- player %>% arrange(Player,Season) %>% mutate(Num_season_more_than_15_BR = cumsum(Prev_season_more_than_15_BR))
player <- player %>% arrange(Player,Season) %>% mutate(Num_season_more_than_20_BR = cumsum(Prev_season_more_than_20_BR))
player <- player %>% arrange(Player,Season) %>% mutate(Total_Votes_since_2012 = cumsum(Total_Votes))
player$Avg_games_polled <- player$Games_polled / player$Games_played

# Compute the avg number of times a player has been selected as a Top 50 player

player_adjusted <- filter(player,Season > 2013)
player_adjusted <- player_adjusted %>% group_by(Player_id) %>% mutate(First_season = min(Season))
player_adjusted$Num_season_since_2014 <- player_adjusted$Season - player_adjusted$First_season + 1
player_adjusted$Avg_AFL_top_50 <- player_adjusted$Num_AFL_top_50 / player_adjusted$Num_season_since_2014
player_adjusted <- within(player_adjusted,rm(First_season))
library(sqldf)
player_final <-
  sqldf('SELECT A.*
        , B.Num_season_since_2014
        , B.Avg_AFL_top_50
        
        FROM player A
        LEFT JOIN player_adjusted B
        ON A.Player = B.Player
        AND A.Season = B.Season
        
        ')
player_final$AFL_player_top_50_flag <- ifelse(player_final$Season<2014,NA,player_final$AFL_player_top_50_flag)
player_final$Num_AFL_top_50 <- ifelse(player_final$Season<2014,NA,player_final$Num_AFL_top_50)
player_final <-
  sqldf('SELECT A.*
        , B.Avg_games_polled as Prev_season_avg_games_polled
        , B.Total_Votes_since_2012 as Prev_season_total_votes_since_2012
        , B.Total_Votes_since_2012 / A.Num_seasons AS Prev_season_avg_votes_per_seasons_since_2012
        , B.Total_Votes as Prev_season_total_BR_votes
        FROM player_final A
        LEFT JOIN player_final B
        ON A.Player = B.Player
        AND A.Season = B.Season + 1
        
        ')
player_final$Avg_games_polled <- ifelse(is.na(player_final$Avg_games_polled),0,player_final$Avg_games_polled)
player_final$Prev_season_avg_games_polled <- ifelse(is.na(player_final$Prev_season_avg_games_polled),0,player_final$Prev_season_avg_games_polled)
check <- filter(player_final,Player == "Tom Mitchell")
View(check)

# Join on player level features to the data set
all_data <- within(all_data, rm("Prev_season_more_than_5_BR"))
all_data <- within(all_data, rm("Prev_season_more_than_10_BR"))
all_data <- within(all_data, rm("Prev_season_more_than_15_BR"))
all_data <- within(all_data, rm("Prev_season_more_than_20_BR"))
all_data <- within(all_data, rm("Games_played"))
all_data <- within(all_data, rm("Games_polled"))
all_data <- within(all_data, rm("Total_Votes"))

all_data <- left_join(x=all_data,y=player_final,by=c("Player","Player_id","AA_team","AA_squad","AFL_player_top_50_flag","Num_seasons","Season"))
View(all_data)
# Add in a record_id column
all_data <- within(all_data, Record_id <- paste(Season, Round,Match_id, Player,Player_id,sep = "_"))

columns <- data.frame(colnames(all_data))
write.csv(columns,file=paste0(model_dir,"column_names_v8.csv"))
saveRDS(all_data, file=paste0(model_dir,"final_data_v8.RDS")) 

# Perform summary statistics on votegetters

target <- c("1", "2","3")
votegetters <- filter(all_data, Num_Votes %in% target)
three_vote <- filter(all_data, Num_Votes == "3")
unique(votegetters$Num_Votes)

# Have players who get less than 10 disposals ever polled votes?

votegetters %>% group_by(D)%>%summarize(n())
hist(votegetters$D)
hist(three_vote$D)
hist(three_vote$Margin)
hist(three_vote$SC_game_rank)
d<-density(three_vote$SC_game_rank)
plot(d)

barplot(prop.table(table(three_vote$Outcome)))
hist(votegetters$SC)
hist(three_vote$SC)

# Over the last two season how often has a player kicked 5 goals and polled?

five_goal_games <- all_data %>% filter(G >= 5)
View(five_goal_games)
library(plyr)
summarize <- count(five_goal_games,c("Season","G","Outcome","Num_Votes"))
write.csv(summarize,file=paste0(data_dir,"five_goal_games.csv"))

View(all_data%>%filter(Match_id == "5461"))

# How often does a player get more than 40 touches and poll?

all_data %>% filter(D > 39)
weird_matches <- all_data %>% filter(D > 39 & Votes == "0") %>% select(Match_id)
weird <- inner_join(all_data, weird_matches, by = "Match_id")
View(weird)
all_data %>% filter(Season == "2013" & Player == "Gary Jnr Ablett")
View(all_data %>% filter(Season == "2013" & Player == "Gary Jnr Ablett"))
View(all_data %>% filter(Match_id == "5628"))

View(all_data%>%filter(Match_id == "5343"))