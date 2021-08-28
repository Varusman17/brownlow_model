##############################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
# Update: v5 changes
#         - BR polling history
##############################################################################

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\"

# Load data

all_data <- readRDS(file=paste0(model_dir,"all_data_restrict_v4.RDS"))
# View(all_data)

br_raw <- read.csv(file=paste0(data_dir,"brownlow_final.csv"), header=TRUE, sep=",")
# head(br_raw)

br_summ <- br_raw[,c("season","player_name","player_team","games_played","games_polled","three_vote_games","two_vote_games","one_vote_games","total_votes")]
# View(br_summ)

# Rank the brownlow votes information

br_summ <- br_summ %>% arrange(season, desc(total_votes)) %>% group_by(season) %>% mutate(season_BR_rank = row_number(desc(total_votes)))
br_summ <- br_summ %>% arrange(season, Team, desc(total_votes)) %>% group_by(season, Team) %>% mutate(team_BR_rank = row_number(desc(total_votes)))
br_summ$season_more_than_5_BR <- ifelse(br_summ$total_votes>4, 1, 0)
br_summ$season_more_than_10_BR <- ifelse(br_summ$total_votes>9, 1, 0)
br_summ$season_more_than_15_BR <- ifelse(br_summ$total_votes>14, 1, 0)
br_summ$season_more_than_20_BR <- ifelse(br_summ$total_votes>19, 1, 0)

View(br_summ)

all_data_ess <- 
  sqldf('SELECT A.*
        , B.team_BR_rank AS prev_season_team_BR_rank
        , B.season_BR_rank AS prev_season_BR_rank
        , B.season_more_than_5_BR AS prev_season_more_than_5_BR
        , B.season_more_than_10_BR AS prev_season_more_than_10_BR
        , B.season_more_than_15_BR AS prev_season_more_than_15_BR
        , B.season_more_than_20_BR AS prev_season_more_than_20_BR
        FROM all_data A
        LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.season = B.season + 2
        WHERE A.season IN ("2017") AND A.Player IN ("Dyson Heppell","Cale Hooker","Michael Hurley","Tom Bellchambers")
        ')

# View(all_data_ess)

all_data_br <- 
sqldf('SELECT A.*
              , B.team_BR_rank AS prev_season_team_BR_rank
              , B.season_BR_rank AS prev_season_BR_rank
              , B.season_more_than_5_BR AS prev_season_more_than_5_BR
              , B.season_more_than_10_BR AS prev_season_more_than_10_BR
              , B.season_more_than_15_BR AS prev_season_more_than_15_BR
              , B.season_more_than_20_BR AS prev_season_more_than_20_BR
      FROM all_data A
      LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.season = B.season + 1
      ')

all_data_br <- all_data_br[!(all_data_br$season=="2017" & all_data_br$Player=="Dyson Heppell"),]
all_data_br <- all_data_br[!(all_data_br$season=="2017" & all_data_br$Player=="Cale Hooker"),]
all_data_br <- all_data_br[!(all_data_br$season=="2017" & all_data_br$Player=="Michael Hurley"),]
all_data_br <- all_data_br[!(all_data_br$season=="2017" & all_data_br$Player=="Tom Bellchambers"),]

all_data <- rbind(all_data_br,all_data_ess)
View(all_data)

all_data <-
  sqldf('SELECT A.*
        , B.games_played
        , B.games_polled
        , B.total_votes
        FROM all_data A
        LEFT JOIN br_summ B
        ON A.Player = B.Player
        AND A.season = B.season
        ')
  
all_data$prev_season_top_5_BR_rank <- ifelse(all_data$prev_season_BR_rank <6, 1, 0)
all_data$prev_season_top_10_BR_rank <- ifelse(all_data$prev_season_BR_rank <11, 1, 0)
all_data$prev_season_top_20_BR_rank <- ifelse(all_data$prev_season_BR_rank <21, 1, 0)
all_data$prev_season_top_30_BR_rank <- ifelse(all_data$prev_season_BR_rank <31, 1, 0)
all_data$prev_season_top_50_BR_rank <- ifelse(all_data$prev_season_BR_rank <51, 1, 0)
# Calculate player age

all_data$Player_age <- floor(age_calc(all_data$DOB,all_data$Date, units = "years"))
View(all_data)
# Calculate AA for previous season as well

all_australian <- read.csv(file=paste0(data_dir,"all_australian_team.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
#afl_top_50 <- read.csv(file=paste0(data_dir,"afl_players_top_50.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)

all_data <-
  sqldf('SELECT A.*
              , B.AA_team as prev_season_AA_team
              , B.AA_squad AS prev_season_AA_squad

         FROM all_data A
         LEFT JOIN all_australian B
         ON A.player_name = B.player_name
         AND A.season = B.season + 1
        
        ')
all_data$prev_season_AA_squad <- ifelse(is.na(all_data$prev_season_AA_squad),0,all_data$prev_season_AA_squad)
all_data$prev_season_AA_team <- ifelse(is.na(all_data$prev_season_AA_team),0,all_data$prev_season_AA_team)

#all_data <-
#  sqldf('SELECT A.*
#        , B.AFL_player_top_50_flag as prev_season_AFL_player_top_50_flag
#        , B.AFL_player_top_50_rank as prev_season_AFL_player_top_50_rank
#        FROM all_data A
#        LEFT JOIN afl_top_50 B
#        ON A.Player = B.Player
#        AND A.season = B.season + 1
#        
#        ')
# all_data$prev_season_AFL_player_top_50_flag <- ifelse(is.na(all_data$prev_season_AFL_player_top_50_flag&all_data$season>2014),0,all_data$prev_season_AFL_player_top_50_flag)

# Rank the factors on a team / match / season level

# SC scores
all_data <- all_data %>% arrange(match_id, desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_supercoach_scorerank = row_number(desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_supercoach_scorerank = row_number(desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(supercoach_score), desc(afl_fantasy_score)) %>% group_by(season) %>% mutate(season_supercoach_scorerank = order(desc(supercoach_score), desc(afl_fantasy_score)))

#AF scores
all_data <- all_data %>% arrange(match_id, desc(afl_fantasy_score)) %>% group_by(match_id) %>% mutate(match_afl_fantasy_scorerank = row_number(desc(afl_fantasy_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(afl_fantasy_score)) %>% group_by(match_id,Team) %>% mutate(team_afl_fantasy_scorerank = row_number(desc(afl_fantasy_score)))
all_data <- all_data %>% arrange(season, desc(supercoach_score), desc(afl_fantasy_score)) %>% group_by(season) %>% mutate(season_afl_fantasy_scorerank = order(desc(afl_fantasy_score), desc(supercoach_score)))

# Contested possessions
all_data <- all_data %>% arrange(match_id, desc(Contested_possessions),desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_CP_rank = order(desc(Contested_possessions), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Contested_possessions),desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_CP_rank = order(desc(Contested_possessions), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Contested_possessions), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_CP_rank = order(desc(Contested_possessions), desc(supercoach_score)))

# Uncontested possessions
all_data <- all_data %>% arrange(match_id, desc(Uncontested_possessions),desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_UP_rank = order(desc(Uncontested_possessions), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Uncontested_possessions),desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_UP_rank = order(desc(Uncontested_possessions), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Uncontested_possessions), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_UP_rank = order(desc(Uncontested_possessions), desc(supercoach_score)))

# Disposals
all_data <- all_data %>% arrange(match_id, desc(Disposals), desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_D_rank = order(desc(Disposals), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Disposals), desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_D_rank = order(desc(Disposals), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Disposals), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_D_rank = order(desc(Disposals), desc(supercoach_score)))

# Effective disposals
all_data <- all_data %>% arrange(match_id, desc(Effective_disposals), desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_ED_rank = order(desc(Effective_disposals), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Effective_disposals), desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_ED_rank = order(desc(Effective_disposals), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Effective_disposals), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_ED_rank = order(desc(Effective_disposals), desc(supercoach_score)))

# Kicks
all_data <- all_data %>% arrange(match_id, desc(Kicks), desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_K_rank = order(desc(Kicks), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Kicks), desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_K_rank = order(desc(Kicks), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Kicks), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_K_rank = order(desc(Kicks), desc(supercoach_score)))

# Handballs
all_data <- all_data %>% arrange(match_id, desc(Handballs), desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_H_rank = order(desc(Handballs), desc(supercoach_score)))
all_data <- all_data %>% arrange(match_id, Team, desc(Handballs), desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_H_rank = order(desc(Handballs), desc(supercoach_score)))
all_data <- all_data %>% arrange(season, desc(Handballs), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_H_rank = order(desc(Handballs), desc(supercoach_score)))

# Marks
all_data <- all_data %>% arrange(match_id, desc(Marks)) %>% group_by(match_id) %>% mutate(match_M_rank = dense_rank(desc(Marks)))
all_data <- all_data %>% arrange(match_id, Team, desc(Marks)) %>% group_by(match_id,Team) %>% mutate(team_M_rank = dense_rank(desc(Marks)))
all_data <- all_data %>% arrange(season, desc(Marks), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_M_rank = order(desc(Marks), desc(supercoach_score)))

# Goals
all_data <- all_data %>% arrange(match_id, desc(Goals), desc(supercoach_score)) %>% group_by(match_id) %>% mutate(match_G_rank = dense_rank(desc(Goals)))
all_data <- all_data %>% arrange(match_id, Team, desc(Goals), desc(supercoach_score)) %>% group_by(match_id,Team) %>% mutate(team_G_rank = dense_rank(desc(Goals)))
all_data <- all_data %>% arrange(season, desc(Goals), desc(supercoach_score)) %>% group_by(season) %>% mutate(season_G_rank = order(desc(Goals), desc(supercoach_score)))

# Proportion of goals kicked for team
all_data$Goals <- as.integer(all_data$Goals)
all_data$team_goals <- as.integer(all_data$team_goals)
all_data <- all_data %>% mutate(Prop_goals = Goals / team_goals)
all_data <- filter(all_data, season > 2011)

# Assign a player id and then a record id for tracking purposes
player_id <- data.frame(unique(all_data$Player))
names(player_id)[1] <- c("player_name")
player_id <- player_id %>% arrange(Player) %>% mutate(player_id = order(Player))
all_data <- inner_join(x=all_data,y=player_id,by=c("player_name"))

# View(all_data)

# Add more player level features
all_data <- all_data %>% group_by(player_id) %>% mutate(First_season = min(season))
all_data$num_seasons <- all_data$season - all_data$First_season + 1

# Compute the avg number of times a player has been selected to AA squad / team

player <- all_data %>% distinct(season,player_name,AA_squad,AA_team,num_seasons,prev_season_more_than_5_BR, prev_season_more_than_10_BR, prev_season_more_than_15_BR, prev_season_more_than_20_BR, games_played, games_polled, total_votes)
# player$AFL_player_top_50_flag <- ifelse(is.na(player$AFL_player_top_50_flag),0,player$AFL_player_top_50_flag)
player$games_played <- ifelse(is.na(player$games_played),0,player$games_played)
player$games_polled <- ifelse(is.na(player$games_polled),0,player$games_polled)
player$total_votes <- ifelse(is.na(player$total_votes),0,player$total_votes)
player <- player %>% arrange(player_name,season) %>% mutate(num_AA_team = cumsum(AA_team))
player <- player %>% arrange(player_name,season) %>% mutate(num_AA_squad = cumsum(AA_squad))
# player <- player %>% arrange(player_name,season) %>% mutate(num_AFL_top_50 = cumsum(AFL_player_top_50_flag))
player$avg_AA_team <- player$num_AA_team / player$num_seasons
player$avg_AA_squad <- player$num_AA_squad / player$num_seasons

# Compute a player's previous brownlow polling history

player$prev_season_more_than_5_BR <- ifelse(is.na(player$prev_season_more_than_5_BR),0,player$prev_season_more_than_5_BR)
player$prev_season_more_than_10_BR <- ifelse(is.na(player$prev_season_more_than_10_BR),0,player$prev_season_more_than_10_BR)
player$prev_season_more_than_15_BR <- ifelse(is.na(player$prev_season_more_than_15_BR),0,player$prev_season_more_than_15_BR)
player$prev_season_more_than_20_BR <- ifelse(is.na(player$prev_season_more_than_20_BR),0,player$prev_season_more_than_20_BR)
player <- player %>% arrange(player_name,season) %>% mutate(num_season_more_than_5_BR = cumsum(prev_season_more_than_5_BR))
player <- player %>% arrange(player_name,season) %>% mutate(num_season_more_than_10_BR = cumsum(prev_season_more_than_10_BR))
player <- player %>% arrange(player_name,season) %>% mutate(num_season_more_than_15_BR = cumsum(prev_season_more_than_15_BR))
player <- player %>% arrange(player_name,season) %>% mutate(num_season_more_than_20_BR = cumsum(prev_season_more_than_20_BR))
player <- player %>% arrange(player_name,season) %>% mutate(total_votes_since_2012 = cumsum(total_votes))
player$avg_games_polled <- player$games_polled / player$games_played

# Compute the avg number of times a player has been selected as a Top 50 player

player_adjusted <- filter(player_name,season > 2013)
player_adjusted <- player_adjusted %>% group_by(player_id) %>% mutate(First_season = min(season))
player_adjusted$num_season_since_2014 <- player_adjusted$season - player_adjusted$First_season + 1
#player_adjusted$avg_AFL_top_50 <- player_adjusted$num_AFL_top_50 / player_adjusted$num_season_since_2014
player_adjusted <- within(player_adjusted,rm(First_season))
library(sqldf)
player_final <-
  sqldf('SELECT A.*
        , B.num_season_since_2014
        
        FROM player A
        LEFT JOIN player_adjusted B
        ON A.Player = B.Player
        AND A.season = B.season
        
        ')
#player_final$AFL_player_top_50_flag <- ifelse(player_final$season<2014,NA,player_final$AFL_player_top_50_flag)
#player_final$num_AFL_top_50 <- ifelse(player_final$season<2014,NA,player_final$num_AFL_top_50)
player_final <-
  sqldf('SELECT A.*
        , B.avg_games_polled as prev_season_avg_games_polled
        , B.total_votes_since_2012 as prev_season_total_votes_since_2012
        , B.total_votes_since_2012 / A.num_seasons AS prev_season_avg_votes_per_seasons_since_2012
        , B.total_votes as prev_season_total_BR_votes
        FROM player_final A
        LEFT JOIN player_final B
        ON A.Player = B.Player
        AND A.season = B.season + 1
        
        ')
player_final$avg_games_polled <- ifelse(is.na(player_final$avg_games_polled),0,player_final$avg_games_polled)
player_final$prev_season_avg_games_polled <- ifelse(is.na(player_final$prev_season_avg_games_polled),0,player_final$prev_season_avg_games_polled)
check <- filter(player_final,Player == "Tom Mitchell")
View(check)

# Join on player level features to the data set
all_data <- within(all_data, rm("prev_season_more_than_5_BR"))
all_data <- within(all_data, rm("prev_season_more_than_10_BR"))
all_data <- within(all_data, rm("prev_season_more_than_15_BR"))
all_data <- within(all_data, rm("prev_season_more_than_20_BR"))
all_data <- within(all_data, rm("games_played"))
all_data <- within(all_data, rm("games_polled"))
all_data <- within(all_data, rm("total_votes"))

all_data <- left_join(x=all_data,y=player_final,by=c("player_name","player_id","AA_team","AA_squad","num_seasons","season"))
View(all_data)
# Add in a record_id column
all_data <- within(all_data, Record_id <- paste(season, Round,match_id, player_name,player_id,sep = "_"))

columns <- data.frame(colnames(all_data))
write.csv(columns,file=paste0(model_dir,"column_names_v9.csv"))
saveRDS(all_data, file=paste0(model_dir,"final_data_v9.RDS")) 

# Perform summary statistics on votegetters

target <- c("1", "2","3")
votegetters <- filter(all_data, num_votes %in% target)
three_vote <- filter(all_data, num_votes == "3")
unique(votegetters$num_votes)

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
summarize <- count(five_goal_games,c("season","G","Outcome","num_votes"))
write.csv(summarize,file=paste0(data_dir,"five_goal_games.csv"))

View(all_data%>%filter(match_id == "5461"))

# How often does a player get more than 40 touches and poll?

all_data %>% filter(D > 39)
weird_matches <- all_data %>% filter(D > 39 & Votes == "0") %>% select(match_id)
weird <- inner_join(all_data, weird_matches, by = "match_id")
View(weird)
all_data %>% filter(season == "2013" & Player == "Gary Jnr Ablett")
View(all_data %>% filter(season == "2013" & Player == "Gary Jnr Ablett"))
View(all_data %>% filter(match_id == "5628"))

View(all_data%>%filter(match_id == "5343"))