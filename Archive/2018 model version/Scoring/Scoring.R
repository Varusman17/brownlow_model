####################################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
# Update: Ensure top 50 player rating is NA for seasons 2012 and 2013 
#
####################################################################################

# install.packages(c("devtools","reshape2","tidyr"))
# install_github("Displayr/flipTime")
# Re-install package to get updated data
devtools::install_github("jimmyday12/fitzRoy")

rm(list = ls())
gc()

library(fitzRoy)
library(reshape2)
library(tidyr)
library(dplyr)
library(rvest)
library(flipTime)
library(stringr)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\Scoring\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\Scoring\\"

# First step is to export a list of player names 
# This will be used to clean up player names in the brownlow votes data
# Manual name fixes to ensure each player name is distinct

player_stats >- update_footywire_stats()
pl_stats<-player_stats

pl_stats[pl_stats$Player == "Scott Thompson" & pl_stats$Team== "North Melbourne", "Player"] <- "Scott D. Thompson"
pl_stats[pl_stats$Player == "Tom Lynch" & pl_stats$Team== "Gold Coast", "Player"] <- "Thomas J. Lynch"
pl_stats[pl_stats$Player == "Will H-Elliott", "Player"] <- "Will Hoskin-Elliott"
pl_stats[pl_stats$Player == "Sam P-Seton", "Player"] <- "Sam Petrevski-Seton"
pl_stats[pl_stats$Player == "Sam P-Pepper", "Player"] <- "Sam Powell-Pepper"
pl_stats[pl_stats$Player == "Darcy B-Jones", "Player"] <- "Darcy Byrne-Jones"
pl_stats[pl_stats$Player == "George H-Smith", "Player"] <- "George Horlin-Smith"
pl_stats[pl_stats$Player == "Anthony M-Tipungwuti", "Player"] <- "Anthony McDonald-Tipungwuti"
pl_stats[pl_stats$Player == "Lewis R-Thomson", "Player"] <- "Lewis Roberts-Thomson"
pl_stats[pl_stats$Player == "Nathan L-Murray", "Player"] <- "Nathan Lovett-Murray"
pl_stats[pl_stats$Player == "Trent D-Lane", "Player"] <- "Trent Dennis-Lane"
pl_stats[pl_stats$Player == "Alex N-Bullen", "Player"] <- "Alex Neal-Bullen"
pl_stats[pl_stats$Player == "Jay K-Harris", "Player"] <- "Jay Kennedy-Harris"
pl_stats[pl_stats$Player == "Cameron E-Yolmen", "Player"] <- "Cameron Ellis-Yolmen"
pl_stats[pl_stats$Player == "Ed V-Willis", "Player"] <- "Ed Vickers-Willis"
pl_stats[pl_stats$Player == "Josh D-Cardillo", "Player"] <- "Josh Deluca-Cardillo"
pl_stats[pl_stats$Player == "Luke D-Uniacke", "Player"] <- "Luke Davies-Uniacke"

# Alter round values and restrict data to 2018

pl_stats$Round <- gsub(' ','_',player_stats$Round)
pl_stats$Round_type <- ifelse(substr(pl_stats$Round,1,5)=="Round","Regular","Finals")
pl_stats <- subset(pl_stats,Season > 2011&Round_type == "Regular")
pl_name <- unique(pl_stats[c("Season","Player","Team")])
# View(pl_name)
write.csv(pl_name,file=paste0(data_dir,"player_names.csv"))

# After cleaning data in excel, import brownlow votes data

br_raw <- read.csv(file=paste0(data_dir,"brownlow_final.csv"), header=TRUE, sep=",")
head(br_raw)

br_summ <- br_raw[,c("Season","Player","Team","Games_played","Games_polled","Total_3_vote_games","Total_2_vote_games","Total_1_vote_games","Total_Votes")]
# View(br_summ)

br_round <- br_raw[,1:26]

br_round_final <- melt(br_round,value.name ="Votes",variable.name ="Round",id=c("Season","Player","Team"))
br_round_final$Player <- as.character(br_round_final$Player)
br_round_final$Team <- as.character(br_round_final$Team)
br_round_final$Round <- as.character(br_round_final$Round)

# Prepare match results
results <- get_match_results()
results_long <- convert_results(results)

# Only consider regular season matches from 2018

mr<-subset(results_long,Season > 2011&Round.Type == "Regular")

# Adjust team names to ensure consistency with footywire stats
mr[mr$Team== "Footscray", "Team"] <- "Western Bulldogs"
mr[mr$Team== "Brisbane Lions", "Team"] <- "Brisbane"

# Adjust round names for ease of joining with other datasets
mr$Round <- gsub('R','Round_',mr$Round)

# Calculate match outcome
mr$Outcome <-ifelse(mr$Margin >0,"Win",ifelse(mr$Margin<0,"Loss","Draw"))

mr$Game <- as.integer(mr$Game)
mr$Season <- as.integer(mr$Season)

# Perform checks to ensure that the two data sets have the same number of games
mr %>% group_by(Season) %>% summarize(n())
pl_stats %>% distinct(Season, Date, Round, Match_id) %>% group_by(Season) %>% summarize(n())

# View(mr)
# View(pl_stats)

# Join player stats to match results

pl_final <- inner_join(pl_stats,mr, by = c("Season","Round","Team","Date","Status"))

# Check that the joined data set also has the same number of games
pl_final %>% distinct(Season, Date, Round, Match_id) %>% group_by(Season) %>% summarize(n())
pl_final <- within(pl_final, rm(Round.Type, Round.Number, Game, Venue.y))
colnames(pl_final)[which(names(pl_final) == "Venue.x")] <- "Venue"
colnames(pl_final)[which(names(pl_final) == "Goals")] <- "Team_goals"
colnames(pl_final)[which(names(pl_final) == "Behinds")] <- "Team_behinds"
colnames(pl_final)[which(names(pl_final) == "Points")] <- "Team_points"
View(pl_final)

# Join player stats to brownlow votes

all_data<-left_join(x=pl_final,y=br_round_final,by=c("Season","Round","Player","Team"))
all_data <- all_data %>% mutate(Num_Votes = coalesce(all_data$Votes, "0"))
all_data <- within(all_data,rm(Votes))
# unique(all_data$Num_Votes)
# View(all_data)

# Join on updated player position and DOB

player_position <- read.csv(file=paste0(data_dir,"player_position_final.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
all_data <-inner_join(x=all_data,y=player_position,by=c("Player","Season","Team"))
all_data$DOB <- AsDate(all_data$DOB)

# Rename all columns to make it usable
all_data <- within(all_data,rm(GA1))
names(all_data)[10:12] <- c("Contested_possessions","Uncontested_possessions","Effective_disposals")
names(all_data)[13:33] <- c("Disposal_efficiency","Contested_marks","Goal_assists","Marks_inside_50","One_percenters","Bounces","Time_on_ground","Kicks","Handballs","Disposals","Marks","Goals","Behinds","Tackles","Hitouts","Inside_50s","Clearances","Clangers","Rebound_50s","Frees_for","Frees_against")
names(all_data)[36:42] <- c("Centre_clearances","Stoppage_clearances","Score_involvements","Metres_gained","Turnovers","Intercepts","Tackles_inside_50")

# Join All-Australian and AFL Top 50 Player ratings

all_australian <- read.csv(file=paste0(data_dir,"all_australian_team.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
all_data <-left_join(x=all_data,y=all_australian,by=c("Player","Season","Team")) 

all_data$AA_squad <- ifelse(is.na(all_data$AA_squad),0,all_data$AA_squad)
all_data$AA_team <- ifelse(is.na(all_data$AA_team),0,all_data$AA_team)
# View(all_data)

afl_top_50 <- read.csv(file=paste0(data_dir,"afl_players_top_50.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
all_data <-left_join(x=all_data,y=afl_top_50,by=c("Player","Season","Team")) 

all_data$AFL_player_top_50_flag <- ifelse(is.na(all_data$AFL_player_top_50_flag&all_data$Season > 2013),0,all_data$AFL_player_top_50_flag)
# View(all_data)


check <- filter(all_data, AFL_player_top_50_flag == 1)
check %>% distinct(Season, Player) %>% group_by(Season) %>% summarize(n())

check <- filter(all_data, AA_team == 1)
check %>% distinct(Season, Player) %>% group_by(Season) %>% summarize(n())

saveRDS(all_data, file=paste0(model_dir,"all_data_scoring.RDS")) 
all_data_restrict <- filter(all_data, Disposals > 7)
saveRDS(all_data_restrict, file=paste0(model_dir,"all_data_restrict_scoring.RDS")) 

# Scraping player position from footywire

# Loop through the urls for past players to get their position

new_players <- read.csv(file=paste0(data_dir,"player_position_unmatched.csv"), header=TRUE, sep=",")
View(new_players)

new_player_url_info <- data.frame(unique(new_players$Footywire_link)) # Insert values to a dataframe
colnames(new_player_url_info) <- "urls" # Rename column to urls
new_player_url_info$urls <- as.character(new_player_url_info$urls) # Convert from factor to character
View(new_player_url_info)
nrow(new_player_url_info)

rm(New_position_data)
New_position_data <- data.frame(Player=character(),
                            Position=character(),
                            #DOB = as.Date(rep(0,0), origin = "1900-01-01"),
                            DOB = character(),
                            stringsAsFactors = FALSE)

for(i in 1:nrow(new_player_url_info)) 
{
  webpage <- read_html(paste0("https://www.footywire.com/afl/footy/",new_player_url_info[i,1]))
  
  extract_info <- webpage %>% html_nodes('.ldrow') %>% html_text()
  Player_pos <- gsub("[[:space:]]", "", extract_info[6])
  Player_age <- gsub("[[:space:]]", "", extract_info[5])
  
  Player_name <- data.frame(gsub("\n", "", extract_info[2]))
  colnames(Player_name) <- paste0("Player")
  Player_name$Player <- as.character(Player_name$Player)
  
  pos_matches <- data.frame(str_locate(Player_pos,"Position:"))
  Player_position <- data.frame(substr(Player_pos,pos_matches$end + 1,nchar(Player_pos)))
  colnames(Player_position) <- paste0("Position")
  Player_position$Position <- as.character(Player_position$Position)
  
  age_matches <- data.frame(str_locate(Player_age,"Born:"))
  Player_birthdate <- data.frame(substr(Player_age,age_matches$end+1,nchar(Player_age)))
  colnames(Player_birthdate) <- paste0("DOB")
  Player_birthdate$DOB <- as.character(Player_birthdate$DOB)  
  # Player_birthdate$DOB <- AsDate(Player_birthdate$DOB)
  
  data <- data.frame(Player_name,Player_position,Player_birthdate)
  
  New_position_data[i,] <- data
  
}

New_position_data <- unique(New_position_data)
View(New_position_data)

library(tidyr)

New_position_data <- New_position_data %>% drop_na(DOB)
New_position_data$DOB <- AsDate(New_position_data$DOB)
write.csv(New_position_data,file=paste0(data_dir,"player_positions_new.csv"))

rm(list = ls())
gc()

library(fitzRoy)
library(reshape2)
library(tidyr)
library(dplyr)
library(rvest)
library(sqldf)
library(eeptools)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\Scoring\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\Scoring\\"

# Load data

all_data <- readRDS(file=paste0(model_dir,"all_data_restrict_scoring.RDS"))

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

# View(br_summ)

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
# View(all_data)

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
View(all_data)
all_data$Prev_season_top_5_BR_rank <- ifelse(all_data$Prev_season_BR_rank <6, 1, 0)
all_data$Prev_season_top_10_BR_rank <- ifelse(all_data$Prev_season_BR_rank <11, 1, 0)
all_data$Prev_season_top_20_BR_rank <- ifelse(all_data$Prev_season_BR_rank <21, 1, 0)
all_data$Prev_season_top_30_BR_rank <- ifelse(all_data$Prev_season_BR_rank <31, 1, 0)
all_data$Prev_season_top_50_BR_rank <- ifelse(all_data$Prev_season_BR_rank <51, 1, 0)
# Calculate player age

all_data$Player_age <- floor(age_calc(all_data$DOB,all_data$Date, units = "years"))

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
write.csv(columns,file=paste0(model_dir,"column_names_scoring.csv"))
saveRDS(all_data, file=paste0(model_dir,"final_data_scoring.RDS")) 

rm(list = ls())
gc()

library(data.table)
library(RODBC)
library(caret)

options(scipen=20)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\Scoring\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\Scoring\\"

# Read in factor data

factor_data <- readRDS(file=paste0(model_dir,"final_data_scoring.RDS"))
factor_data <- data.table(factor_data)

# Read factor list table

factorListTable <- read.csv(paste0(model_dir,"training_factor_list_v8.csv"))
ResponseCol <- c(as.character(factorListTable[factorListTable$Response=="Y","Factor"]))
keyCols <- c(as.character(factorListTable[factorListTable$Key=="Y","Factor"]))
View(factorListTable)

# Grab additional variable information

var_names <- colnames(factor_data)
var_table <- data.frame(Factor = var_names)
var_table$row_ID <-1:nrow(var_table)
# write.csv(var_table,paste0(model_dir,"var_table.csv"),row.names=FALSE)
var_table <- merge(var_table,factorListTable,by = "Factor",all.x = TRUE)
var_table <- var_table[(var_table$InputFactor == "Y")&(var_table$Factor != ResponseCol), ]
View(var_table)

# Drop remaining non-input factors

non_input_vars <- colnames(factor_data)[(!colnames(factor_data)%in%var_table$Factor)&(colnames(factor_data) != ResponseCol)&(colnames(factor_data) != 'Record_id')]
factor_data[, (non_input_vars) := NULL]
var_table <- var_table[!var_table$Factor%in%non_input_vars,]

library(dplyr)
var_table <- var_table %>% arrange(row_ID)
View(var_table)
# View(factor_data)

# Check that info has been provided for all input columns 
var_table[is.na(var_table$Type),]
# View(factor_data)
colnames(factor_data)

# Move the response column to the end of the dataset
factor_data <- factor_data%>%select(-Num_Votes,everything())
View(factor_data)

# specify monotonic column vector
monotone_neg_names <- var_table$Factor[var_table$Monotone==-1]
monotone_pos_names <- var_table$Factor[var_table$Monotone==1]
monotone_var_vec <- c(ifelse(colnames(factor_data)%in%monotone_neg_names,-1,ifelse(colnames(factor_data)%in%monotone_pos_names,1,0)))
monotone_var_vec <- ifelse(is.na(monotone_var_vec),0,monotone_var_vec)
monotone_var_vec <- monotone_var_vec[1:96]
monotone_var_vec

# print monotonic variables -- check log to make sure these are as expected
print(colnames(factor_data)[monotone_var_vec==1])
print(colnames(factor_data)[monotone_var_vec==-1])

# Convert factor variables

factorVars <- c(as.character(factorListTable[factorListTable$Type=="Factor","Factor"]))
remaining_factorVars <- factorVars[factorVars%in%colnames(factor_data)]

numericVars <- c(as.character(factorListTable[factorListTable$Type=="Numeric","Factor"]))
remaining_numericVars <- numericVars[numericVars%in%colnames(factor_data)]

factor_data[,(remaining_factorVars):=lapply(.SD, as.character),.SDcols=remaining_factorVars]
factor_data[,(remaining_factorVars):=lapply(.SD, as.factor),.SDcols=remaining_factorVars]
factor_data[,(remaining_numericVars):=lapply(.SD, as.numeric),.SDcols=remaining_numericVars]

View(factor_data)

# Restrict to 2018 data

factor_data$Season <- substr(factor_data$Record_id,1,4)
scoring_2018_data <- filter(factor_data,Season == "2018")
scoring_2018_data <- within(scoring_2018_data,rm(Season))
saveRDS(scoring_2018_data, file=paste0(model_dir,"scoring_2018_data.RDS")) 
View(scoring_2018_data)

rm(list = ls())
gc()

library(pdp)
library(gbm)
library(ggplot2)
library(ROCR)
library(dplyr)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\Scoring\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\Scoring\\"

# Read in 2018 data

scoring_data <- readRDS(file=paste0(model_dir,"scoring_2018_data.RDS"))
final_data <- readRDS(file=paste0(model_dir,"final_data_scoring.RDS"))

# Load model for assessment

load(paste0("C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\training_gbmFit_v8.RData"))

scoring_preds_linear <- predict(gbmFit,scoring_data)
scoring_preds <- data.frame(llh = exp(scoring_preds_linear), Record_id = scoring_data$Record_id)
View(scoring_preds)

# Predict the total number of votes per player

library(dplyr)
final_data_with_2018_pred <- inner_join(x = final_data, y = scoring_preds, by = c("Record_id"))
final_data_with_2018_pred <- final_data_with_2018_pred %>% arrange(Match_id, desc(llh)) %>% group_by(Match_id) %>% mutate(Expected_vote_rank = order(desc(llh)))

library(sqldf)
final_data_with_2018_pred <-
  sqldf('SELECT A.*
        , CASE WHEN Expected_vote_rank = 1 THEN 3
        WHEN Expected_vote_rank = 2 THEN 2
        WHEN Expected_vote_rank = 3 THEN 1
        ELSE 0 END AS Pred_votes
        FROM final_data_with_2018_pred A
        
        ')

View(final_data_with_2018_pred)
write.csv(final_data_with_2018_pred,paste0(model_dir,"final_predictions_2018_by_game.csv"),row.names=FALSE)


# Make predictions for 2018 Brownlow

library(dplyr)
expected_votes <- final_data_with_2018_pred %>% group_by(Player, Position, Team) %>% summarise(Expected_votes = sum(llh))
predicted_votes <- final_data_with_2018_pred %>% group_by(Player, Position, Team) %>% summarise(Predicted_votes = sum(Pred_votes))
model_votes <- inner_join(x = expected_votes, y = predicted_votes, by = c("Player","Position","Team"))
model_votes$Pred_BR_rank <- row_number(desc(model_votes$Predicted_votes))
View(model_votes)
write.csv(model_votes,paste0(model_dir,"final_predictions_2018.csv"),row.names=FALSE)
