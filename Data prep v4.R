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
# require(devtools)
# install_github("Displayr/flipTime")

rm(list = ls())
gc()

library(fitzRoy)
library(reshape2)
library(tidyr)
library(dplyr)
library(rvest)
library(flipTime)
library(stringr)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\"

# First step is to export a list of player names 
# This will be used to clean up player names in the brownlow votes data
# Manual name fixes to ensure each player name is distinct
View(pl_stats)
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

pl_name <- unique(pl_stats[c("Season","Player","Team")])
head(pl_name)
write.csv(pl_name,file=paste0(data_dir,"player_names.csv"))

# After cleaning data in excel, import brownlow votes data

br_raw <- read.csv(file=paste0(data_dir,"brownlow_final.csv"), header=TRUE, sep=",")
head(br_raw)

br_summ <- br_raw[,c("Season","Player","Team","Games_played","Games_polled","Total_3_vote_games","Total_2_vote_games","Total_1_vote_games","Total_Votes")]
View(br_summ)

br_round <- br_raw[,1:26]

br_round_final <- melt(br_round,value.name ="Votes",variable.name ="Round",id=c("Season","Player","Team"))
br_round_final$Player <- as.character(br_round_final$Player)
br_round_final$Team <- as.character(br_round_final$Team)
br_round_final$Round <- as.character(br_round_final$Round)
head(br_round_final)
unique(br_round_final$Votes)

# Alter round values and restrict data to relevant period

pl_stats$Round <- gsub(' ','_',player_stats$Round)
pl_stats$Round_type <- ifelse(substr(pl_stats$Round,1,5)=="Round","Regular","Finals")
pl_stats <- subset(pl_stats,Season > 2011&Season < 2019&Round_type == "Regular")
tail(pl_stats)
unique(pl_stats$Team)

# Prepare match results
results <- get_match_results()
results_long <- convert_results(results)
View(results_long)

# Only consider regular season matches from 2012 onwards
mr<-subset(results_long,Season > 2011&Season < 2019&Round.Type == "Regular")

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

View(mr)
View(pl_stats)

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
View(br_round_final)

# Join player stats to brownlow votes

all_data<-left_join(x=pl_final,y=br_round_final,by=c("Season","Round","Player","Team"))
all_data <- all_data %>% mutate(Num_Votes = coalesce(all_data$Votes, "0"))
all_data <- within(all_data,rm(Votes))
unique(all_data$Num_Votes)
View(all_data%>%filter(Match_id == "5343"))

# Join on updated player position and DOB

player_position <- read.csv(file=paste0(data_dir,"Position data\\player_position_final.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
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
View(all_data)

# afl_top_50 <- read.csv(file=paste0(data_dir,"afl_players_top_50.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
# all_data <-left_join(x=all_data,y=afl_top_50,by=c("Player","Season","Team")) 

# all_data$AFL_player_top_50_flag <- ifelse(is.na(all_data$AFL_player_top_50_flag&all_data$Season > 2013),0,all_data$AFL_player_top_50_flag)
# View(all_data)


# check <- filter(all_data, AFL_player_top_50_flag == 1)
# check %>% distinct(Season, Player) %>% group_by(Season) %>% summarize(n())

check <- filter(all_data, AA_team == 1)
check %>% distinct(Season, Player) %>% group_by(Season) %>% summarize(n())

saveRDS(all_data, file=paste0(model_dir,"all_data_v4.RDS")) 
all_data_restrict <- filter(all_data, Disposals > 7)
saveRDS(all_data_restrict, file=paste0(model_dir,"all_data_restrict_v4.RDS")) 

# Scraping player position from footywire

# Base site to get the urls of all of the different players

player_url <- read_html("https://www.footywire.com/afl/footy/ft_players")
player_url_info <- player_url %>% html_nodes('a') %>% html_attr('href') # Find all of the links on the page

player_url_info <- data.frame(player_url_info) # Insert values to a dataframe
colnames(player_url_info) <- "urls" # Rename column to urls
player_url_info$urls <- as.character(player_url_info$urls) # Convert from factor to character
player_url_info <- na.omit(player_url_info) # Remove NAs created by converting to character
player_url_info <- player_url_info %>% filter(str_detect(player_url_info$urls, "pp-"))
View(player_url_info)
nrow(player_url_info)

# Create empty dataframe for inserting data
rm(Position_data)
Position_data <- data.frame(Player=character(),
                            Team=character(), 
                            Position=character(),
                            DOB = as.Date(rep(0,0), origin = "1900-01-01")
                            ,stringsAsFactors = FALSE)

team_short <- c("geelon","st-kil","wester","west-c","collin","gold-c","kangar","sydney","brisba","essend","port-a","freman","richmo","adelai","melbou","hawtho","greate","carlto")
team_full <- c("Geelong","St Kilda", "Western Bulldogs","West Coast","Collingwood", "Gold Coast", "North Melbourne", "Sydney","Brisbane","Essendon","Port Adelaide","Fremantle","Richmond", "Adelaide","Melbourne","Hawthorn","GWS","Carlton")
team_mapping <- data.frame(team_short, team_full, stringsAsFactors = FALSE)
View(team_mapping)

# Loop through all of the player urls

for(i in 1:nrow(player_url_info)) 
{
      webpage <- read_html(paste0("https://www.footywire.com/afl/footy/",player_url_info[i,1]))
      
      Data1 <- webpage %>% html_nodes('#playerProfileData1') %>% html_text()
      Data2 <- webpage %>% html_nodes('#playerProfileData2') %>% html_text()
      Name <- webpage %>% html_nodes('#playerProfileName') %>% html_text()
      Team <- webpage %>% html_nodes('#playerProfileTeamDiv') %>% html_text()
      
      Player_name <- data.frame(Name)
      colnames(Player_name) <- paste0("Player")
      Player_name$Player <- as.character(Player_name$Player)
      
      pos_matches <- data.frame(str_locate(Data2,"Position:"))
      Player_position <- data.frame(substr(Data2,pos_matches$end + 1,nchar(Data2)))
      colnames(Player_position) <- paste0("Position")
      Player_position$Position <- gsub("[[:space:]]", "", Player_position$Position)
      Player_position$Position <- as.character(Player_position$Position)
      
      age_matches <- data.frame(str_locate(Data1,"Born:"))
      origin_matches <- data.frame(str_locate(Data1,"Origin:"))
      Player_birthdate <- data.frame(substr(Data1,age_matches$end+1,origin_matches$start-6))
      colnames(Player_birthdate) <- paste0("DOB")
      Player_birthdate$DOB <- anydate(Player_birthdate$DOB)
      
      Player_team <- data.frame(substr(player_url_info[i,1],4,9))
      colnames(Player_team) <- paste0("team_short")  
      Player_team$team_short <- as.character(Player_team$team_short)
      Player_team <- inner_join(Player_team,team_mapping, by="team_short")
      Player_team_final <- data.frame(Player_team$team_full)
      colnames(Player_team_final) <- paste0("Team")
      Player_team_final$Team <- as.character(Player_team_final$Team)
      
      data <- data.frame(Player_name, Player_team_final, Player_position,Player_birthdate)
      
      Position_data[i,] <- data
      
}
View(Position_data)

# Manual fixes for player names to ensure consistency

Position_data[Position_data$Player == "Scott Thompson" & Position_data$Team== "North Melbourne", "Player"] <- "Scott D. Thompson"
Position_data[Position_data$Player == "Tom Lynch" & Position_data$Team== "Gold Coast", "Player"] <- "Thomas J. Lynch"

write.csv(Position_data,file=paste0(data_dir,"Position data\\player_positions.csv"))

# Read in player position data for current players

Position_data <- read.csv(file=paste0(data_dir,"Position data\\player_positions.csv"), header=TRUE, sep=",",stringsAsFactors = FALSE)
Position_data <- within(Position_data, rm("X"))
View(Position_data)

# Check how many players position matches

position_check <-left_join(x=all_data,y=Position_data,by=c("Player"))
View(position_check)
View(unique(all_data$Player))
all_positions <- position_check %>% distinct(Season, Player, Team.x, Position, Team.y, DOB)
all_positions$Position <- trimws(all_positions$Position)
write.csv(all_positions,file=paste0(data_dir,"Position data\\player_positions_all.csv"))
View(all_positions)

# Loop through the urls for past players to get their position

past_players <- read.csv(file=paste0(data_dir,"Position data\\player_position_unmatched.csv"), header=TRUE, sep=",")
# View(past_players)

past_player_url_info <- data.frame(unique(past_players$Footywire_link)) # Insert values to a dataframe
colnames(past_player_url_info) <- "urls" # Rename column to urls
past_player_url_info$urls <- trimws(past_player_url_info$urls)
past_player_url_info$urls <- as.character(past_player_url_info$urls) # Convert from factor to character
View(past_player_url_info)
nrow(past_player_url_info)

rm(Past_position_data)
Past_position_data <- data.frame(Player=character(),
                            Position=character(),
                            DOB = as.Date(rep(0,0), origin = "1900-01-01"),
                            stringsAsFactors = FALSE)

for(i in 1:nrow(past_player_url_info)) 
{
  webpage <- read_html(paste0("https://www.footywire.com/afl/footy/",past_player_url_info[i,1]))
  
  Data1 <- webpage %>% html_nodes('#playerProfileData1') %>% html_text()
  Data2 <- webpage %>% html_nodes('#playerProfileData2') %>% html_text()
  Name <- webpage %>% html_nodes('#playerProfileName') %>% html_text()
  Team <- webpage %>% html_nodes('#playerProfileTeamDiv') %>% html_text()
  
  Player_name <- data.frame(Name)
  colnames(Player_name) <- paste0("Player")
  Player_name$Player <- as.character(Player_name$Player)
  
  pos_matches <- data.frame(str_locate(Data2,"Position:"))
  Player_position <- data.frame(substr(Data2,pos_matches$end + 1,nchar(Data2)))
  colnames(Player_position) <- paste0("Position")
  Player_position$Position <- gsub("[[:space:]]", "", Player_position$Position)
  Player_position$Position <- as.character(Player_position$Position)
  
  age_matches <- data.frame(str_locate(Data1,"Born:"))
  origin_matches <- data.frame(str_locate(Data1,"Origin:"))
  Player_birthdate <- data.frame(substr(Data1,age_matches$end+1,origin_matches$start-6))
  colnames(Player_birthdate) <- paste0("DOB")
  Player_birthdate$DOB <- anydate(Player_birthdate$DOB)
  
  data <- data.frame(Player_name,Player_position,Player_birthdate)
  
  Past_position_data[i,] <- data
  
}

Past_position_data <- unique(Past_position_data)
View(Past_position_data)
View(count(Past_position_data))
data
library(tidyr)

Past_position_data <- Past_position_data %>% drop_na(DOB)
Past_position_data$DOB <- as.character(Past_position_data$DOB)

# Check how many players position matches

position_final <-left_join(x=all_positions,y=Past_position_data,by=c("Player"))
position_final$Position_final <- ifelse(is.na(position_final$Position.y), position_final$Position, position_final$Position.y)
position_final$DOB_final <- ifelse(is.na(position_final$DOB.x), position_final$DOB.y, position_final$DOB.x)
View(position_final)

colnames(position_final)[which(names(position_final) == "Team.x")] <- "Team"
position_final <- within(position_final, rm("Team.y"))
position_final <- within(position_final, rm("Position.y"))
position_final <- within(position_final, rm("Position"))
position_final <- within(position_final, rm("DOB.x"))
position_final <- within(position_final, rm("DOB.y"))
colnames(position_final)[which(names(position_final) == "Position_final")] <- "Position"
colnames(position_final)[which(names(position_final) == "DOB_final")] <- "DOB"
position_final <- unique(position_final)
write.csv(position_final,file=paste0(data_dir,"Position data\\player_positions_extract.csv"))

