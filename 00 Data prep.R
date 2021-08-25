####################################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different data sources for modelling
# Update 2021-08-23: New source of data have solved for data cleaning steps
# 
####################################################################################

# install.packages(c("devtools","reshape2","tidyr"))
# require(devtools)
# install_github("Displayr/flipTime")

rm(list = ls())
gc()

source(paste0(data_dir,"00 Setup.R"))

data_dir <- "C:\\Users\\User\\Documents\\Brownlow\\brownlow_model\\"
model_dir <- "C:\\Users\\User\\Documents\\Brownlow\\02 Model\\"

# Fetch stats from fryzigg 
base_stats <- fetch_player_stats(season = list(2014, 2015, 2016, 2017, 2018), source = "fryzigg")

# Add season, full player name and calculate match outcome
base_stats$season <- as.numeric(format(base_stats$date, "%Y"))
base_stats$match_outcome <-ifelse(base_stats$match_margin>0,"Win",ifelse(base_stats$match_margin<0,"Loss","Draw"))
base_stats$player_name <- paste(base_stats$player_first_name, base_stats$player_last_name)

# Add brownlow information
player_stats <- base_stats %>%
  group_by(season, player_id) %>%
  mutate(
    games_polled = length(player_id[brownlow_votes >0]),
    three_vote_games = length(player_id[brownlow_votes == 3]),
    two_vote_games = length(player_id[brownlow_votes == 2]),
    one_vote_games = length(player_id[brownlow_votes == 1]),
    total_votes = sum(brownlow_votes)
    )

# Join All-Australian
all_australian <- read_excel(paste0(data_dir,"all_australian_team.xlsx"))

View(all_australian)

all_data <-left_join(x=player_stats,y=all_australian,by=c("player_name","season","player_team")) 
all_data$AA_squad <- ifelse(is.na(all_data$AA_squad),0,all_data$AA_squad)
all_data$AA_team <- ifelse(is.na(all_data$AA_team),0,all_data$AA_team)
View(all_data)

View(filter(all_data, player_last_name == "Ryder") %>% select(player_name))

check <- filter(all_data, AA_squad == 1)
all_australian %>% distinct(season, player_name) %>% group_by(season) %>% summarize(n())
check %>% distinct(season, player_id) %>% group_by(season) %>% summarize(n())
View(check %>% distinct(season, player_id, player_name))

saveRDS(all_data, file=paste0(model_dir,"all_data.RDS")) 
all_data_restrict <- filter(all_data, Disposals > 7)
saveRDS(all_data_restrict, file=paste0(model_dir,"all_data_restrict.RDS")) 