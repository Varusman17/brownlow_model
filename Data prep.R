####################################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
# Update: Ensure top 50 player rating is NA for seasons 2012 and 2013
# Update 2021-08-23: New source of data have solved for data cleaning steps
####################################################################################

install.packages(c("devtools","reshape2","tidyr"))
require(devtools)
install_github("Displayr/flipTime")

rm(list = ls())
gc()

library(fitzRoy)
library(reshape2)
library(tidyr)
library(dplyr)
library(rvest)
library(flipTime)
library(stringr)

data_dir <- "C:\\Users\\User\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\User\\Documents\\Brownlow\\02 Model\\"

# First step is to export a list of player names 
# This will be used to clean up player names in the brownlow votes data
# Manual name fixes to ensure each player name is distinct
player_stats <- fetch_player_stats()
fryzigg <- fetch_player_stats(season = 2019, source = "fryzigg")
View(fryzigg)

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

# Replicate brownlow adjustments
head(fryzigg)




# Only consider regular season matches from 2012 onwards
mr<-subset(results_long,Season > 2011&Season < 2019&Round.Type == "Regular")

# Calculate match outcome
mr$Outcome <-ifelse(mr$Margin >0,"Win",ifelse(mr$Margin<0,"Loss","Draw"))

mr$Game <- as.integer(mr$Game)
mr$Season <- as.integer(mr$Season)

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