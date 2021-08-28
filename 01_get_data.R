####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: List of years you want data for
# Outputs: Dataframe at a player-game level with added features
#
####################################################################################

get_data <- function(years = list(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)) {
  require(here) 
  require(dplyr)
  require(readxl)
  require(fitzRoy)
  
  # Fetch stats from fryzigg 
  base_stats <- fetch_player_stats(season = years, source = "fryzigg")
  
  # Add season, full player name and calculate match outcome
  base_stats <- base_stats %>% 
    mutate(
      season = as.numeric(format(date, "%Y")),
      margin = ifelse(player_team == match_home_team,match_home_team_score - match_away_team_score,match_away_team_score - match_home_team_score),
      match_outcome = case_when(margin > 0 ~ "Win",margin < 0 ~ "Loss",T ~ 'Draw'),
      player_opposition = ifelse(player_team == match_home_team,match_away_team,match_home_team),
      team_goals = ifelse(player_team == match_home_team,match_home_team_goals,match_away_team_goals),
      team_behinds = ifelse(player_team == match_home_team,match_home_team_behinds,match_away_team_behinds),
      team_score = ifelse(player_team == match_home_team,match_home_team_score,match_away_team_score),
      player_name = paste(player_first_name, player_last_name)
    )
  
  # Add brownlow information
  player_stats <- base_stats %>%
    group_by(season, player_id) %>%
    mutate(
      games_played = length(player_id),
      games_polled = length(player_id[brownlow_votes >0]),
      three_vote_games = length(player_id[brownlow_votes == 3]),
      two_vote_games = length(player_id[brownlow_votes == 2]),
      one_vote_games = length(player_id[brownlow_votes == 1]),
      total_votes = sum(brownlow_votes)
    ) %>% 
    ungroup()
  
  # Join All-Australian
  all_australian <- read_excel(paste0(here(),"/Data/all_australian_team.xlsx"))
  
  all_data <- left_join(x=player_stats,y=all_australian,by=c("player_name","season","player_team")) 
  all_data$AA_squad <- ifelse(is.na(all_data$AA_squad),0,all_data$AA_squad)
  all_data$AA_team <- ifelse(is.na(all_data$AA_team),0,all_data$AA_team)
  
  return(all_data)
}





