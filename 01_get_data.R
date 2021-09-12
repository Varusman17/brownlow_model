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
  
  # Add season, full player name and calculate match outcome
  base_stats <- fetch_player_stats(season = years, source = "fryzigg") %>% 
    # remove players who were the sub but didn't come on
    filter(time_on_ground_percentage > 0) %>%
    # restrict to just home and away season
    filter(as.integer(match_round) < 24) %>%
    mutate(
      match_round = as.integer(match_round),
      season = as.integer(format(date, "%Y")),
      margin = ifelse(player_team == match_home_team,match_home_team_score - match_away_team_score,match_away_team_score - match_home_team_score),
      match_outcome = case_when(margin > 0 ~ "Win",margin < 0 ~ "Loss",T ~ 'Draw'),
      player_opposition = ifelse(player_team == match_home_team,match_away_team,match_home_team),
      team_goals = ifelse(player_team == match_home_team,match_home_team_goals,match_away_team_goals),
      team_behinds = ifelse(player_team == match_home_team,match_home_team_behinds,match_away_team_behinds),
      team_score = ifelse(player_team == match_home_team,match_home_team_score,match_away_team_score),
      player_name = paste(player_first_name, player_last_name),
      sc_score = supercoach_score,
      af_score = afl_fantasy_score,
      br_votes = brownlow_votes
    )
  
  # Load 2020 brownlow votes
  br_2020 <- read_csv(paste0(here(),"/Data/brownlow_2020.csv"))
  br_2020 <- br_2020[,1:21]
  br_2020 <- melt(br_2020,value.name ="BR",variable.name ="match_round",id=c("season","player_name","player_team"))
  br_2020 <- br_2020 %>% 
    filter(BR > 0) %>% 
    filter(BR != "DNP") %>%
    mutate(
      match_round = as.integer(match_round),
      BR = as.integer(BR)
    )  
  
  # Add 2020 brownlow information
  br_stats <- left_join(base_stats, br_2020, by=c("season", "match_round", "player_team", "player_name")) %>%
    mutate(
      brownlow_votes = ifelse(is.na(BR), br_votes, BR)
    )

  
  # Add brownlow information
  player_stats <- br_stats %>%
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
  aa_data <- left_join(player_stats,all_australian,by=c("player_name","season","player_team")) 
  aa_data$aa_squad <- ifelse(is.na(aa_data$aa_squad),0,aa_data$aa_squad)
  aa_data$aa_team <- ifelse(is.na(aa_data$aa_team),0,aa_data$aa_team)
  
  # Join coaches votes
  coaches_votes <- read_csv(paste0(here(),"/Data/coaches_votes.csv"))
  cv_data <- left_join(aa_data,coaches_votes, by=c('season','match_round','player_team','player_name'))
  cv_data$coaches_votes <- ifelse(is.na(cv_data$coaches_votes),0,cv_data$coaches_votes)
  
  # Join captain info
  captains <- read_excel(paste0(here(),"/Data/captains.xlsx"))
  captain_data <- left_join(cv_data,captains, by=c("player_name","season","player_team"))
  captain_data$captain <- ifelse(is.na(captain_data$captain),0,captain_data$captain)
  
  # Join 2020 and 2021 fantasy scores
  fantasy_scores <- read_csv(paste0(here(), '/Data/fantasy_scores.csv'))
  all_data <- left_join(captain_data, fantasy_scores, by=c("season", "match_round", "player_team", "player_name")) %>%
    mutate(
      supercoach_score = ifelse(is.na(sc_score), SC, sc_score),
      afl_fantasy_score = ifelse(is.na(af_score), AF, af_score)
    ) %>%
    select(-sc_score, -SC, -af_score, -AF)
  
  return(all_data)
}
