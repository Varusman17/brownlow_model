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
 
  # Import data
  all_australian <- read_excel(paste0(here(),"/Data/all_australian_team.xlsx"))
  coaches_votes <- read_csv(paste0(here(),"/Data/coaches_votes.csv"), show_col_types = FALSE)
  captains <- read_excel(paste0(here(),"/Data/captains.xlsx"))
  fantasy_scores <- read_csv(paste0(here(), '/Data/fantasy_scores.csv'), show_col_types = FALSE)
  betfair_votes <- read_csv(paste0(here(), '/Data/betfair_votes.csv'), show_col_types = FALSE)
  player_position <- read_csv(paste0(here(), '/Data/player_position.csv'), show_col_types = FALSE)
  # br_2020 <- read_csv(paste0(here(),"/Data/brownlow_2020.csv"))
  # br_2020 <- br_2020[,1:21]
  # br_2020 <- melt(br_2020,value.name ="BR",variable.name ="match_round",id=c("season","player_name","player_team"))
  # br_2020 <- br_2020 %>% 
  #   filter(BR > 0) %>% 
  #   filter(BR != "DNP") %>%
  #   mutate(
  #     match_round = as.integer(match_round),
  #     BR = as.integer(BR)
  #   ) 

  # Add season, full player name and calculate match outcome
  base_stats <- fetch_player_stats(season = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021), source = "fryzigg") %>% 
    #fetch_player_stats(season = years, source = "fryzigg") %>% 
    # Restrict to H&A season, players 
    filter(time_on_ground_percentage > 0 & as.integer(match_round) < 24) %>%
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
      af_score = afl_fantasy_score
      # br_votes = brownlow_votes
    )

  # Distinct list of players
  all_players <- base_stats %>% distinct(player_id, player_name)  
  
  # All matches
  all_stats <- fitzRoy::get_fryzigg_stats() %>%
    mutate(
      season = as.integer(format(date, "%Y")),
      player_name = paste(player_first_name, player_last_name),
      br_game_votes = brownlow_votes
    ) %>%
    select(season, match_round, player_id, player_name, player_team, br_game_votes)
  
  # Number of games and brownlow votes per games
  experience <- right_join(all_stats, all_players, by = c("player_id", "player_name")) %>%
    group_by(player_id, player_name) %>%
    mutate(
      num_games = row_number()
    ) %>%
    ungroup() %>%
    mutate(match_round = as.integer(match_round)) %>%
    filter(as.integer(match_round) < 24)
    # join on 2020 brownlow votes
    # left_join(br_2020, by=c("season", "match_round", "player_team", "player_name")) %>%
    # mutate(
    #   br_game_votes = ifelse(is.na(BR), br_votes, BR),
    # ) %>%
    # select(-BR, -br_votes)
    # 
  # Summarise brownlow polling history
  br_summ <-  experience %>% 
    group_by(season, player_id, player_name) %>%
    summarise(
      num_HA_games = length(player_id),
      num_votes = sum(br_game_votes),
      games_polled = length(player_id[br_game_votes >0]),
    ) %>%
    group_by(player_id, player_name) %>% 
    mutate(
      total_HA_games = cumsum(num_HA_games),
      total_votes = cumsum(num_votes),
      total_games_polled = cumsum(games_polled),
      avg_votes_per_game = total_votes / total_HA_games,
      avg_games_polled = total_games_polled / total_HA_games
    ) %>%
    mutate(
      prev_season_br_votes = lag(num_votes, order_by = season),
      prev_season_games_polled = lag(games_polled, order_by = season),
      rolling_avg_votes_per_game = lag(avg_votes_per_game, order_by = season),
      rolling_avg_games_polled = lag(avg_games_polled, order_by = season)
    ) %>% 
    select(season, player_id, player_name, prev_season_br_votes, prev_season_games_polled, rolling_avg_votes_per_game, rolling_avg_games_polled)

  # Create final clean dataset
  all_data <- base_stats %>% 
    # Join 2020 brownlow votes
    # left_join(br_2020, by=c("season", "match_round", "player_team", "player_name")) %>%
    # mutate(brownlow_votes = ifelse(is.na(BR), br_votes, BR)) %>%
    # select(-BR,-br_votes) %>% 
    # # Join all australian
    left_join(all_australian,by=c("player_name","season","player_team")) %>% 
    mutate(
      aa_squad = ifelse(is.na(aa_squad),0,aa_squad),
      aa_team = ifelse(is.na(aa_team),0,aa_team)
    ) %>% 
    # Join coaches votes
    left_join(coaches_votes, by=c('season','match_round','player_team','player_name')) %>% 
    mutate(coaches_votes = ifelse(is.na(coaches_votes),0,coaches_votes)) %>% 
    # Join captain info
    left_join(captains, by=c("player_name","season","player_team")) %>% 
    mutate(captain = ifelse(is.na(captain),0,captain)) %>% 
    # Join 2020 and 2021 fantasy scores
    left_join(fantasy_scores, by=c("season", "match_round", "player_team", "player_name")) %>%
    mutate(
      supercoach_score = ifelse(is.na(sc_score), SC, sc_score),
      afl_fantasy_score = ifelse(is.na(af_score), AF, af_score)
    ) %>%
    select(-sc_score, -SC, -af_score, -AF, -footywire_player) %>%  
    # Join player experience
    left_join(experience, by=c("season", "match_round", "player_id", "player_name","player_team")) %>%
    select(-br_game_votes) %>% 
    # Join past polling history
    left_join(br_summ, by=c("season","player_id", "player_name")) %>% 
    mutate(
      prev_season_br_votes = ifelse(is.na(prev_season_br_votes),0,prev_season_br_votes),
      prev_season_games_polled = ifelse(is.na(prev_season_games_polled),0,prev_season_games_polled),
      rolling_avg_votes_per_game = ifelse(is.na(rolling_avg_votes_per_game),0,rolling_avg_votes_per_game),
      rolling_avg_games_polled = ifelse(is.na(rolling_avg_games_polled),0,rolling_avg_games_polled)   
    ) %>%
    # Join betfair votes
    left_join(betfair_votes, by=c("season","match_round","player_team", "player_name")) %>%
    mutate(betfair_votes = ifelse(season %in% c(2019,2020,2021) & is.na(betfair_votes),0,betfair_votes))  
  
  return(all_data)
}


# Add brownlow information
# player_stats <- br_stats %>%
#   group_by(season, player_id) %>%
#   mutate(
#     games_played = length(player_id),
#     games_polled = length(player_id[brownlow_votes >0]),
#     three_vote_games = length(player_id[brownlow_votes == 3]),
#     two_vote_games = length(player_id[brownlow_votes == 2]),
#     one_vote_games = length(player_id[brownlow_votes == 1]),
#     total_votes = sum(brownlow_votes)
#   ) %>% 
#   ungroup()


