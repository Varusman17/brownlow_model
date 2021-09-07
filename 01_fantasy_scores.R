####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: Fantasy scores from 2020 and 2021 from footywire
# Outputs: CSV of fantasy scores for 2020 and 2021 at the player level 
# 
####################################################################################

rm(list = ls())
gc()
library(here)
source(paste0(here(),"/00_setup.R"))
source(paste0(here(),"/01_get_data.R"))

years <- list(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)

base_stats <- fetch_player_stats(season = years, source = "fryzigg")

# Add season, full player name and calculate match outcome
base_stats <- base_stats %>% 
  filter(time_on_ground_percentage > 0) %>%
  mutate(
    match_round = as.integer(match_round),
    season = as.integer(format(date, "%Y")),
    margin = ifelse(player_team == match_home_team,match_home_team_score - match_away_team_score,match_away_team_score - match_home_team_score),
    match_outcome = case_when(margin > 0 ~ "Win",margin < 0 ~ "Loss",T ~ 'Draw'),
    player_opposition = ifelse(player_team == match_home_team,match_away_team,match_home_team),
    team_goals = ifelse(player_team == match_home_team,match_home_team_goals,match_away_team_goals),
    team_behinds = ifelse(player_team == match_home_team,match_home_team_behinds,match_away_team_behinds),
    team_score = ifelse(player_team == match_home_team,match_home_team_score,match_away_team_score),
    player_name = paste(player_first_name, player_last_name)
  ) %>%
  filter(!is.na(match_round)) 

# No fantasy scores from 2020 and no SC scores in 2021
View(
  df %>%
    group_by(season) %>%
    summarise(min_AF = min(afl_fantasy_score)
              , max_AF = max(afl_fantasy_score)
              , min_SC = min(supercoach_score)
              , max_SC = max(supercoach_score)
    )
)

# Fetch fantasy scores from 2020 and 2021
sc_scores <- fetch_player_stats(season = c(2020,2021), source = "footywire") %>%
  mutate(
    round_adj = gsub('Round','',Round),
    player_team = mgsub(Team, pattern = c('Brisbane','GWS'), replacement = c('Brisbane Lions', 'Greater Western Sydney')),
    match_round = as.integer(round_adj)
  ) %>%
  filter(match_round < 24) %>%
  rename(season = Season, player_name = Player) %>%
  select(season, match_round, player_name, player_team, AF, SC)  

# Write out this dataset as a csv
write.csv(sc_scores, file=paste0(here(), '/Data/fantasy_scores.csv'))

# Read in csv after making manual adjustments for the following games:
# 2021 Round 9 St Kilda vs Geelong missing SC scores
# 2021 Round 13 Richmond vs West Coast missing SC scores
sc_scores_temp <- read_csv(paste0(here(), '/Data/fantasy_scores.csv'))

# Add 2020 and 2021 sc scores
base_stats_adj <- left_join(base_stats, sc_scores_temp, by=c('season', 'match_round','player_team', 'player_name')) %>%
  mutate(
    sc_score = ifelse(is.na(supercoach_score), SC, supercoach_score),
    af_score = ifelse(is.na(afl_fantasy_score), AF, afl_fantasy_score)
  )

unmatched_name <- base_stats_adj %>% filter(is.na(sc_score)) %>% distinct(season, match_round, player_name, player_team)
write.csv(unmatched_name,file=paste0(here(), '/Data/unmatched_name.csv'))

# Most common player names in the AFL
base_stats_adj %>%
  filter(season == 2021) %>%
  group_by(player_first_name) %>%
  summarise(
    name_count = n_distinct(player_id)
  ) %>%
  unique() %>%
  arrange(desc(name_count)) %>%
  View()
