####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: AFLCA champion player of the year votes from 2014 to 2021
# Outputs: CSV of coaches votes dataset at the player level
# 
####################################################################################

rm(list = ls())
gc()
library(here)
source(paste0(here(),"/00_setup.R"))

# Check if scraping is allowed
# paths_allowed(paths="https://www.betfair.com.au/hub/2020-brownlow-medal-predictor/")

# Load in main URL
cv_url_2019 <- "https://www.betfair.com.au/hub/2019-brownlow-medal-predictor/"
cv_url_2020 <- "https://www.betfair.com.au/hub/2020-brownlow-medal-predictor/"
cv_url_2021 <- "https://www.betfair.com.au/hub/brownlow-medal-predictor/"

# Load round names
round_2019 <- read_html(cv_url_2019) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_text()
round_2020 <- read_html(cv_url_2020) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_text()
round_2021 <- read_html(cv_url_2021) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_text()

# Load data tabs
data_tabIDs_2019 <- read_html(cv_url_2019) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_attr("data-tab")
data_tabIDs_2020 <- read_html(cv_url_2020) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_attr("data-tab")
data_tabIDs_2021 <- read_html(cv_url_2021) %>% html_elements('div.c-tab__items') %>% html_children() %>% html_attr("data-tab")

# Get round to data_tab_mapping   
data_tab_mapping_2019 <-
  bind_cols(
    data.frame(season = 2019),
    data.frame(match_round = round_2019),
    data.frame(data_tab = as.integer(data_tabIDs_2019))
  ) %>% 
  mutate(
    match_round = as.integer(gsub(".*Round ","",match_round))
  )

data_tab_mapping_2020 <-
  bind_cols(
    data.frame(season = 2020),
    data.frame(match_round = round_2020),
    data.frame(data_tab = as.integer(data_tabIDs_2020))
  ) %>% 
  mutate(
    match_round = as.integer(gsub(".*R","",match_round))
  )

data_tab_mapping_2021 <-
  bind_cols(
    data.frame(season = 2021),
    data.frame(match_round = round_2021),
    data.frame(data_tab = as.integer(data_tabIDs_2021))
  ) %>% 
  mutate(
    match_round = as.integer(gsub(".*R","",match_round))
  )

# Combine mapping
data_tab_mapping <- rbind(data_tab_mapping_2019, data_tab_mapping_2020, data_tab_mapping_2021)

# Grab all of the tables with votes information
betfair_votes_2019 <- read_html(cv_url_2019) %>% html_elements('div.c-tab__contents') %>% html_children() %>% html_table()
betfair_votes_2020 <- read_html(cv_url_2020) %>% html_elements('div.c-tab__contents') %>% html_children() %>% html_table()
betfair_votes_2021 <- read_html(cv_url_2021) %>% html_elements('div.c-tab__content.u-overflow-auto') %>% html_text()
betfair_votes_2021 <- str_split(betfair_votes_2021,"\n")

# Loop through and clean 2019 results
result_table_2019 <- NULL
for (i in 1:23) {
  
  result_table_round_2019 <- 
    bind_cols(
      data.frame(season = 2019),
      data.frame(data_tab = i - 1),
      betfair_votes_2019[[i]] %>% filter(Player != 'Player')
    )
  result_table_2019 <- rbind(result_table_round_2019, result_table_2019)
  
}
# Adjust 2019 results
result_table_2019 <- result_table_2019 %>% 
  filter(Player != 'PLayer') %>% 
  select(season, data_tab, player_name = Player, betfair_votes = `Projected Votes`)

# Loop through and clean 2020 results
result_table_2020 <- NULL
for (i in 1:18) {
  
  result_table_round_2020 <- 
      bind_cols(
        data.frame(season = 2020),
        data.frame(data_tab = i - 1),
        betfair_votes_2020[[i]] %>% filter(Player != 'Player')
    )
  result_table_2020 <- rbind(result_table_round_2020, result_table_2020)
  
}

# Adjust 2020 results
result_table_2020 <- result_table_2020 %>% 
  mutate(len = nchar(Player)) %>% 
  filter(len > 0) %>% 
  select(season, data_tab, player_name = Player, betfair_votes = `Projected Votes`)

# Loop through and clean 2021 results
result_table_2021 <- NULL
for (i in 1:23) {
  
  result_table_round_2021 <- 
    bind_cols(
      data.frame(season = 2021),
      data.frame(data_tab = i - 1),
      data.frame(string_parse = betfair_votes_2021[[i]])
    )
  result_table_2021 <- rbind(result_table_round_2021, result_table_2021)
  
}

# Adjust 2021 results
teams <- c("ADELAIDE", "BRISBANE", "CARLTON", "COLLINGWOOD", "ESSENDON","FREMANTLE","GEELONG", "GWS","GOLD","HAWTHORN","MELBOURNE","PORT ADELAIDE","RICHMOND","ST", "SYDNEY","WEST COAST","WESTERN BULLDOGS")
result_table_2021 <- result_table_2021 %>% 
  mutate(
    len = nchar(string_parse),
    first = gsub("([A-Za-z]+).*", "\\1", string_parse),
    player_name = gsub("\\s+[^ ]+$", "", string_parse),
    betfair_votes = gsub(".*\\s", "", string_parse)
  ) %>% 
  filter(len > 0 & len < 24) %>% 
  filter(!(first %in% teams)) %>% 
  select(season, data_tab, player_name, betfair_votes)

# Combine results
result_table <- rbind(result_table_2019, result_table_2020, result_table_2021)

# Adjust webscraped data to be usable
player_team_mapping <- read_csv(paste0(here(),"/Data/player_team_mapping.csv"))
betfair_team <- result_table %>% 
  left_join(data_tab_mapping, by=c("data_tab","season")) %>% 
  select(season, match_round, player_name, betfair_votes) %>% 
  mutate(player_name = replace(player_name, season == 2019 & match_round %in% c(4,18,19) & player_name == 'Tom Lynch','Tom J Lynch')) %>%
  mutate(player_name = replace(player_name, season == 2020 & match_round == 15 & player_name == 'Tom Lynch','Tom J Lynch')) %>%
  mutate(player_name = replace(player_name, season == 2021 & match_round %in% c(7,16) & player_name == 'Tom Lynch','Tom J Lynch')) %>%
  left_join(player_team_mapping, by = c("season", "player_name")) %>% 
  select(season, match_round, player_temp = player_name, player_team, betfair_votes)

# Write out list of unmatched names
write.csv(subset(betfair_team, is.na(player_team)), file=paste0(here(), '/Data/unmatched_betfair.csv'))

# Load in name fixes to make it compatible with fryzigg
name_fixes <- read_csv(paste0(here(),"/Data/name_fixes.csv"))
betfair_data <- left_join(betfair_team, name_fixes, by = c('player_temp')) %>%
  mutate(
    player_name = coalesce(player_temp_adj, player_temp),
    player_team = coalesce(player_team.x, player_team.y)
  ) %>% 
  select(season, match_round, player_name, player_team, betfair_votes) %>% 
  filter(!(player_name == 'Tom J. Lynch' & player_team == 'Gold Coast'))  

# Write out final csv for use
write.csv(betfair_data,file=paste0(here(), '/Data/betfair_votes.csv'))
