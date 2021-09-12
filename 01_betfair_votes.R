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
source(paste0(here(),"/01_get_data.R"))

# Check if scraping is allowed
paths_allowed(paths="https://www.betfair.com.au/hub/2020-brownlow-medal-predictor/")

# Load in main URL
cv_url <- "https://www.betfair.com.au/hub/2020-brownlow-medal-predictor/"
result_table <- NULL


betfair_2020 <- read_html(cv_url)


  for (i in 1:25) {
    
      
    
    
  }
# get rounds    
betfair_2020 <- read_html(cv_url) %>%
  html_elements('a:nth-child(18)') %>%
  html_text()

betfair_2020 <- read_html(cv_url) %>%
  html_elements('div.c-tab__items') %>%
  html_children()
  html_attr('href')

#
betfair_2020 <- read_html(cv_url) %>%
  html_elements('th:nth-child(2)') %>%
  html_text()

# get player names
betfair_2020 <- read_html(cv_url) %>%
  html_elements('#tablepress-2589') %>%
  html_table()

betfair_2020 <- read_html(cv_url) %>%
  html_elements('td:nth-child(1)') %>%
  html_text()

# get votes
betfair_2020 <- read_html(cv_url) %>%
  html_elements('td:nth-child(2)') %>%
  html_text()

?html_element

# Set up looping through season and rounds
for (k in 2014:2021) {
  
  cv_url_season <- paste0("https://aflcoaches.com.au/awards/the-aflca-champion-player-of-the-year-award/leaderboard/",k,"/",k,"01")
  
  for (i in 1:25) {
    skip<-F
    round <- str_pad(i,2,'left','0')
    cv_url_round <- paste0(cv_url_season,round)
    
    #If page doesn't exist go to next round/year
    tryCatch(read_html(cv_url_round), error = function(e) {skip<<-T})
    if(skip) {next} 
    coaches_votes_url <- read_html(cv_url_round)
    
    cv_numbers <- coaches_votes_url %>% 
      html_elements('.col-2') %>% 
      html_text() %>% 
      mgsub("\n|\t",'')
    
    cv_players <- coaches_votes_url %>% 
      html_elements('.col-10') %>% 
      html_text() %>% 
      mgsub::mgsub("\n|\t",'')
    
    result_table_round <- tibble(
      votes = cv_numbers,
      players = cv_players,
      round = as.character(i),
      season = k
    )
    result_table <- rbind(result_table, result_table_round)
    print(i)
  }
  print(k)
}

# Adjust webscraped data to be usable
coaches_votes_data <- result_table %>%
  filter(votes != 'Votes') %>%
  separate(players,c('player_first_name','player_last_name','playing_for_short'), sep = ' ', remove = F) %>% 
  mutate(web_scraped_team = gsub('\\(|\\)','',playing_for_short),
         player_temp = paste(player_first_name, player_last_name),
         match_round = as.numeric(round),
         coaches_votes = as.numeric(votes),
         record_id = paste(season, match_round, player_temp,web_scraped_team, coaches_votes, sep="_")) 

# Load in clean team name
team_mapping <- read_csv(paste0(here(), '/Data/team_attributes.csv'))
coaches_votes_data <- inner_join(coaches_votes_data, team_mapping, by = c('web_scraped_team'))

# Load in name fixes to make it compatible with fryzigg
name_fixes <- read_excel(paste0(here(),"/Data/name_fixes.xlsx"))
coaches_votes_data <- left_join(coaches_votes_data, name_fixes, by = c('player_temp','web_scraped_team')) %>%
  mutate(player_name = coalesce(player_temp_adj, player_temp))

# write out final csv for use
cv_data <- coaches_votes_data %>% 
  select(season, match_round, player_name, player_team, coaches_votes)
write.csv(cv_data,file=paste0(here(), '/Data/coaches_votes.csv'))

# check that every player exists in fryzigg
# df <- get_data() 
# 
# match_coaches <- left_join(df, cv_data, by = c('season','match_round','player_team','player_name'))
# match_coaches$coaches_votes <- ifelse(is.na(match_coaches$coaches_votes),0,match_coaches$coaches_votes)
# 
# cv_check <- cv_data %>%
#   group_by(season, player_name) %>%
#   summarise(total_votes = sum(as.numeric(coaches_votes))) %>%
#   mutate(record_id = paste(season, player_name, total_votes, sep="_")) %>%
#   arrange(season,desc(total_votes))
# 
# join_check <- match_coaches %>%
#   filter(coaches_votes > 0) %>%
#   group_by(season, player_name) %>%
#   summarise(total_votes = sum(as.numeric(coaches_votes))) %>%
#   mutate(record_id = paste(season, player_name, total_votes, sep="_")) %>%
#   arrange(season,desc(total_votes))
# 
# write.csv(cv_check,file=paste0(here(), '/Data/cv_check.csv'))
#write.csv(join_check,file=paste0(here(), '/Data/join_check.csv'))

# Check individual players
# View(match_coaches %>%
#        filter(player_first_name == "Nick", player_team == "Essendon") %>% 
#        distinct(player_first_name, player_last_name, player_name, player_team))
# 
# View(coaches_votes_data %>% 
#        filter(player_first_name == "Nick", player_team == "Essendon") %>%  
#        distinct(player_first_name, player_last_name, player_name, player_team))
# 
# View(cv_data %>% filter(player_name == "Nick OBrien"))