####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: 
#   - Dataframe to transform
#   - csv stating data type of each feature (default)
# 
# Outputs: Transformed data set ready for modelling
#
####################################################################################

transform_data <- function(df, training_season_cutoff, testing_season, dataInfoPath = paste0(here(), '/Data/training_factor_list.csv')) {
  
  # Add in variables that are proportional to the match
  df <- df %>% 
    group_by(match_id) %>% 
    mutate(
      kicks_prop = kicks / sum(kicks),
      marks_prop = marks / sum(marks),
      handballs_prop = handballs / sum(handballs),
      disposals_prop = disposals / sum(disposals),
      goals_prop = goals / sum(goals),
      behinds_prop = behinds / sum(behinds),
      tackles_prop = tackles / sum(tackles),
      inside_fifties_prop = inside_fifties / sum(inside_fifties),
      clearances_prop = clearances / sum(clearances),
      contested_possessions_prop = contested_possessions / sum(contested_possessions),
      uncontested_possessions_prop = uncontested_possessions / sum(uncontested_possessions),
      contested_marks_prop = contested_marks / sum(contested_marks),
      marks_inside_fifty_prop = marks_inside_fifty / sum(marks_inside_fifty),
      goal_assists_prop = goal_assists / sum(goal_assists),
      turnovers_prop = turnovers / sum(turnovers),
      shots_at_goal_prop = shots_at_goal / sum(shots_at_goal)
    ) %>% 
    ungroup()
  
  # Filter to only rows of dataInfo that match column names in df
  dataInfo <- read_csv(dataInfoPath) %>% 
    inner_join(data.frame(Factor = colnames(df)), by = 'Factor')

  # Remove unnecessary variables but keeping unique identifier for the match
  df_removed <- df %>%
    select(all_of(dataInfo$Factor[dataInfo$Include == 'Y']), match_id, match_round, player_team, season)  
  
  # One hot encode all character variables
  chr_vars <- df_removed %>% select(where(is.character), -player_team) %>% colnames()
  # dummies <- dummyVars(as.formula(paste('~', paste(chr_vars, collapse = ' + '))), data = df_removed)
  # df_ohe <- as.data.frame(predict(dummies, newdata = df_removed))
  dummies <- dummy_cols(
    df_removed, 
    select_columns = chr_vars, 
    remove_first_dummy = T, 
    remove_selected_columns = T
  )
  
  df_all <- bind_cols(dummies, data.frame(player_name = df$player_name))
  
  # Split into training and test sets based on season
  #' [Note: Random splitting vs season splitting]
  #' Because we are using  variables based on previous seasons (brownlow votes, etc.), we can't split
  #' randomly due to information leakage by capturing response variable in our independent variables
  df_all <- df_all %>% 
    # eg. if we want to train on pre2017 and test on 2018, we want to filter out 2019 onwards
    filter(season >= training_season_cutoff - 4 & season <= testing_season) %>% 
    mutate(train_test = ifelse(as.integer(season) <= training_season_cutoff, 'train', 'test'))
  
  # Take out response
  df_response <- df_all %>% 
    filter(train_test == "train" & brownlow_votes > 0)
  
  # Take 7 rows where no brownlow votes were polled
  set.seed(123)
  df_zeros <- df_all %>%
    filter(train_test == "train" & brownlow_votes == 0) %>% 
    group_by(match_id) %>% 
    sample_n(7)
  
  # Take test season
  df_test <- df_all %>% 
    filter(train_test == "test")
  
  # Union this dataset ready for modelling
  df_modelling <- bind_rows(df_response, df_zeros, df_test)
  df_modelling <- df_modelling %>% sample_n(as.numeric(count(df_modelling)))
  
  list_output = list(
    entire_df = df_modelling,
    X_train = df_modelling %>%
      filter(train_test == 'train') %>% 
      select(-player_name, -train_test, -brownlow_votes, -match_id, -match_round, -season,  -player_team),
    y_train = df_modelling %>% filter(train_test == 'train') %>% select(brownlow_votes) %>% pull(),
    X_test = df_modelling %>%
      filter(train_test == 'test') %>% 
      select(-player_name, -train_test, -brownlow_votes, -match_id, -match_round, -season, -player_team),
    y_test = df_modelling %>% filter(train_test == 'test') %>% select(brownlow_votes) %>% pull()
  )
  
  return(list_output)
  
}


