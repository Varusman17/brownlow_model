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

transform_data <- function(
  df, 
  training_season_cutoff = 2020,
  dataInfoPath = paste0(here(), '/Data/training_factor_list.csv')
) {
  
  # Filter to only rows of dataInfo that match column names in df
  dataInfo <- read_csv(dataInfoPath) %>% 
    inner_join(data.frame(Factor = colnames(df)), by = 'Factor')
  
  # Remove unnecessary variables but keeping unique identifier for the match
  df_removed <- df %>%
    select(all_of(dataInfo$Factor[dataInfo$Include == 'Y']), match_id)
  
  # One hot encode all character variables
  chr_vars <- df_removed %>% select(where(is.character)) %>% colnames()
  dummies <- dummyVars(as.formula(paste('~', paste(chr_vars, collapse = ' + '))), data = df_removed)
  df_ohe <- as.data.frame(predict(dummies, newdata = df_removed))
  df_all <- bind_cols(df_removed %>% select(-all_of(chr_vars)), df_ohe, data.frame(player_name = df$player_name))
  
  # Split into training and test sets based on season
  #' [Note: Random splitting vs season splitting]
  #' Because we are using  variables based on previous seasons (brownlow votes, etc.), we can't split
  #' randomly due to information leakage by capturing response variable in our independent variables
  df_all <- df_all %>% 
    # eg. if we want to train on pre2017 and test on 2018, we want to filter out 2019 onwards
    filter(as.numeric(season) <= training_season_cutoff + 1) %>% 
    mutate(train_test = ifelse(as.numeric(season) <= training_season_cutoff, 'train', 'test'))
  
  list_output = list(
    entire_df = df_all,
    X_train = df_all %>%
      filter(train_test == 'train') %>% 
      select(-player_name, -train_test, -brownlow_votes, -match_id),
    y_train = df_all %>% filter(train_test == 'train') %>% select(brownlow_votes) %>% pull(),
    X_test = df_all %>%
      filter(train_test == 'test') %>% 
      select(-player_name, -train_test, -brownlow_votes, -match_id),
    y_test = df_all %>% filter(train_test == 'test') %>% select(brownlow_votes) %>% pull()
  )
  
  return(list_output)
  
}


