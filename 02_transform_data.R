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
  dataInfoPath = paste0(here(), '/Data/training_factor_list.csv'), 
  training_season_cutoff = 2020
) {
  
  # Filter to only rows of dataInfo that match column names in df
  dataInfo <- read_csv(dataInfoPath) %>% 
    inner_join(data.frame(Factor = colnames(df)), by = 'Factor')
  
  # Remove unnecessary variables
  df <- df %>%
    select(all_of(dataInfo$Factor[dataInfo$Include == 'Y']))
  
  # One hot encode all character variables
  chr_vars <- df %>% select(where(is.character)) %>% colnames()
  dummies <- dummyVars(as.formula(paste('~', paste(chr_vars, collapse = ' + '))), data = df)
  df_ohe <- as.data.frame(predict(dummies, newdata = df))
  df_all <- bind_cols(df %>% select(-all_of(chr_vars)), df_ohe)
  
  # Split into training and test sets based on season
  #' [Note: Random splitting vs season splitting]
  #' Because we are using  variables based on previous seasons (brownlow votes, etc.), we can't split
  #' randomly due to information leakage by capturing response variable in our independent variables
  df_all <- df_all %>% 
    # eg. if we want to train on pre2017 and test on 2018, we want to filter out 2019 onwards
    filter(as.numeric(season) <= training_season_cutoff + 1) %>% 
    mutate(train_test = ifelse(as.numeric(season) <= training_season_cutoff, 'train', 'test'))
  
  return(df_all)
  
}


