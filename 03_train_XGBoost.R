####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: Dataframe to train XGBoost model with
# Outputs: Model object
# 
####################################################################################

train_XGBoost <- function(df, y = 'brownlow_votes') {
  
  X <- data.matrix(df %>% select(-y))
  
  model <- xgboost(
    data = X,
    label = df %>% select(y) %>% pull(),
    booster = "gbtree", 
    objective = "reg:squarederror",
    nrounds = 100,
    eta=0.3,
    gamma=0,
    max_depth=6,
    min_child_weight=1,
    subsample=1,
    colsample_bytree=1
  )
  
  return(model)
}