####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: Dataframe to train XGBoost model with
# Outputs: Model object
# 
####################################################################################

train_XGBoost <- function(X, y, type = 'classification') {
  
  X <- data.matrix(X)
  # y <- factor(y, levels = c(0, 1, 2, 3), ordered = T)
  
  if (type == 'classification') {
    model <- xgboost(
      data = X,
      label = y,
      booster = "gbtree", 
      objective = "multi:softprob",
      eval_metric="mlogloss",
      num_class = 4,
      nrounds = 100,
      eta=0.3,
      gamma=0,
      max_depth=6,
      min_child_weight=1,
      subsample=1,
      colsample_bytree=1
    )
  } else {
    model <- xgboost(
      data = X,
      label = y,
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
  }
  
  return(model)
}