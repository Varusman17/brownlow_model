####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: 
#   - Model object
#   - Dataframe to testing
# Outputs: List of model diagnostics
# 
####################################################################################

test_model <- function(model, df) {
  
  names <- dimnames(data.matrix(df))[[2]]
  names <- names[names != 'brownlow_votes']
  
  imp <- xgb.importance(
    feature_names = names, 
    model = model
  )
  
  model_performance_list <- list(
    variable_importance = imp
  )
  
  return(model_performance_list)
}