####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Description: Pipeline file that runs individual functions 
# 
####################################################################################

rm(list = ls())
gc()
library(here)
source(paste0(here(),"/00_setup.R"))
source(paste0(here(),"/01_get_data.R"))
source(paste0(here(),"/02_transform_data.R"))
source(paste0(here(),"/03_train_XGBoost.R"))
source(paste0(here(),"/04_test_model.R"))

# Source data and add factors ---------------------------------------------
df <- get_data()


# Cast data to correct type and split into train and test -----------------
transformed_data <- transform_data(df, training_season_cutoff = 2018) # Try and predict on 2019 and assess model performance


# Train XGBoost model -----------------------------------------------------
model <- train_XGBoost(transformed_data$X_train, transformed_data$y_train)


# Test model --------------------------------------------------------------
model_diagnostics <- test_model(model, transformed_data$X_test, transformed_data$entire_df)


# Explore model diagnostics -----------------------------------------------
# Plot variable importance
xgb.plot.importance(model_diagnostics$variable_importance[1:10])

# Explore predicted votes vs actual votes at player level
model_diagnostics$votes_by_player
