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

# Source data and add factors ---------------------------------------------
df <- get_data()


# Cast data to correct type and split into train and test -----------------
transformed_data <- transform_data(df)


# Train XGBoost model -----------------------------------------------------
model <- train_XGBoost(transformed_data)


# Test model --------------------------------------------------------------

model_diagnostics <- test_model(model, testDf)
