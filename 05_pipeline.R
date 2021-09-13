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

# Set training season cutoff (i.e. predict for the next year)

training_season_cutoff <- 2019
testing_season <- training_season_cutoff + 1

# Source data and add factors ---------------------------------------------
df <- get_data()

# Cast data to correct type and split into train and test -----------------
transformed_data <- transform_data(df, training_season_cutoff, testing_season)


# Train XGBoost model -----------------------------------------------------
model <- train_XGBoost(transformed_data$X_train, transformed_data$y_train)


# Test model --------------------------------------------------------------
model_diagnostics <- test_model(model, transformed_data$X_test, transformed_data$entire_df, testing_season)


# Explore model diagnostics -----------------------------------------------
# Plot variable importance
xgb.plot.importance(model_diagnostics$variable_importance[1:10])


# Explore predicted votes vs actual votes at player level
View(model_diagnostics$votes_by_player %>% arrange(desc(predicted_votes)))

# Inspect matches where coaches, predicted and actual votes were greater than 0
# 2021 SC scores have not been updated in fryzigg package

View(
  model_diagnostics$votes_by_match %>%
  filter(brownlow_votes >0 | predicted_votes > 0 | coaches_votes >0) %>%
  select(player_team, player_name, player_positionC, supercoach_score, afl_fantasy_score, brownlow_votes, predicted_votes, coaches_votes)
)


# View how the coaches votes perform for vote-getting performances
ggplot(model_diagnostics$votes_by_match %>% filter(brownlow_votes > 0),
  aes(x = coaches_votes, fill = factor(brownlow_votes))) +
  geom_bar(position = "fill")+
  xlab('Coaches Votes') +
  ylab('Count') +
  ggtitle('Coaches Votes by Brownlow Votes')

# View how the predicted votes perform for vote-getting performances
ggplot(model_diagnostics$votes_by_match %>% filter(brownlow_votes > 0),
  aes(x = predicted_votes, fill = factor(brownlow_votes))) +
  geom_bar(position = "fill")+
  xlab('Predicted Votes') +
  ylab('Count') +
  ggtitle('Predicted Votes by Brownlow Votes')

# View how the predicted votes compare to coaches votes
ggplot(model_diagnostics$votes_by_match %>% filter(predicted_votes > 0),
  aes(x = coaches_votes, fill = factor(predicted_votes))) +
  geom_bar(position = "fill")+
  xlab('Coaches Votes') +
  ylab('Count') +
  ggtitle('Coaches Votes by Predicted Votes')

# Correlation between features
# data <- model_diagnostics$votes_by_match %>% 
#   select(-player_name, -season)
# 
# corr <- round(cor(data),2)
# p.mat <- cor_pmat(data)
# ggcorrplot(corr, p.mat = p.mat, type = "lower", insig = "blank")

