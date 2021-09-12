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

test_model <- function(model, X_test, entire_df, testing_season) {
  
  # Pull out variable importance
  names <- dimnames(data.matrix(X_test))[[2]]
  imp <- xgb.importance(
    feature_names = names, 
    model = model
  )
  
  #' [1.] Predict on the latest season in the dataset
  #' [2.] Bind with rest of test dataset
  #' [3.] Assign votes of 3, 2, and 1 for each game based on the predictions
  #' [4.] Aggregate for each player
  brownlow_pred <- predict(model, data.matrix(X_test))
  latest_season_with_pred <- bind_cols(
    X_test, 
    data.frame(pred = brownlow_pred), 
    entire_df %>% 
      filter(train_test == 'test') %>% 
      select(player_name, brownlow_votes, match_id, match_round, player_team, season)
  )
  votes_by_match <- latest_season_with_pred %>% 
    group_by(match_id, match_round) %>% 
    mutate(
      pred_rank = row_number(desc(pred)),
      predicted_votes = case_when(
        pred_rank == 1 ~ 3,
        pred_rank == 2 ~ 2,
        pred_rank == 3 ~ 1,
        T ~ 0
      )
    )
  votes_by_player <- votes_by_match %>% 
    group_by(player_name, player_team) %>% 
    summarise(
      actual_votes = sum(brownlow_votes),
      predicted_votes = sum(predicted_votes),
      coaches_votes = sum(coaches_votes),
      prediction_sum = sum(pred),
      votes_accuracy = actual_votes - predicted_votes
    ) %>% 
    arrange(desc(actual_votes)) %>% 
    ungroup()
  
  # Store the total difference in predicted vs actual votes across the top 20 players
  top20_abs_diff <- votes_by_player %>% 
    head(20) %>% 
    mutate(votes_accuracy = abs(votes_accuracy)) %>% 
    summarise(top20_diff = sum(votes_accuracy))

  
  # Record the model run and store variable names, variable importance, votes by player and votes by match
  season <- testing_season
  datetime <- format(Sys.time(),'%Y%d%m_%H%M')
  new_dir <- glue('{here()}/Logs/season_{season}_diff_{top20_abs_diff}_{datetime}')
  dir.create(new_dir)
  write_csv(data.frame(features_used = colnames(X_test)), glue('{new_dir}/features_used.csv'))
  write_csv(imp, glue('{new_dir}/variable_importance.csv'))
  write_csv(votes_by_match, glue('{new_dir}/votes_by_match.csv'))
  write_csv(votes_by_player %>% arrange(desc(predicted_votes)), glue('{new_dir}/votes_by_player.csv'))
  
  # Arrange output into list
  model_performance_list <- list(
    variable_importance = imp,
    pred = latest_season_with_pred,
    votes_by_match = votes_by_match,
    votes_by_player = votes_by_player,
    top20_abs_diff = top20_abs_diff
  )
  
  return(model_performance_list)
}