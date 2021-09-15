####################################################################################
#
# Author: Saurav Acharya & Mitchell Ong-Thomson
# Project: AFL Brownlow prediction
# Inputs: Dataframe to train ordinal logistic regression model with
# Outputs: Model object
# 
####################################################################################

train_oLogit <- function(X_train, y_train, X_test, y_test) {
  
  # Create ordinal logistic formula
  X_names <- X_train %>% select(-disposals, -team_score) %>% colnames()
  OLogit_formula <- paste0('actual_votes ~ ', paste0(X_names, collapse = ' + '))
  
  # Bind independent variables with dependent variable as ordered factor
  OLogit_training_df <- X_train %>% 
    bind_cols(
      data.frame(
        actual_votes = factor(
          y_train, 
          levels = c('0', '1', '2', '3'), 
          ordered = T
        )
      )
    )
  
  # Train ordinal logistic model and predict on X_test
  m <- MASS::polr(OLogit_formula, data = OLogit_training_df)
  pred <- predict(m, newdata = X_test, type = 'probs')
  
  # Build list object to return
  return_list <- list(
    model = m,
    predicted_probs = pred
  )
  
  return(return_list)
}
# X_names <- transformed_data$X_train %>% select(-disposals, -team_score) %>% colnames()
# OLogit_formula <- paste0('actual_votes ~ ', paste0(X_names, collapse = ' + '))
# OLogit_training_df <- transformed_data$X_train %>% 
#   bind_cols(
#     data.frame(
#       actual_votes = factor(
#         transformed_data$y_train, 
#         levels = c('0', '1', '2', '3'), 
#         ordered = T
#       )
#     )
#   )
# m <- MASS::polr(OLogit_formula, data = OLogit_training_df)
# pred <- predict(m, newdata = transformed_data$X_test, type = 'probs')
# 
# test_names <- names(m$coefficients)
# polr_train <- OLogit_training_df %>% select(all_of(test_names))
# polr_test <- transformed_data$X_test %>% select(all_of(test_names))
# m <- MASS::polr(OLogit_formula, data = polr_train)
# 
# pred <- predict(m, newdata = polr_test, type = 'probs')
# 
# test <- data.frame(a = c('a', 'b', 'c'))
# 
# test_dummies <- dummy_cols(
#   test, 
#   select_columns = test %>% select(where(is.character)) %>% colnames(), 
#   remove_first_dummy = T, 
#   remove_selected_columns = T
# )
# 
# table <- data.frame(
#   round = ,
#   player = ,
#   votes
# )
# for (round_iter in 2:length(list)) {
#   table <- bind_rows(
#     table,
#     bind_cols(
#       data.frame(round = round_iter),
#       list[[round_iter]]
#     )
#   )
# }
# 
# 
# names <- dimnames(data.matrix(transformed_data$X_test))[[2]]
# imp <- xgb.importance(
#   feature_names = names, 
#   model = model
# )
# 
# brownlow_pred <- predict(model, data.matrix(transformed_data$X_test), reshape = T)
# colnames(brownlow_pred) <- c('vote_0', 'vote_1', 'vote_2', 'vote_3')
# 
# brownlow_pred %>% 
#   as_tibble() %>% 
#   # arrange(desc(vote_2)) %>% 
#   mutate(expected_votes = vote_1+vote_2*2 + vote_3*3) %>% 
#   rowwise() %>% 
#   mutate(pred = case_when(
#     vote_0 == max(vote_0, vote_1, vote_2, vote_3) ~ 0,
#     vote_1 == max(vote_0, vote_1, vote_2, vote_3) ~ 1,
#     vote_2 == max(vote_0, vote_1, vote_2, vote_3) ~ 2,
#     T ~ 3
#   ))
# 
# brownlow_pred %>% rename()
# 
# test <- list('a', 'b', 'c')
# unlist(test)
# 
# model_diagnostics$votes_by_player %>% select(actual_votes, predicted_votes) %>% distinct()
# 
# model_diagnostics$votes_by_player %>% 
#   select(actual_votes, predicted_votes) %>% 
#   distinct() %>% 
#   mutate(diff = abs(actual_votes-predicted_votes)) %>% 
#   summarise(total_diff = sum(diff))
# 
# read_csv(paste0(here(), '/Logs/season_2020_diff_63_20211409_0108/votes_by_player.csv')) %>% 
#   select(actual_votes, predicted_votes) %>% 
#   distinct() %>% 
#   mutate(diff = abs(actual_votes-predicted_votes)) %>% 
#   summarise(total_diff = sum(diff))
# 
# 
# test <- list.files(paste0(here(), '/logs'))
# 
# str_extract(test, '(?<=season_)[0-9]{4}')
