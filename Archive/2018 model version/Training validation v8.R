#####################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
#
#####################################################

# install.packages(c("pdp","gbm","ROCR"))

library(pdp)
library(gbm)
library(ggplot2)
library(ROCR)
library(dplyr)

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\"

version_no <- "v8"

# Load training data. 

trainDescr <- readRDS(file=paste0(model_dir,"Training//trainDescr_",version_no,".RDS"))

trainDescr <- trainDescr%>%select(-Num_Votes,everything())
View(trainDescr)
# print monotonic variables -- check log to make sure these are as expected

print(colnames(trainDescr)[monotone_var_vec==1])
print(colnames(trainDescr)[monotone_var_vec==-1])

# Run GBM

set.seed(123)
trainDescr <- trainDescr[sample(nrow(trainDescr)),]

set.seed(123)
trainDescr <- trainDescr[sample(nrow(trainDescr)),]

gbmFit <- gbm(`Num_Votes` ~ . - Record_id,
              data = trainDescr,
              distribution = "poisson",
              var.monotone = monotone_var_vec,
              n.trees = 1500,
              shrinkage = 0.02,
              interaction.depth = 4,
              bag.fraction = 0.5,
              train.fraction = 0.8,
              # n.minobsinnode = 100,
              # cv.folds = 5,
              verbose = TRUE
)

mean(predict(gbmFit,trainDescr, type="response"))

# Training performance

png(filename=paste0(model_dir,"Training\\training_performance_",version_no,".png"))
gbm.perf(gbmFit, method="test")
dev.off()

# Save model object for further use 
save(gbmFit, file = paste0(model_dir,"training_gbmFit_",version_no,".RData"))

# Load model for assessment

load(paste0(model_dir,"training_gbmFit_",version_no,".RData"))

# Analyse variable influence
varImp_df <- data.frame(summary.gbm(gbmFit,n.trees=c(1:gbmFit$n.trees)[gbmFit$valid.error==min(gbmFit$valid.error)]))
varImp_plot <- ggplot(varImp_df[1:15,],aes(x=var,y=rel.inf)) +
  geom_bar(stat="identity", fill="black") +
  scale_x_discrete(limits=rev(as.character(varImp_df[1:15,"var"])),labels=rev(as.character(varImp_df[1:15,"var"]))) +
  coord_flip() +
  xlab("Factor\n") +
  ylab("\nRelative Importance")
varImp_df
ggsave(filename=paste0(model_dir,"Training\\variable_importance_plot_",version_no,".png"), plot = varImp_plot)
write.csv(varImp_df,file=paste0(model_dir,"Training\\variable_importance_",version_no,".csv"))

n_vars <- 96

# Plot and save partial dependency curves
i <- 1
for (var in varImp_df$var[1:n_vars]){
  png(filename=paste0(model_dir,"Training\\PD_plots\\",version_no,"\\",i,"_",var,".png"))
  plot.gbm(gbmFit,i.var = var,type="response")
  dev.off()
  i <- i + 1
}


# Load training/testing data

train_data <- readRDS(file=paste0(model_dir,"Training//trainDescr_",version_no,".RDS"))
test_data <- readRDS(file=paste0(model_dir,"Testing//testDescr_",version_no,".RDS"))
test_2017_data <- readRDS(file=paste0(model_dir,"Testing//test_2017_data_",version_no,".RDS"))
final_data <- readRDS(file=paste0(model_dir,"final_data_",version_no,".RDS"))

test_2016_data <- filter(final_data,Season == 2016)
test_2016_data$Num_Votes <- as.numeric(test_2016_data$Num_Votes)

train_preds_linear <- predict(gbmFit,train_data)
test_preds_linear <- predict(gbmFit,test_data)
test_2017_preds_linear <- predict(gbmFit,test_2017_data)
test_2017_preds_linear <- predict(gbmFit,test_2017_data)
test_2016_preds_linear <- predict(gbmFit,test_2016_data)

# Convert predictions into a probability using log link 

train_preds <- data.frame(llh = exp(train_preds_linear), response = train_data$Num_Votes, Record_id = train_data$Record_id,  model = 'training')
test_preds <- data.frame(llh = exp(test_preds_linear), response = test_data$Num_Votes, Record_id = test_data$Record_id, model = 'testing')
test_2017_preds <- data.frame(llh = exp(test_2017_preds_linear), response = test_2017_data$Num_Votes, Record_id = test_2017_data$Record_id, model = 'testing 2017')
test_2016_preds <- data.frame(llh = exp(test_2016_preds_linear), response = test_2016_data$Num_Votes, Record_id = test_2016_data$Record_id, model = 'testing 2016')

total_preds <- rbind(train_preds, test_preds,test_2017_preds)
View(test_preds)

# Predict the total number of votes per player

library(dplyr)
final_data_with_2017_pred <- inner_join(x = final_data, y = test_2017_preds, by = c("Record_id"))
final_data_with_2016_pred <- inner_join(x = final_data, y = test_2016_preds, by = c("Record_id"))
final_data_with_2017_pred <- final_data_with_2017_pred %>% arrange(Match_id, desc(llh)) %>% group_by(Match_id) %>% mutate(Expected_vote_rank = order(desc(llh)))
final_data_with_2016_pred <- final_data_with_2016_pred %>% arrange(Match_id, desc(llh)) %>% group_by(Match_id) %>% mutate(Expected_vote_rank = order(desc(llh)))
final_data_with_2016_pred$response <- as.numeric(final_data_with_2016_pred$response)

library(sqldf)
final_data_with_2017_pred <-
  sqldf('SELECT A.*
        , CASE WHEN Expected_vote_rank = 1 THEN 3
          WHEN Expected_vote_rank = 2 THEN 2
          WHEN Expected_vote_rank = 3 THEN 1
          ELSE 0 END AS Pred_votes
        FROM final_data_with_2017_pred A
        
        ')
final_data_with_2016_pred <-
  sqldf('SELECT A.*
        , CASE WHEN Expected_vote_rank = 1 THEN 3
        WHEN Expected_vote_rank = 2 THEN 2
        WHEN Expected_vote_rank = 3 THEN 1
        ELSE 0 END AS Pred_votes
        FROM final_data_with_2016_pred A
        
        ')

View(final_data_with_2017_pred)
write.csv(final_data_with_2017_pred,paste0(model_dir,"final_predictions_2017_by_game_",version_no,".csv"),row.names=FALSE)
write.csv(final_data_with_2016_pred,paste0(model_dir,"final_predictions_2016_by_game_",version_no,".csv"),row.names=FALSE)

three_votes <- filter(final_data_with_2017_pred,response == 3)
View(three_votes)
three_votes %>% group_by(Pred_votes) %>% summarize(n())

three_votes <- filter(final_data_with_2016_pred,response == 3)
View(three_votes)
three_votes %>% group_by(Pred_votes) %>% summarize(n())

# Make predictions for 2017 Brownlow

library(dplyr)
expected_votes <- final_data_with_2017_pred %>% group_by(Player, Position, Team) %>% summarise(Expected_votes = sum(llh))
predicted_votes <- final_data_with_2017_pred %>% group_by(Player, Position, Team) %>% summarise(Predicted_votes = sum(Pred_votes))
actual_votes <- final_data_with_2017_pred %>% group_by(Player, Position, Team) %>% summarise(Actual_votes = sum(response))
model_votes <- inner_join(x = expected_votes, y = predicted_votes, by = c("Player","Position","Team"))
final_votes <- inner_join(x = model_votes, y = actual_votes, by = c("Player","Position","Team"))
final_votes$Diff_votes <- final_votes$Predicted_votes - final_votes$Actual_votes
final_votes$Pred_BR_rank <- row_number(desc(final_votes$Predicted_votes))
final_votes$Actual_BR_rank <- row_number(desc(final_votes$Actual_votes))
View(final_votes)
write.csv(final_votes,paste0(model_dir,"final_predictions_2017_",version_no,".csv"),row.names=FALSE)

# Make predictions for 2016 Brownlow

expected_votes <- final_data_with_2016_pred %>% group_by(Player, Position,Team) %>% summarise(Expected_votes = sum(llh))
predicted_votes <- final_data_with_2016_pred %>% group_by(Player, Position,Team) %>% summarise(Predicted_votes = sum(Pred_votes))
actual_votes <- test_2016_data %>% group_by(Player, Position,Team) %>% summarise(Actual_votes = sum(Num_Votes))
model_votes <- inner_join(x = expected_votes, y = predicted_votes, by = c("Player","Position","Team"))
final_votes <- inner_join(x = model_votes, y = actual_votes, by = c("Player","Position","Team"))
final_votes$Diff_votes <- final_votes$Predicted_votes - final_votes$Actual_votes
final_votes$Pred_BR_rank <- row_number(desc(final_votes$Predicted_votes))
final_votes$Actual_BR_rank <- row_number(desc(final_votes$Actual_votes))
write.csv(final_votes,paste0(model_dir,"final_predictions_2016_",version_no,".csv"),row.names=FALSE)


# Plot prediction distribution
pred_dsn_plot <- ggplot(total_preds, aes(x=llh,fill=factor(response))) +
  geom_density(alpha=0.5) +
  #labs(title = "Classification distribution") +
  ylab("Density\n") +
  xlab("\nApplication Likelihood") +
  facet_wrap(~model)

ggsave(filename=paste0(model_dir,"Testing\\pred_dsn_plot.png"), plot = pred_dsn_plot)


# PvO charts

PVObyPred <- function (obs, pred, ylabel)
{
  responses <- cbind(obs, pred) # Combine the obs and predicted vectors
  colnames(responses)<- c("obs","pred")
  responses <- responses[complete.cases(responses[,1]) ,]
  responses<- responses[order(responses[,2]),] # Order in terms of the predicted values
  allbins <- seq(0.50000001,100.49999999,length=nrow(responses))
  bins <- round(allbins)
  responses.grouped2 <- aggregate(responses[,1:2],by=list(Percentile=bins),mean) #Find the average observed value in each percentile bucket
  average_observed <- mean(responses[,1])
  
  ########### Plot the PVO curve #################
  
  # Get the range for the x and y axis
  xrange <- range(bins)
  yrange <- range(responses.grouped2[,2],responses.grouped2[,3])
  
  plot(responses.grouped2$Percentile
       , responses.grouped2[,3]
       , type="l"
       , xlab= "Predicted Percentile"
       , ylab= ylabel
       , ylim = c(yrange[1],yrange[2])
       , main= "PvO analysis"
       , pch=1
       , col="blue"
       , lty=1
       , lwd=2
  )
  lines(responses.grouped2$Percentile
        , responses.grouped2[,2]
        , type="o"
        , pch=17
        , col="red"
        , lty=1
        , lwd=2)
  
  legend (x=xrange[1], y=yrange[2], c("Predicted","Observed"), lwd=c(1,2), col=c("blue","red"),lty=c(1,1))
}

#png(filename=paste0(model_dir,"Testing\\complete_PvO_plot.png"))
#par(mfrow=c(1,2))
#PVObyPred(train_preds$response,train_preds$llh,"Number of votes (training)")
#PVObyPred(test_preds$response,test_preds$llh,"Number of votes (testing)")
#dev.off()

png(filename=paste0(model_dir,"Testing\\testing_PvO_plot_,",version_no,".png"))
par(mfrow=c(1,1))
PVObyPred(test_preds$response,test_preds$llh,"Number of votes (testing)")
dev.off()

png(filename=paste0(model_dir,"Testing\\training_PvO_plot_",version_no,".png"))
par(mfrow=c(1,1))
PVObyPred(train_preds$response,train_preds$llh,"Number of votes (training)")
dev.off()


png(filename=paste0(model_dir,"Testing\\testing_2017_PvO_plot_",version_no,".png"))
par(mfrow=c(1,1))
PVObyPred(test_2017_preds$response,test_2017_preds$llh,"Number of votes (testing 2017)")
dev.off()

png(filename=paste0(model_dir,"Testing\\testing_2016_PvO_plot_",version_no,".png"))
par(mfrow=c(1,1))
PVObyPred(test_2016_preds$response,test_2016_preds$llh,"Number of votes (testing 2016)")
dev.off()

