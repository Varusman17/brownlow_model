#####################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different
#              data sources for modelling
#
#####################################################

# install.packages(c("RODBC","data.table","caret")))

rm(list = ls())
gc()

options(scipen=20)

training_prop <- 0.8

data_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\01 Data\\"
model_dir <- "C:\\Users\\Saurav\\Documents\\Brownlow\\02 Model\\"

version_no <- "v9"

# Read in factor data

factor_data <- readRDS(file=paste0(model_dir,"final_data_",version_no,".RDS"))
factor_data <- data.table(factor_data)

# Read factor list table

factorListTable <- read.csv(paste0(model_dir,"training_factor_list_",version_no,".csv"))
ResponseCol <- c(as.character(factorListTable[factorListTable$Response=="Y","Factor"]))
keyCols <- c(as.character(factorListTable[factorListTable$Key=="Y","Factor"]))
View(factorListTable)

# Grab additional variable information

var_names <- colnames(factor_data)
var_table <- data.frame(Factor = var_names)
var_table$row_ID <-1:nrow(var_table)
write.csv(var_table,paste0(model_dir,"var_table.csv"),row.names=FALSE)
var_table <- merge(var_table,factorListTable,by = "Factor",all.x = TRUE)
var_table <- var_table[(var_table$InputFactor == "Y")&(var_table$Factor != ResponseCol), ]
View(var_table)
# Drop remaining non-input factors

non_input_vars <- colnames(factor_data)[(!colnames(factor_data)%in%var_table$Factor)&(colnames(factor_data) != ResponseCol)&(colnames(factor_data) != 'Record_id')]
factor_data[, (non_input_vars) := NULL]
var_table <- var_table[!var_table$Factor%in%non_input_vars,]

library(dplyr)
var_table <- var_table %>% arrange(row_ID)
View(var_table)
# View(factor_data)

# Check that info has been provided for all input columns 
var_table[is.na(var_table$Type),]
# View(factor_data)
colnames(factor_data)

# Move the response column to the end of the dataset
factor_data <- factor_data%>%select(-Num_Votes,everything())
View(factor_data)
# specify monotonic column vector
monotone_neg_names <- var_table$Factor[var_table$Monotone==-1]
monotone_pos_names <- var_table$Factor[var_table$Monotone==1]
monotone_var_vec <- c(ifelse(colnames(factor_data)%in%monotone_neg_names,-1,ifelse(colnames(factor_data)%in%monotone_pos_names,1,0)))
monotone_var_vec <- ifelse(is.na(monotone_var_vec),0,monotone_var_vec)
monotone_var_vec <- monotone_var_vec[1:96]
monotone_var_vec

# print monotonic variables -- check log to make sure these are as expected
print(colnames(factor_data)[monotone_var_vec==1])
print(colnames(factor_data)[monotone_var_vec==-1])

# Convert factor variables

factorVars <- c(as.character(factorListTable[factorListTable$Type=="Factor","Factor"]))
remaining_factorVars <- factorVars[factorVars%in%colnames(factor_data)]

numericVars <- c(as.character(factorListTable[factorListTable$Type=="Numeric","Factor"]))
remaining_numericVars <- numericVars[numericVars%in%colnames(factor_data)]

factor_data[,(remaining_factorVars):=lapply(.SD, as.character),.SDcols=remaining_factorVars]
factor_data[,(remaining_factorVars):=lapply(.SD, as.factor),.SDcols=remaining_factorVars]
factor_data[,(remaining_numericVars):=lapply(.SD, as.numeric),.SDcols=remaining_numericVars]
View(factor_data)

# Create a dataset which contains all of the response observations and an equal number of non-responders
target <- c("1", "2","3")
response_set <- subset(factor_data, Num_Votes %in% target)
nrow(response_set)

# Check number of rows: nrow(response_set) -- 3,545

# Subset data to all non-responders
nonresponse_set <- subset(factor_data,Num_Votes == "0")
nrow(nonresponse_set)

# Check number of rows: nrow(response_set) -- 44,316

final_factor_data <- rbind(response_set,nonresponse_set)
View(final_factor_data)

# Save 2017 for testing

final_factor_data$Season <- substr(final_factor_data$Record_id,1,4)
test_2018_data <- filter(final_factor_data,Season == "2018")
test_2018_data <- within(test_2018_data,rm(Season))
saveRDS(test_2018_data, file=paste0(model_dir,"Testing//test_2018_data_",version_no,".RDS")) 

# Subset to 2015 to 2018 data
final_factor_data <- subset(final_factor_data,Season > 2014&Season < 2019)
final_factor_data <- within(final_factor_data,rm(Season))
View(final_factor_data)
# Create balanced train/test partitions

set.seed(123)
final_factor_data <- final_factor_data[sample(nrow(final_factor_data)),]

set.seed(1)
inTrain <- createDataPartition(final_factor_data[, ResponseCol, with = FALSE][[1]], p = training_prop, list = FALSE, groups = 4)
trainClass <- c(final_factor_data[inTrain[,1], c(ResponseCol), with = FALSE])
testClass <- c(final_factor_data[-inTrain[,1], c(ResponseCol), with = FALSE])

# Check that training/testing samples have similar class balance to population
prop.table(table(final_factor_data[, c(ResponseCol), with = FALSE]))
prop.table(table(trainClass))
prop.table(table(testClass))

# Create obs_table recording assignment of observations to test / train

trainDescr <- final_factor_data[inTrain[,1],]
testDescr <- final_factor_data[-inTrain[,1],]

# Pull out the BankCustomerID_INT associated with the training / test data

train_obs <- trainDescr[,(keyCols),with = FALSE]
test_obs <- testDescr[,(keyCols), with = FALSE]

tt_table <- rbind(data.frame(train_obs, Train = 1, Test = 0)
                  , data.frame(test_obs, Train = 0, Test = 1))

write.csv(tt_table,paste0(model_dir,"observation_status_",version_no,".csv"),row.names=FALSE)
write.table(tt_table,paste0(model_dir,"observation_status_",version_no,".txt"),row.names=FALSE, col.names = FALSE, sep="\t")

# Find zero-variance columns for removal from factor table

zv <- apply(trainDescr[,sapply(trainDescr,is.numeric),with=FALSE], 2, function(x) var(x, na.rm=TRUE)==0)
na_zv_cols <- c(names(zv)[is.na(zv)])
print(na_zv_cols)

# Remove NA zero-var columns

if (length(na_zv_cols)>0){
  trainDescr[, (na_zv_cols) := NULL]
  testDescr[, (na_zv_cols) := NULL]
}

# Save training/testing variables

saveRDS(trainDescr, file=paste0(model_dir,"Training//trainDescr_",version_no,".RDS")) 
saveRDS(testDescr, file=paste0(model_dir,"Testing//testDescr_",version_no,".RDS")) 
saveRDS(trainClass, file=paste0(model_dir,"Training//trainClass_",version_no,".RDS")) 
saveRDS(testClass, file=paste0(model_dir,"Testing//testClass_",version_no,".RDS")) 


