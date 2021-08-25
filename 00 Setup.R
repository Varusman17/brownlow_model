####################################################################################
#
# Author: Saurav Acharya
# Project: AFL Brownlow prediction
# Description: Bring together all of the different data sources for modelling
# Update 2021-08-23: New source of data have solved for data cleaning steps
# 
####################################################################################

# install.packages(c("devtools","reshape2","tidyr"))
# require(devtools)
# install_github("Displayr/flipTime")

list_of_packages <- c("readxl"
                      , "fitzRoy"
                      , "reshape2"
                      , "tidyverse"
                      , "rvest"
                      , "flipTime"
                      , "data.table"
                      , "RODBC"
                      , "caret"
                      , "pdp"
                      , "gbm"
                      , "ggplot2"
                      , "ROCR"
                      , "sqldf"
                      , "eeptools"
                      )

new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
rm('new_packages')    
lapply(list_of_packages, library, character.only = TRUE)
