#Detecting anomalies in electronic wafer production

#Perform features engineering
#predicting anamalies in production




# Tools ----
library(tidyverse)
library(visdat)
library(ppsr)
library(correlationfunnel)
library(tidymodels)
library(xgboost)
library(vip)
library(caret)
library(DMwR2)
library(ConfusionTableR)

library(hrbrthemes)
library(Cairo)
library(extrafont)
library(RColorBrewer)


# Data ----

train_df<- read.csv('Data/Train.csv')
test_df<- read.csv('Data/Test.csv')


# Sanity check ----
sum(is.na(train_df)) #NA check

nzv<- nearZeroVar(train_df, saveMetrics = TRUE, allowParallel = TRUE) #near zero variance check

nzv_features<- which(nzv$zeroVar == TRUE)

# Eval target variable ----
train_df$Class<- as.factor(train_df$Class)

train_df %>% ggplot(aes(Class, fill = Class)) +
             geom_bar(stat = 'count') +
             theme_ipsum() +
             labs(title = 'Target variable balance')




#Evaluating predictive power score, correlation, feature importance ----

data_model_ppsr<- train_df %>% visualize_pps(y = 'Class', do_parallel = TRUE, n_cores = 7)

data_model_ppsr_dummy<- data_model_ppsr

data_model_ppsr_dummy$data<- data_model_ppsr_dummy$data[order(data_model_ppsr_dummy$data$pps, decreasing = TRUE),]

data_model_ppsr_dummy$data<- head(data_model_ppsr_dummy$data, 20)

data_model_ppsr_dummy + theme_ipsum() + labs(title = 'Features prediction power score', subtitle = 'Model algorithm: tree, metric: F1')


nzv_pps_df<-data.frame(features = which(data_model_ppsr$data$x %in% colnames(train_df[,nzv_features])),
                       pps = data_model_ppsr$data$pps[which(data_model_ppsr$data$x %in% colnames(train_df[,nzv_features]))])
