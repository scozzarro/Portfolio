#######################################################################
# FPS Machine learning project
# 24 May 2020
# Gabrielmaria Scozzarro
#    This project born from a suggestion from my company partner who is 
#    a teacher at the Italian VideoGames Accademy as I need a project for my
#    exam during HardvardX: Capstone.
#    The final aim is to built a machine learning algorythm to predict the 
#    winners of a game starting from the players game stats.
#    It's composed by 2 parts: the first one is Exploration and Data Analisys (EDA)
#    the second part is the the creation of the machine learning algorythm,
#    winners prediction and calculate MAE as KPI.
#######################################################################

#######################################################################
#PART 2: Prediction algorithm usign machine learning
#######################################################################

#####################################################
# load data sets (train_V2, test_V2) and libraries (tidyverse,data.table,corrplot)

#####################################################

#Load packages.

library(tidyverse)
library(data.table)
library(caret)
library(h2o)

#options and set up
conn_h2o<- h2o.init(nthreads = 4)

#Load Data if needed

train_set <- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/train_V2.csv")
test_set <- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/test_V2.csv")

#Remove NAs if needed

train_set$winPlacePerc[is.na(train_set$winPlacePerc)]<- 0


#Prepare Data for the model


#To fully take in consideration the team work inside the game,
#the exploration of the map even when the game time is short and 
#the boost and perks found in the game we can create 3 different features 
#that take all of that and insert them in the train data set

FPS <- function(x){
  x <- x %>%
    mutate(teamwork = (assists*0.2)+(revives*0.8),
           avgDistPerMinute = (walkDistance+rideDistance+swimDistance)/(matchDuration/60),
           itemsFound = (weaponsAcquired+heals+boosts))
  return(x)
}

train_set <- FPS(train_set)

#Drop less important feature and games

less_imp <- c("vehicleDestroys","roadKills ","teamKills", "maxPlace")

train_set <- train_set[, -which(names(train_set) %in% less_imp)]

#Remove games with numGroup == 1 

train_set <- train_set %>% 
  filter(numGroups > 1)

test_set <- test_set %>% 
  filter(numGroups > 1)

#Splitting Data in train , validation , test

val_index<- createDataPartition(train_set$winPlacePerc, p = 0.2, list = FALSE)
validation<- train_set[val_index,]
train<- train_set[-val_index,]


#since the target value can be any value from 0 to 1 we can try
#a liner model to predict it 


glm_fit <- train(winPlacePerc ~ walkDistance + rideDistance + 
                               swimDistance + avgDistPerMinute +
                               kills + matchDuration + 
                               assists + boosts + 
                               killPoints + winPoints +
                               numGroups + headshotKills +
                               teamwork + itemsFound ,
                               method = "glm" , data = train)
 
#Examine the glm model 
glm_fit
summary(glm_fit)
glm_fit$finalModel
varImp(glm_fit$finalModel)

#Use the model to predict the target value

winPerc_glm_yhat <- predict(glm_fit, validation)

#calculate MAE on the prediction
postResample(pred = winPerc_glm_yhat, obs = validation$winPlacePerc)

#Try a more complex model to obtain a better result using
#a deep learning algorithm.

#preparing data for deep learning

train.h2o <- as.h2o(train)
sampled_train <- as.h2o(train[1:10000,]) #For speed we create a 10K rows train data set for model tuning
sampled_valid <- as.h2o(train[10001:20000,])
validation.h2o <- as.h2o(validation)
test.h2o <- as.h2o(test_set)

colnames(train.h2o)

target_val <- 26
features <- c(4:25,27:29)

#Find tuning parameter for DL model

# Selecting hyper-parameters:

hyper_params <- list(activation = c("Rectifier","Tanh"), 
                     hidden = list(c(50, 50, 50), c(200, 200), c(100, 100), c(20,20)), 
                     input_dropout_ratio = c(0, 0.05),
                     l1=seq(0,1e-4,1e-6),
                     l2=seq(0,1e-4,1e-6))

# Selecting optimal model search criteria. Search will stop once top 5 models are within 1% of each other:

search_criteria = list(strategy = "RandomDiscrete", 
                       max_runtime_secs = 600, 
                       max_models = 200, 
                       seed=1234567, 
                       stopping_rounds=10, 
                       stopping_tolerance=1e-2)

#Perform a random hyper parameters search

dl_random_grid <- h2o.grid(
  algorithm="deeplearning",
  grid_id = "dl_grid_random",
  training_frame=sampled_train,
  validation_frame=sampled_valid, 
  x=features, 
  y=target_val,
  epochs=5,
  stopping_metric="MAE",
  stopping_tolerance=1e-2,        ## stop when MAE does not improve by >=1% for 2 scoring events
  stopping_rounds=3,
  max_w2=10,                      ## can help improve stability for Rectifier
  hyper_params = hyper_params,
  search_criteria = search_criteria
)                

#get tuning results

grid <- h2o.getGrid("dl_grid_random",sort_by="MAE", decreasing=FALSE);grid

best_model <- h2o.getModel(grid@model_ids[[1]]);best_model #learn more about the best model 
best_params <- best_model@allparameters;best_params

#Run the training with the entire data set and best params 
dl_model <- h2o.deeplearning(model_id = "dl_all_data_1",
                             training_frame=train.h2o,
                             x=features, 
                             y=target_val,
                             hidden = c(200,200),
                             activation = "Rectifier",
                             stopping_metric="MAE",
                             stopping_tolerance=1e-2,
                             score_validation_samples=10000, # downsample validation set for faster scoring
                             score_duty_cycle=0.025,         # don't score more than 2.5% of the wall time
                             seed = 1234567,
                             epochs = 10,
                             l1 = 7.4e-05,
                             l2 = 5.8e-05,
                             max_w2 = 10,)

#Examine the deep learning model
dl_model
head(as.data.frame(h2o.varimp(dl_model)))
plot(dl_model)


#generate prediction

dl_result <-as.data.frame(h2o.predict(dl_model, validation.h2o))

#calcute MAE on results

MAE(dl_result$predict,validation$winPlacePerc)
