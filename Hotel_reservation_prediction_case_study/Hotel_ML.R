library(tidyverse)
library(caret)
library(doParallel)
library(ROSE)
library(h2o)
library(RColorBrewer)




#=================================
#  Option
#=================================

theme_set(theme_minimal())


#=================================
#  Data preparation 
#=================================

data_ml<- data

`%ni%` <- Negate(`%in%`)

lessimp<- c("ReservationStatus", "ReservationStatusDate")
data_ml<- subset(data_ml, select = names(data_ml) %ni% lessimp) #drop non important feature

summary(data_ml)

prop.table(table(data_ml$IsCanceled)) #moderate imbalance data set

validation_indexex<- createDataPartition(data_ml$IsCanceled, p = 0.2, list = FALSE)
train_set<- data_ml[-validation_indexex,]
validation_set<- data_ml[validation_indexex,]



#=================================
#  Test data preparation 
#=================================
test<- read.csv("H2full.csv")

test$IsCanceled<- as.factor(test$IsCanceled)
test$ArrivalDateYear<- as.factor(test$ArrivalDateYear)
test$ArrivalDateWeekNumber<- as.factor(test$ArrivalDateWeekNumber)
test$ArrivalDateDayOfMonth<- as.factor(test$ArrivalDateDayOfMonth)
test$ArrivalDateMonth<- as.factor(test$ArrivalDateMonth)
test$Meal<- as.factor(test$Meal)
test$Country<- as.factor(test$Country)
test$MarketSegment<- as.factor(test$MarketSegment)
test$DistributionChannel<- as.factor(test$DistributionChannel)
test$IsRepeatedGuest<- as.logical(test$IsRepeatedGuest)
test$PreviousBookingsNotCanceled<- as.logical(test$PreviousBookingsNotCanceled)
test$ReservedRoomType<- as.factor(test$ReservedRoomType)
test$AssignedRoomType<- as.factor(test$AssignedRoomType)
test$DepositType<- as.factor(test$DepositType)
test$Agent<- as.factor(test$Agent)
test$Company<- as.factor(test$Company)
test$CustomerType<- as.factor(test$CustomerType)
test$RequiredCarParkingSpaces<- as.logical(test$RequiredCarParkingSpaces)
test$ReservationStatus<- as.factor(test$ReservationStatus)
test$ReservationStatusDate<- as.Date(test$ReservationStatusDate)

summary(test)

test$Deltaroomtype<- ifelse(as.character(test$ReservedRoomType) == as.character(test$AssignedRoomType), 1, 0)

test$Deltaroomtype<- as.logical(test$Deltaroomtype)

summary(test$Deltaroomtype)

test_ml<- test

test_ml<- subset(test_ml, select = names(test_ml) %ni% lessimp) #drop non important feature

#=================================
#  Initialize H2O 
#=================================

h2o.init(nthreads = -1, max_mem_size = "16g")

train_h2o<- as.h2o(train_set)
validate_h20<- as.h2o(validation_set)
test_h2o<- as.h2o(test_ml)


#=================================
#  h2o RF #1 
#=================================

h2o_rf_mod_1<- h2o.randomForest(model_id = "h2o_rf_1",
                                training_frame = train_h2o,
                                validation_frame = validate_h20,
                                x = 2:30,
                                y = 1,
                                nfolds = 10,
                                ntrees = 200,
                                stopping_rounds = 5,
                                score_each_iteration = TRUE,
                                balance_classes = TRUE)



results_cross_validation <- function(h2o_model) {
  h2o_model@model$cross_validation_metrics_summary %>% 
    as.data.frame() %>% 
    select(-mean, -sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(as.character) %>% 
    mutate_all(as.numeric) %>% 
    select(Accuracy = accuracy, 
           AUC = auc, 
           Precision = precision, 
           Specificity = specificity, 
           Recall = recall, 
           Logloss = logloss,
           F1 = f1) %>% 
    return()
}

plot_results <- function(df_results) {
  df_results %>% 
    gather(Metrics, Values) %>% 
    ggplot(aes(Metrics, Values, fill = Metrics, color = Metrics)) +
    geom_boxplot(alpha = 0.3, show.legend = FALSE) + 
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +    
    scale_y_continuous(labels = scales::percent) + 
    facet_wrap(~ Metrics, scales = "free") + 
    labs(title = "Model Performance by Some Criteria Selected", y = NULL)
}

cv_result<- results_cross_validation(h2o_rf_mod_1);cv_result

plot_results(cv_result)

h2o.varimp_plot(h2o_rf_mod_1)

#=================================
#  Random Discrete Grid Search
#=================================

hyper_params <- list(mtries = seq(2,8, by = 2),
                     ntrees = seq(50,250, by = 50),
                     balance_classes = c(TRUE, FALSE))

sapply(hyper_params, length) %>% prod()

search_criteria<- list(strategy = "RandomDiscrete",
                       stopping_rounds = 10,
                       stopping_metric = "AUCPR",
                       stopping_tolerance = 1e-2,
                       max_runtime_secs = 30*60)


system.time(rf_random_grid <- h2o.grid(algorithm = "randomForest",
                                          grid_id = "rf_grid_random",
                                          training_frame = train_h2o,
                                          validation_frame = validate_h20,
                                          x = 2:30,
                                          y = 1,
                                          nfolds = 10,
                                          hyper_params = hyper_params,
                                          search_criteria = search_criteria))


#=================================
#  Random Discrete Grid Search performance
#=================================
F1_sorted_grid <- h2o.getGrid("rf_grid_random", sort_by = "F1", decreasing = TRUE); F1_sorted_grid
Accuracy_sorted_grid<- h2o.getGrid("rf_grid_random", sort_by = "Accuracy", decreasing = TRUE); Accuracy_sorted_grid

F1_best_model<- best_model<- h2o.getModel(F1_sorted_grid@model_ids[[1]]); F1_best_model #get and learn more about the best model according F1 score
Accuracy_best_model<- h2o.getModel(Accuracy_sorted_grid@model_ids[[1]]); Accuracy_best_model #get and learn more about the best model according Accuracy

plot_model<- function(h2o_model) {
  metric_df<- data.frame(metric = h2o_model@model$validation_metrics@metrics$max_criteria_and_metric_scores$metric,
                            value = h2o_model@model$validation_metrics@metrics$max_criteria_and_metric_scores$value)
  metric_df[1:10,] %>%
    ggplot(aes(x = metric, y = value)) +
    geom_segment( aes(x=metric, xend=metric, y=0, yend= value), color="grey") +
    geom_point( aes(color = metric), size=4) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "Model Performance by Criteria Selected", y = NULL)
} #function to plot model main metrics


plot_model(Accuracy_best_model) +
  labs(subtitle = "Model: Random Forest (ID:rf_grid_random), best accuracy")
plot_model(F1_best_model) +
  labs(subtitle = "Model: Random Forest (ID:rf_grid_random), best F1 score")



#=================================
#  Prediction using Accuracy best model
#=================================

pred_1 <- h2o.predict(object = Accuracy_best_model, newdata = test_h2o); pred_1
summary(pred_1, exact_quantiles = TRUE)

pred_1_df<- as.data.frame(pred_1)

sum(test_ml$IsCanceled == pred_1_df$predict)

sum(test_ml$IsCanceled == pred_1_df$predict)/nrow(test_ml)


#=================================
#  Balance data with ROSE
#=================================

data_ml$IsRepeatedGuest<- as.factor(data_ml$IsRepeatedGuest)
data_ml$PreviousBookingsNotCanceled<- as.factor(data_ml$PreviousBookingsNotCanceled)
data_ml$RequiredCarParkingSpaces<- as.factor(data_ml$RequiredCarParkingSpaces)
data_ml$Deltaroomtype<- as.factor(data_ml$Deltaroomtype)

data_ml_rose <- ROSE(IsCanceled ~ ., data = data_ml, seed = 1)$data

summary(data_ml_rose)

prop.table(table(data_ml_rose$IsCanceled)) #moderate imbalance data set


data_ml_rose$IsRepeatedGuest<- as.logical(data_ml_rose$IsRepeatedGuest)
data_ml_rose$PreviousBookingsNotCanceled<- as.logical(data_ml_rose$PreviousBookingsNotCanceled)
data_ml_rose$Deltaroomtype<- as.logical(data_ml_rose$Deltaroomtype)

summary(data_ml_rose)

#=================================
#  Prepare data set for ml
#=================================

validation_indexes_rose<- createDataPartition(data_ml_rose$IsCanceled, p = 0.2, list = FALSE)
train_set_rose<- data_ml_rose[-validation_indexes_rose,]
validation_set_rose<- data_ml[validation_indexes_rose,]

train_h2o_rose<- as.h2o(train_set_rose)
validate_h20_rose<- as.h2o(validation_set_rose)

#=================================
#  Random Discrete Grid Search 2
#=================================

hyper_params_2 <- list(mtries = seq(2,8, by = 2),
                     ntrees = seq(50,250, by = 50))

rf_random_grid_2 <- h2o.grid(algorithm = "randomForest",
                           grid_id = "rf_grid_random_3",
                           training_frame = train_h2o_rose,
                           validation_frame = validate_h20_rose,
                           x = 2:30,
                           y = 1,
                           nfolds = 10,
                           hyper_params = hyper_params_2,
                           search_criteria = search_criteria)



#=================================
#  Random Discrete Grid 2 Search performance
#=================================

F1_sorted_grid_rose <- h2o.getGrid("rf_grid_random_3", sort_by = "F1", decreasing = TRUE); F1_sorted_grid_rose
Accuracy_sorted_grid_rose<- h2o.getGrid("rf_grid_random_3", sort_by = "Accuracy", decreasing = TRUE); Accuracy_sorted_grid_rose


F1_best_model_rose<- h2o.getModel(F1_sorted_grid_rose@model_ids[[1]]); F1_best_model_rose #get and learn more about the best model according F1 score

Accuracy_best_model_rose<- h2o.getModel(Accuracy_sorted_grid_rose@model_ids[[1]]); Accuracy_best_model_rose #get and learn more about the best model according Accuracy


plot_model(Accuracy_best_model_rose) +
  labs(subtitle = "Model: Random Forest (ID:rf_grid_random_3), best accuracy")
  
plot_model(F1_best_model_rose) +
  labs(subtitle = "Model: Random Forest (ID:rf_grid_random_3), best F1 score")


#=================================
#  Prediction using Accuracy best model from rf_grid_random_3
#=================================

pred_2 <- h2o.predict(object = Accuracy_best_model_rose, newdata = test_h2o); pred_2
summary(pred_2, exact_quantiles = TRUE)

pred_2_df<- as.data.frame(pred_2)

sum(test_ml$IsCanceled == pred_2_df$predict)

sum(test_ml$IsCanceled == pred_2_df$predict)/nrow(test_ml)


h2o.shutdown(prompt = FALSE)