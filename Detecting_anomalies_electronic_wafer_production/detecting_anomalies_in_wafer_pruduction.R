#Detecting anomalies in electronic wafer production

#Perform features engineering
#predicting anamalies in production




# Tools ----
library(tidyverse)
library(ppsr)
library(correlationfunnel)
library(tidymodels)
library(xgboost)
library(vip)
library(caret)
library(DMwR)
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
nzv_features_colnms<- colnames(train_df[,nzv_features])

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



data_model_binned<- train_df %>% binarize()

cor_fun_plot<- data_model_binned %>% correlate(target = Class__1) %>%
                                     plot_correlation_funnel() +
                                     geom_point(size = 2, color = 'aquamarine3')

cor_fun_plot$data<- head(cor_fun_plot$data, 40)

cor_fun_plot


recipe_spec <- recipe(Class ~ ., data = train_df) %>%
               step_dummy(all_nominal(), -Class)


fit_xgb <- workflow() %>%
           add_model(boost_tree(mode = "classification") %>% set_engine("xgboost")) %>%
           add_recipe(recipe_spec) %>%
           fit(train_df)

vip_plot<- fit_xgb$fit$fit$fit %>% vip()

vip_plot + theme_ipsum() + labs(title = 'Variable Importance Plots', subtitle = 'Importance extracted using XgBoost classification model')

nzv_pps_df<-data.frame(features = which(data_model_ppsr$data$x %in% colnames(train_df[,nzv_features])),
                       pps = data_model_ppsr$data$pps[which(data_model_ppsr$data$x %in% colnames(train_df[,nzv_features]))])



# Feature Eng ----

train_df<- train_df[,-nzv_features] #removing near zero o zero variance features

Validation_index<- createDataPartition(train_df$Class, p = 0.3, list = FALSE) #create validation set indx

validation_df<- train_df[Validation_index,]
train_df<- train_df[-Validation_index,]

train_df_smt <-  SMOTE(Class~., train_df, perc.over = 200) #balancing df using SMOTE technique

prop.table(table(train_df_smt$Class))

#Model creation ----

library(h2o)

h2o.init(nthreads = -1)

train_h20<- as.h2o(train_df_smt)
validation_h20<- as.h2o(validation_df)

x_h2o<- c(1:1522)
y_h2o<- 1523

###4.5.1 H2o auto machine learning 

h2o_aml <- h2o.automl(x = x_h2o, y = y_h2o,
                      training_frame = train_h20,
                      nfolds = 10,
                      max_models = 10,
                      max_runtime_secs_per_model = 600,
                      stopping_metric = 'AUC',
                      stopping_rounds = 4,
                      seed = 123)

h2o_aml_lb <- h2o_aml@leaderboard

h2o_aml_lb


h2o_aml_pred <- h2o.predict(h2o_aml@leader, validation_h20[,1:1522])

leader_perf<- h2o.performance(h2o_aml@leader, validation_h20)

leader_perf

leader_cf<- h2o.confusionMatrix(h2o_aml@leader)

leader_cf

plot(leader_perf, type = 'pr')

plot(leader_perf, type = 'roc')

h2o.shutdown(prompt = FALSE)
