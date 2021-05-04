
#Patients prescription churn case study ----

#Perform a complete data analysis
#Create a churn prediction model

#1.0 Library & Tools ----
library(tidyverse)
library(lubridate)
library(skimr)
library(visdat)
library(RColorBrewer)
library(ppsr)
library(correlationfunnel)
library(tidymodels)
library(xgboost)
library(vip)
library(caret)


#2.0 Data ----

##2.1 Importing ----

purchase_data_Apr_Sep<- read.csv('Data/purchase_data_bill_level_sept.csv')
purchase_data_test_Oct<- read.csv('Data/out_of_time_test_oct.csv')


#Sanity check

skim_without_charts(purchase_data_Apr_Sep)
skim_without_charts(purchase_data_test_Oct)

##2.2 Cleaning  ----

purchase_data_Apr_Sep$created_at_bill<- ymd_hms(purchase_data_Apr_Sep$created_at_bill)



#3.0 EDA ----

n_distinct(purchase_data_Apr_Sep$bill_ref_id)
mean(purchase_data_Apr_Sep$total_spend_bill)

## 3.1 Sales analysis ----
purchase_data_Apr_Sep %>% ggplot(aes(month(created_at_bill, label = TRUE), fill = month(created_at_bill, label = TRUE))) +
                          geom_bar(stat = 'count') +
                          scale_fill_brewer(palette = 'Set3') +
                          theme_light() +
                          theme(legend.position = 'none') +
                          labs(title = 'Number of sales by months') +
                          xlab('Months') +
                          ylab('Number of sales')

purchase_data_Apr_Sep %>% group_by(week_day = wday(created_at_bill,label = TRUE)) %>%
                          summarise(Number_of_sales = n()) %>%
                          ggplot(aes(week_day, Number_of_sales, fill = week_day)) +
                          geom_bar(stat = 'identity') +
                          scale_fill_brewer(palette = 'Set3') +
                          theme_minimal() +
                          theme(legend.position = 'none') +
                          labs(title = 'Number of sales by day of the week') +
                          xlab('Day of the week') +
                          ylab('Number of sales')

purchase_data_Apr_Sep %>% mutate(am_pm = ifelse(am(created_at_bill) == TRUE, 'am', 'pm')) %>%
                          group_by(am_pm) %>%
                          summarise(Number_of_sales = n()) %>%
                          ggplot(aes(am_pm, Number_of_sales, fill = am_pm)) +
                          geom_bar(stat = 'identity') +
                          scale_fill_brewer(palette = 'Set1') +
                          theme_light() +
                          theme(legend.position = 'none') +
                          xlab('Part of the day') +
                          ylab('Number of sales') +
                          labs(title = 'Number of sales by part of the day')
                          


purchase_data_Apr_Sep %>% mutate(created_at_bill = hour(round_date(created_at_bill, unit = 'hour'))) %>%
                          group_by(created_at_bill) %>%
                          summarise(Number_of_sales = n()) %>%
                          ggplot(aes(created_at_bill, Number_of_sales)) +
                          geom_bar(stat = 'identity', fill = 'darkorange2') +
                          theme_light() +
                          theme(legend.position = 'none') +
                          xlab('Hours of the day') +
                          ylab('Number of sales') +
                          labs(title = 'Number of sales by hours of the day')

## 3.2 Store analysis ----
purchase_data_Apr_Sep %>% group_by(store_ref_id) %>%
                          summarise(Number_of_sales = n()) %>%
                          top_n(10, wt = Number_of_sales) %>%
                          arrange(Number_of_sales) %>% 
                          ggplot(aes(as.factor(store_ref_id), Number_of_sales , fill = as.factor(store_ref_id))) +
                          geom_bar(stat = 'identity') +
                          scale_fill_brewer(palette = 'Set3') +
                          theme_light() +
                          theme(legend.position = 'none') +
                          xlab('Store Id') +
                          ylab('Number of sales') +
                          labs(title = 'Best 10 stores by number of sales')


## 3.3 Customer analysis ----
purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
                          summarise(first_bill = min(month(created_at_bill, label = TRUE))) %>%
                          ggplot(aes(first_bill, fill = first_bill)) +
                          geom_bar(stat = 'count') +
                          scale_fill_brewer(palette = 'Set2') +
                          theme_light() +
                          theme(legend.position = 'none') +
                          xlab('New customers') +
                          labs(title = 'New customer per month')

different_store_visit<- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
                                                  summarise(store_visited = n_distinct(store_ref_id)) %>%
                                                  arrange(desc(store_visited))

  
purchase_data_Apr_Sep %>% ggplot(aes(payment_method, fill = payment_method)) +
                          geom_bar(stat = 'count') +
                          scale_fill_brewer(palette = 'Set2') +
                          theme_minimal() +
                          theme(legend.position = 'none') +
                          xlab('Payment method') +
                          labs(title = 'Payment method frequency')

purchase_data_Apr_Sep %>% group_by(payment_method) %>%
                          summarise(avg_payment = mean(total_spend_bill)) %>%
                          arrange(desc(avg_payment)) %>%
                          ggplot(aes(payment_method, avg_payment, fill = payment_method)) +
                          geom_bar(stat = 'identity') +
                          scale_fill_brewer(palette = 'Set2') +
                          theme_minimal() +
                          theme(legend.position = 'none') +
                          xlab('Payment method') +
                          ylab('Average transaction') +
                          labs(title = 'Average transaction per payment method')

## 3.4 Doctor analysis ----                          
number_of_doctors<- n_distinct(purchase_data_Apr_Sep$doctor_ref_id)

doctor_drugs_cor<- purchase_data_Apr_Sep %>% group_by(doctor_ref_id) %>%
                                             summarise(total_drugs_prescibed = sum(total_quantity_bill),
                                                       ethical_drugs = sum(quantity_ethical),
                                                       generic_drugs = sum(quantity_generic),
                                                       surgical_drugs = sum(quantity_surgical),
                                                       ayurvedic_drugs = sum(quantity_ayurvedic),
                                                       general_drugs = sum(quantity_general),
                                                       otc_drugs = sum(quantity_otc),
                                                       chronic_drugs = sum(quantity_chronic),
                                                       acute_drugs = sum(quantity_acute),
                                                       h1_drugs = sum(quantity_h1))




#4.0 Prediction model ----

##4.1 Create data for training ----

dummy<- purchase_data_Apr_Sep %>% filter(month(created_at_bill) < 9) %>%
                                  group_by(customer_ref_id, month = month(created_at_bill)) %>%
                                  summarise(n= n_distinct(bill_ref_id))

dummy<- dummy %>% group_by(customer_ref_id) %>%
                  summarise(avg_visit = mean(n))

data_model_train<- purchase_data_Apr_Sep %>% filter(month(created_at_bill) < 9) %>%
                                             group_by(customer_ref_id) %>%
                                             summarise(avg_spent =  mean(total_spend_bill),
                                                       avg_ethical_drug = mean(quantity_ethical),
                                                       avg_generic_drugs = mean(quantity_generic),
                                                       avg_surgical_drugs = mean(quantity_surgical),
                                                       avg_ayurvedic_drugs = mean(quantity_ayurvedic),
                                                       avg_general_drugs = mean(quantity_general),
                                                       avg_otc_drugs = mean(quantity_otc),
                                                       avg_chronic_drugs = mean(quantity_chronic),
                                                       avg_acute_drugs = mean(quantity_acute),
                                                       avg_h1_drugs = mean(quantity_h1),
                                                       avg_drugs_per_bill = mean(total_quantity_bill),
                                                       avg_type_of_drugs_per_bill = mean(num_drugs_bill))

data_model_train$avg_visit<- dummy$avg_visit

dummy<- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
                                  summarise(sep_bill = sum(ifelse(month(created_at_bill) == 9, 1, 0)))

dummy$churn_in_sep<- ifelse(dummy$sep_bill == 0, 1, 0)

data_model_train<- left_join(data_model_train, dummy, by = "customer_ref_id")
data_model_train$churn_in_sep<- as.factor(data_model_train$churn_in_sep)

data_model_train<- data_model_train[,-which(colnames(data_model_train) == 'sep_bill')]

prop.table(table(data_model_train$churn_in_sep))

data_model<- data_model_train

##4.2 Create data for validation ----
validation_index<- createDataPartition(data_model_train$churn_in_sep, p = 0.3, list = FALSE)
data_model_validation<- data_model_train[validation_index,]
data_model_train<- data_model_train[-validation_index,]
##4.3 Create data for test ---- 

data_model_test<- purchase_data_Apr_Sep %>% group_by(customer_ref_id) %>%
                                            summarise(avg_spent =  mean(total_spend_bill),
                                                      avg_ethical_drug = mean(quantity_ethical),
                                                      avg_generic_drugs = mean(quantity_generic),
                                                      avg_surgical_drugs = mean(quantity_surgical),
                                                      avg_ayurvedic_drugs = mean(quantity_ayurvedic),
                                                      avg_general_drugs = mean(quantity_general),
                                                      avg_otc_drugs = mean(quantity_otc),
                                                      avg_chronic_drugs = mean(quantity_chronic),
                                                      avg_acute_drugs = mean(quantity_acute),
                                                      avg_h1_drugs = mean(quantity_h1),
                                                      avg_drugs_per_bill = mean(total_quantity_bill),
                                                      avg_type_of_drugs_per_bill = mean(num_drugs_bill))

dummy<- purchase_data_Apr_Sep %>% group_by(customer_ref_id, month = month(created_at_bill)) %>%
                                  summarise(n= n_distinct(bill_ref_id))

dummy<- dummy %>% group_by(customer_ref_id) %>%
                  summarise(avg_visit = mean(n))

data_model_test$avg_visit<- dummy$avg_visit

data_model_test$churn_in_oct<- purchase_data_test_Oct$oct_purchase_flag


##4.4 Evaluating predictive power score, correlation, feature importance ---- 
data_model_ppsr<- data_model %>% select(-customer_ref_id) %>%
                                 visualize_pps(do_parallel = TRUE)

data_model_ppsr + theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_model_binned<- data_model %>% select(-customer_ref_id) %>%
                                   binarize()

data_model_binned %>% correlate(target = churn_in_sep__1) %>%
                      plot_correlation_funnel() +
                      geom_point(size = 3, color = 'aquamarine3')

recipe_spec <- recipe(churn_in_sep ~ ., data = data_model) %>%
               step_rm(customer_ref_id) %>%
               step_dummy(all_nominal(), -churn_in_sep)

recipe_spec %>% prep() %>% juice() %>% glimpse()

fit_xgb <- workflow() %>%
           add_model(boost_tree(mode = "classification") %>% set_engine("xgboost")) %>%
           add_recipe(recipe_spec) %>%
           fit(data_model)

fit_xgb$fit$fit$fit %>% vip()

##4.5 Create model ----
library(h2o)

h2o.init(nthreads = -1)

train_h20<- as.h2o(data_model_train)
validation_h20<- as.h2o(data_model_validation)
test_h2o<- as.h2o(data_model_test)

x_h2o<- c(1:14)
y_h2o<- 15

###4.5.1 H2o auto machine learning 

h2o_aml <- h2o.automl(x = x_h2o, y = y_h2o,
                      training_frame = train_h20,
                      validation_frame = validation_h20,
                      nfolds = 10,
                      max_models = 10,
                      max_runtime_secs_per_model = 600,
                      stopping_metric = 'AUC',
                      stopping_rounds = 4,
                      seed = 123)

h2o_aml_lb <- h2o_aml@leaderboard

h2o_aml_lb

h2o_aml_pred <- h2o.predict(h2o_aml@leader, validation_h20[,1:14])

h2o.explain(h2o_aml@leader, validation_h20)

leader_perf<- h2o.performance(h2o_aml@leader, validation_h20)

leader_perf

h2o.confusionMatrix(h2o_aml@leader)

plot(leader_perf, type = 'roc')

plot(leader_perf, type = 'pr')

h2o.shutdown(prompt = FALSE)