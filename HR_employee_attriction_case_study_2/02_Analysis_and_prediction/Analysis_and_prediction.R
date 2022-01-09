#Libraries ----
library(tidyverse)
library(tidyquant)
library(readxl)
library(forcats)
library(stringr)
library(skimr)
library(fs)
library(GGally)
library()
library(cowplot)
library(recipes)
library(h2o)
library(caret)
library(lime)

#Load Data----
path_train<- "HR_employee_attriction_case_study_2/00_Data/telco_train.xlsx"
path_test<- "HR_employee_attriction_case_study_2/00_Data/telco_test.xlsx"
path_data_definition<- "HR_employee_attriction_case_study_2/00_Data/telco_data_definitions.xlsx"

train_raw_tbl<- read_excel(path_train, sheet = 1)
test_raw_tbl<- read_excel(path_test, sheet = 1)
definition_raw_tbl<- read_excel(path_data_definition, col_names = FALSE, sheet = 1)

#Data join ----
full_raw_tbl<- merge(train_raw_tbl, test_raw_tbl, all = TRUE)

#Data Subset ----
dept_job_role_tbl<- full_raw_tbl %>% select(EmployeeNumber, Department, JobRole, PerformanceRating, Attrition)

#Business Science Problem Framework ----

##1A. View Business as machine ----
#understanding Department and job role
#Define objectives: Retain high performers
#Assess outcomes: TBD

dept_job_role_tbl %>% 
  group_by(Attrition) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(pct = n/sum(n))%>%
  ggplot(aes(Attrition, pct, fill = Attrition)) +
  geom_bar(stat = 'identity') +
  theme_tq() +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = 'Attrition presence in the company',
       subtitle = 'Is 16% attrition a bad thing?',
       y = 'Percentage')

#is 16% attrition a bad thing ?

##1B. Understand the drivers ----
#Investigate objectives: 16% attrition 
#Synthesize outcomes:
#Hypothesize drivers: Job role and departments

dept_job_role_tbl %>% group_by(Department, Attrition) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(Department) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(Department, pct, fill = Attrition)) +
  geom_bar(stat = 'identity', position = position_dodge2(preserve = "single")) +
  theme_tq() +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = 'Department with attrition',
       y = 'Percentage',
       x = '')

dept_job_role_tbl %>% group_by(JobRole, Department, Attrition) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(JobRole, Department) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes") %>%
  ggplot(aes(reorder(JobRole, pct), pct, fill = Department)) +
  geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single')) +
  theme_tq() +
  scale_fill_tq() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Attrition by Job Role and Department",
       x = 'Job Role',
       y = 'Percentage') +
  coord_flip()

##1C. Measure the drivers ----

#Develop KPI: industry KPI: 8.8% (find on google https://www.zenefits.com/workest/your-company-healthy-employee-retention-rate/)

dept_job_role_tbl %>% group_by(JobRole, Department, Attrition) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(JobRole, Department) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(above_industry_avg = case_when(pct > 0.088 ~ "Yes", 
                                        TRUE ~ "No")) %>%
  mutate(JobRole_and_dep = paste(JobRole, Department, sep = ' - ')) %>%
  ggplot(aes(pct, reorder(JobRole_and_dep, pct), fill = above_industry_avg)) +
  geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single')) +
  geom_vline(xintercept = 0.088, linetype = 'dashed', color = 'black', size = 1) +
  annotate(geom = 'text', label = 'Industry Average', x = 0.088, y = 3, angle = 90, vjust = 1.2) +
  theme_tq() +
  scale_fill_tq() +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = 'Job Role and Department above the industry average attrition percentage',
       subtitle = 'According 2016 Compensation Force Study, 8.8% is the average attrition for utilities company',
       y = 'Job Role and Department',
       x = 'Percentage',
       fill = 'Above industry average')

##1D. Uncover problems and opportunities ----

calculate_attrition_cost<- function( n = 1,
                                     salary = 80000,
                                     separation_cost = 500,
                                     vacancy_cost = 10000,
                                     acquisition_cost = 4900,
                                     placement_cost = 3500,
                                     net_revenue_per_employee = 250000,
                                     wordays_per_year = 240,
                                     workdays_position_open = 40,
                                     workdays_onboarding = 60,
                                     onboarding_efficiency = 0.5) {
  
  #Direct cost
  direct_cost<- sum(separation_cost,vacancy_cost,acquisition_cost,placement_cost)
  
  #Lost productivity cost
  productivity_cost<- net_revenue_per_employee/wordays_per_year*
    (workdays_position_open + workdays_onboarding*onboarding_efficiency)
  
  #Saving on salary
  salary_reduction<- salary/wordays_per_year * workdays_position_open
  
  #estimated turnover per employee
  cost_per_employee<- direct_cost + productivity_cost - salary_reduction
  
  #Total cost of employee turnover
  total_cost<- n * cost_per_employee
  
  return(total_cost)
}

source(file = 'HR_employee_attriction_case_study_2/01_Script_and_function/assess_attrition.R')

##Calculate turnover cost per job role ----

dept_job_role_tbl %>% group_by(JobRole, Department, Attrition) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(JobRole, Department) %>%
  mutate(pct = n/sum(n)) %>%
  ungroup() %>%
  filter(Attrition %in% "Yes") %>%
  arrange(desc(pct)) %>%
  mutate(above_industry_avg = case_when(pct > 0.088 ~ "Yes", TRUE ~ "No")) %>%
  mutate(cost_of_attrition = calculate_attrition_cost( n = n , salary = 80000)) %>%
  ggplot(aes(reorder(JobRole, cost_of_attrition), cost_of_attrition, fill = Department)) +
  geom_bar(stat = 'identity', position = position_dodge2(preserve = 'single')) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_tq() +
  scale_fill_tq() +
  coord_flip() +
  labs(title = 'Estimated attrition cost by Job Role and department',
       subtitle = 'A $15.7M. problem',
       x = 'Job Role and Department',
       y = 'Cost of attrition in ')


#Workflow of attrition

count_to_pct<- function(data, ..., col = n){
  
  grouping_vars_expr<- quos(...)
  col_expr<- enquo(col)
  
  ret<- data %>% group_by(!!! grouping_vars_expr) %>%
    mutate(pct = (!! col_expr)/sum((!! col_expr))) %>%
    ungroup()
  
  return(ret)
}

assess_attrition<- function(data, attrition_col, attrition_val, baseline_pct){
  
  attrition_col_expr<- enquo(attrition_col)
  
  data %>% filter((!! attrition_col_expr) %in% attrition_val) %>%
    arrange(desc(pct)) %>%
    mutate(above_industry_avg = case_when(pct > baseline_pct ~ "Yes",
                                          TRUE ~ "No"))
  
}

dept_job_role_tbl %>% count(Department, JobRole, Attrition) %>%
  count_to_pct(Department, JobRole) %>%
  assess_attrition(Attrition, "Yes", 0.088) %>%
  mutate(cost_of_attrition = calculate_attrition_cost( n = n , salary = 80000))


#Exploratory Data Analysis (EDA) ----

##Step1: Data summary ----

skim(train_raw_tbl)

#Character Data Type
train_raw_tbl %>% select_if(is.character) %>%
  glimpse()


train_raw_tbl %>% select_if(is.character) %>%
  map(unique)

train_raw_tbl %>% select_if(is.character) %>%
  map(~ table(.) %>% prop.table())

#Numeric Data Type

train_raw_tbl %>% select_if(is.numeric) %>%
  map(~unique(.) %>% length())

train_raw_tbl %>% select_if(is.numeric) %>%
  map_df(~unique(.) %>% length()) %>%
  gather() %>%
  arrange(value) %>%
  filter(value <= 10)

##Step2: Data visualization ----

plot_ggpairs<- function(data, color = NULL, density_alpha = 0.5, plot_title = 'No title'){
  
  color_expr<- enquo(color)
  
  if (rlang::quo_is_null(color_expr)) 
  {
    
    g<- data %>% ggpairs(lower = "blank")
    
  } else{
    
    color_name<- quo_name(color_expr)
    
    g<- data %>% ggpairs(mapping = aes_string(color = color_name),
                         lower = "blank",
                         diag = list(continuous = wrap("densityDiag", alpha = density_alpha))) +
      theme(legend.position = "bottom") +
      labs(title = plot_title) +
      scale_fill_tq() +
      theme_tq()
  }
  
  return(g)
  
}



train_raw_tbl %>% select(Attrition, 
                         Age, 
                         Gender, 
                         MaritalStatus, 
                         NumCompaniesWorked, 
                         Over18, 
                         DistanceFromHome) %>%
                  plot_ggpairs(color = Attrition)

##Explore features by category ----

##1. Descriptive Features: Age, Gender, Marital Status ----
train_raw_tbl %>% select(Attrition, 
                         Age, 
                         Gender, 
                         MaritalStatus, 
                         NumCompaniesWorked, 
                         Over18, 
                         DistanceFromHome) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Descriptive Features')

##2. Employment Features: Department, Job Role, Jobe Level ----
train_raw_tbl %>% select(Attrition, 
                         contains("employee"),
                         contains("department"),
                         contains("job")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Employment Features')

##3. Compensation Features: HourlyRate, MonthIncome, StockoptionLevel ----
train_raw_tbl %>% select(Attrition, 
                         contains("income"),
                         contains("rate"),
                         contains("stock")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Compensation Features')

##4. Survey Features: SatisfactioLevel, WorklifeBalance ----
train_raw_tbl %>% select(Attrition, 
                         contains("satisfaction"),
                         contains("life")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Survey Features')

##5. Performance Features: Job Involvement, Performance Rating ----
train_raw_tbl %>% select(Attrition, 
                         contains("perfomance"),
                         contains("involvement")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Performance Features')

##6. Worklife Features ----
train_raw_tbl %>% select(Attrition, 
                         contains("overtime"),
                         contains("travel")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Work-life balance Features')

##7. Training and Education ----
train_raw_tbl %>% select(Attrition, 
                         contains("training"),
                         contains("education")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Training and Education Features')

##8. Time based Features: Years at company, Years in current role ----
train_raw_tbl %>% select(Attrition, 
                         contains("years")) %>%
  plot_ggpairs(color = Attrition, plot_title = 'Data exploration for Time based Features')

#Data preparation ----

##Relabeling Features----
source("HR_employee_attriction_case_study_2/01_Script_and_function/data_processing_pipeline.R")

train_readable_tbl<- process_hr_data_readable(train_raw_tbl, definition_raw_tbl)
test_readable_tbl<- process_hr_data_readable(test_raw_tbl, definition_raw_tbl)

#Education

#Before
train_raw_tbl %>% ggplot(aes(Education)) +
  geom_bar()

#After
train_readable_tbl %>% ggplot(aes(Education)) +
  geom_bar()

#Plot faceted hist plot function

plot_hist_facet<- function(data, 
                           bins = 10, 
                           ncol = 5,
                           fct_reorder = FALSE,
                           fct_rev = FALSE,
                           fill = palette_light()[[3]],
                           color = "white",
                           scale = "free") {
  
  data_factored<- data %>% mutate_if(is.character, as.factor) %>%
    mutate_if(is.factor, as.numeric) %>%
    gather(key = key, value = value, factor_key = TRUE)
  
  if (fct_reorder) {
    data_factored<- data_factored %>% mutate(key = as.character(key), as.factor())
    
  }
  
  if (fct_rev) {
    data_factored<- data_factored %>% mutate(key = fct_rev(key))
    
  }
  
  g<- data_factored %>% ggplot(aes(x = value, group = key)) +
    geom_histogram(bins = bins, fill = fill, color = color) +
    facet_wrap(~key, ncol = ncol, scale = scale) +
    theme_tq()
  
  return(g)
}

source('HR_employee_attriction_case_study_2/01_Script_and_function/plot_hist_facet.R')

train_raw_tbl %>% plot_hist_facet(bins = 10, ncol = 5, plot_title = 'Data as it is')

#Data preprocessing with recipe ----

##Goal: Correlation analysis
##1.Impute and NZV and Outliers ----
recipe_obj<- recipe(Attrition ~.,train_readable_tbl) %>%
  step_zv(all_predictors())

##2.Transformations ----
skewed_feature_names<- train_readable_tbl %>% select_if(is.numeric) %>% 
  map_df(skewness) %>%
  gather(factor_key = T) %>%
  arrange(desc(value)) %>%
  filter(value>= 0.8) %>%
  pull(key) %>%
  as.character()

train_readable_tbl %>% select(all_of(skewed_feature_names)) %>%
  plot_hist_facet(plot_title = 'Skewed features')


skewed_feature_names<- train_readable_tbl %>% select_if(is.numeric) %>% 
  map_df(skewness) %>%
  gather(factor_key = T) %>%
  arrange(desc(value)) %>%
  filter(value>= 0.8) %>%
  filter(!key %in% c("JobLevel", "StockOptionLevel")) %>%
  pull(key) %>%
  as.character()  

factor_names<- c("JobLevel", "StockOptionLevel")

recipe_obj<- recipe(Attrition ~.,train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(all_of(factor_names), fn = as.factor)


##3.Discretize ----
##4.Normalization (Scaling) ----
recipe_obj<- recipe(Attrition ~.,train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(all_of(factor_names), fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric())

##5.Dummy variables ----
recipe_obj<- recipe(Attrition ~.,train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(all_of(factor_names), fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal())

##6.Interaction variables / Engineering Feature ----
##7.Multivariate Transformation ----


##Final Recipe ----
recipe_obj<- recipe(Attrition ~.,train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_YeoJohnson(skewed_feature_names) %>%
  step_mutate_at(all_of(factor_names), fn = as.factor) %>%
  step_center(all_numeric()) %>%
  step_scale(all_numeric()) %>%
  step_dummy(all_nominal()) %>%
  prep()


train_tbl<- bake(recipe_obj, train_readable_tbl)
test_tbl<- bake(recipe_obj, test_readable_tbl)

#Correlation Analysis ----

# get_cor<- function(data, 
#                    target, 
#                    use = "pairwise.complete.obs",
#                    fct_reorder = FALSE,
#                    fct_rev = FALSE) {
#   
#   feature_exp<- enquo(target)
#   feature_name<- quo_name(feature_exp)
#   
#   data_cor<- data %>% mutate_if(is.character, as.factor) %>%
#     mutate_if(is.factor, as.numeric) %>%
#     cor(use = use) %>%
#     as_tibble() %>%
#     mutate(feature = names(.)) %>%
#     select(feature, !! feature_exp) %>%
#     filter(!(feature == feature_name)) %>%
#     mutate_if(is.character, as.factor)
#   
#   if (fct_reorder) {
#     data_cor<- data_cor %>% mutate(feature = fct_reorder(feature, !! feature_exp)) %>%
#       arrange(feature)
#   }
#   
#   if (fct_rev) {
#     data_cor<- data_cor %>% mutate(feature = fct_rev(feature)) %>%
#       arrange(feature)
#   }
#   
#   return(data_cor)
# }

source('HR_employee_attriction_case_study_2/01_Script_and_function/plot_cor.R')

train_tbl %>% get_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T)


# plot_cor <- function(data, target, fct_reorder = FALSE, fct_rev = FALSE,
#                      include_lbl = TRUE, lbl_precision = 2, lbl_position = "outward",
#                      size = 2, line_size = 1, vert_size = 1, 
#                      color_pos = palette_light()[[1]], color_neg = palette_light()[[2]]) {
#   
#   feature_exp<- enquo(target)
#   feature_name<- quo_name(feature_exp)
#   
#   data_cor<- data %>% get_cor(!! feature_exp, fct_reorder = fct_reorder, fct_rev = fct_rev) %>%
#     mutate(feature_name_text = round(!! feature_exp, lbl_precision)) %>%
#     mutate(Correlation = case_when((!! feature_exp) >= 0 ~ "Positive", 
#                                    T ~ "Negative") %>% as.factor())
#   
#   g<- data_cor %>% ggplot(aes_string(x = feature_name, y = "feature", group = "feature")) +
#     geom_point(aes(color = Correlation), size = size) +
#     geom_segment(aes(xend = 0, yend = feature, color = Correlation), size = line_size) +
#     geom_vline(xintercept = 0, color = palette_light()[[1]], size = vert_size) +
#     expand_limits(x = c(-1,1)) +
#     theme_tq() +
#     scale_color_manual(values = c(color_neg, color_pos))
#   
#   if (include_lbl) {
#     g<- g + geom_label(aes(label = feature_name_text), hjust = lbl_position)
#   }
#   
#   return(g)
# }

train_tbl %>% plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel')

#Correlation Evaluation ----

#1. Descriptive Features: Age, Gender, Marital Status
train_tbl %>% select(Attrition_Yes, 
                     Age, 
                     contains("Gender"), 
                     contains("MaritalStatus"), 
                     NumCompaniesWorked, 
                     contains("Over18"), 
                     DistanceFromHome) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Descriptive Features')

#2. Employment Features: Department, Job Role, Job Level
train_tbl %>% select(Attrition_Yes, 
                     contains("employee"),
                     contains("department"),
                     contains("job")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Employment Features')

#3. Compensation Features: HourlyRate, MonthIncome, StockoptionLevel
train_tbl %>% select(Attrition_Yes, 
                     contains("income"),
                     contains("rate"),
                     contains("stock")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Compensation Features')

#4. Survey Features: SatisfactioLevel, WorklifeBalance
train_tbl %>% select(Attrition_Yes, 
                     contains("satisfaction"),
                     contains("life")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Survey Features')

#5. Performance Data: Job Involvement, Performance Rating
train_tbl %>% select(Attrition_Yes, 
                     contains("Perfomance"),
                     contains("involvement")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Performance Features')

#6. Worklife Features
train_tbl %>% select(Attrition_Yes, 
                     contains("overtime"),
                     contains("travel")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Work-life balance Features')

#7. Training and Education
train_tbl %>% select(Attrition_Yes, 
                     contains("training"),
                     contains("education")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Training and Education Features')

#8. Time based Features: Years at company, Years in current role
train_tbl %>% select(Attrition_Yes, 
                     contains("years")) %>%
  plot_cor(target = Attrition_Yes, fct_reorder = T, fct_rev = T, plot_title = 'Correlation Funnel - Time based Features')

#Ml Preprocessing----

recipe_obj_2<- recipe(Attrition ~., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(c(JobLevel, StockOptionLevel), fn = as.factor) %>%
  prep()

train_tbl<- bake(recipe_obj_2, new_data = train_readable_tbl)                            
test_tbl<- bake(recipe_obj_2, new_data = test_readable_tbl)                            


#Modelling ----

h2o.init()


train_tbl_h2o<- as.h2o(train_tbl)
test_tbl_h2o<- as.h2o(test_tbl)

y<- "Attrition"

x<- setdiff(names(train_tbl_h2o), y)


automl_models_h20<- h2o.automl(x = x,
                               y = y,
                               training_frame = train_tbl_h2o,
                               nfolds = 10,
                               max_runtime_secs = 120)


automl_h2o_leaderboard<- h2o.get_leaderboard(automl_models_h20)
automl_h2o_leaderboard
h2o.get_best_model(automl_models_h20)


h2o.saveModel(h2o.get_best_model(automl_models_h20),path = "HR_employee_attriction_case_study_2/03_Output/Model/")


#Making Predictions -----

best_model<- h2o.get_best_model(automl_models_h20)


prediction<- h2o.predict(best_model, test_tbl_h2o)

prediction_tbl<- prediction %>% as_tibble()
prediction_tbl

#Visualize the leader board ----

data_transformed<- automl_models_h20@leaderboard %>% as_tibble() %>%
  select(model_id, auc, logloss, aucpr) %>%
  mutate(model_type = str_split(model_id, "_", simplify = T) %>% .[,1]) %>%
  slice(1:20) %>%
  rownames_to_column() %>%
  mutate(model_id = as.factor(model_id) %>% reorder(auc),
         model_type = as.factor(model_type)) %>%
  gather(key = key, value = value, -c(model_id, model_type, rowname), factor_key = T) %>%
  mutate(model_id = paste0(rowname, ". ", model_id) %>% as_factor() %>% fct_rev())

data_transformed %>% ggplot(aes(value, model_id, color = model_type)) +
  geom_point(size = 3) +
  geom_label(aes(label = round(value, 2), hjust = "inward")) +
  facet_wrap(~key, scales = "free_x") +
  theme_tq() +
  scale_color_tq() +
  labs(title = "H2o Leader Board Metrics", subtitle = paste0("Ordered by: AUC"),
       y = "Model position, Model ID", x = "")

#Make function

plot_h2o_leaderboard<- function(h2o_leaderboard, 
                                order_by = c("auc", "logloss", "aucpr"),
                                n_max = 20,
                                size = 4,
                                include_lbl = TRUE){
  
  #Input
  order_by<- tolower(order_by[[1]])
  
  
  leaderboard_tbl<- h2o_leaderboard %>% as_tibble() %>%
    select(model_id, auc, logloss, aucpr) %>%
    mutate(model_type = str_split(model_id, "_", simplify = T)[,1]) %>%
    rownames_to_column(var = "rowname") %>%
    mutate(model_id = paste0(rowname, ". ", as.character(model_id)) %>% as.factor())
  #Transformation
  if (order_by == "auc") {
    data_transformed_tbl<- leaderboard_tbl %>% slice(1:n_max) %>%
      mutate(model_id = as_factor(model_id) %>% reorder(auc),
             model_type = as.factor(model_type)) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname),
             factor_key = TRUE)
    
    
  } else if (order_by == "logloss"){
    
    data_transformed_tbl<- leaderboard_tbl %>% slice(1: n_max) %>%
      mutate(model_id = as_factor(model_id) %>% reorder(logloss) %>% fct_rev(),
             model_type = as.factor(model_type)) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname),
             factor_key = TRUE)
    
  } else if (order_by == "aucpr"){
    
    data_transformed_tbl<- leaderboard_tbl %>% slice(1: n_max) %>%
      mutate(model_id = as_factor(model_id) %>% reorder(aucpr),
             model_type = as.factor(model_type)) %>%
      gather(key = key, value = value, 
             -c(model_id, model_type, rowname),
             factor_key = TRUE)
  } else {
    
    stop(paste0("order_by = '", order_by, "' is not a permitted option"))
  }
  #Visualization
  g<- data_transformed_tbl %>% ggplot(aes(value, model_id, color = model_type)) +
    geom_point(size = size) +
    facet_wrap(~key, scales = "free_x") +
    theme_tq() +
    scale_color_tq() +
    labs(title = "H2o Leader Board Metrics", subtitle = paste0("Ordered by: ",
                                                               toupper(order_by)),
         y = "Model position, Model ID", x = "")
  
  if (include_lbl){
    
    g<- g + geom_label(aes(label = round(value, 2), hjust = "inward"))
    
  }
  
  return(g)
  
}

#plot_h2o_leaderboard(automl_h2o_leaderboard, n_max = 10, size = 3)



#Assessing Performance ----
h2o_performance<- h2o.performance(best_model, newdata = test_tbl_h2o)

##Summary metrics ----
h2o.auc(h2o_performance)

h2o.confusionMatrix(h2o_performance) %>% as_tibble()

model_conf_mtrx<- confusionMatrix(prediction_tbl$predict, test_tbl$Attrition)

draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  

draw_confusion_matrix(model_conf_mtrx)

performance_tbl<- h2o_performance %>% h2o.metric() %>% 
  as_tibble()

performance_tbl %>% ggplot(aes(x = threshold)) +
  geom_line(aes(y = precision), color = "blue", size = 1) +
  geom_line(aes(y = recall), color = "red", size = 1) +
  geom_vline(xintercept = h2o.find_threshold_by_max_metric(h2o_performance, "f1")) +
  annotate(geom = 'text', label = 'F1 Threshold', x = h2o.find_threshold_by_max_metric(h2o_performance, "f1"), y = 0.25, angle = 90, vjust = 1.2) +
  theme_tq() +
  labs(title = "Precision Vs Recall", y = "value")

##ROC Curve ----
plot(h2o_performance, type = 'roc')

# p1 <- performance_tbl %>% ggplot(aes(fpr, tpr)) +
#   geom_line(size = 1.5) +
#   theme_tq() +
#   scale_color_tq() +
#   labs(title = "ROC", x = "FPR", y = "TPR") +
#   theme(legend.direction = "vertical")


##Gain/ Lift

get_gain_lift <- function(h20_performance, test_tbl_h2o) {
  
  h20_performance %>% h2o.gainsLift() %>%
    as_tibble() %>%
    select(group, 
           cumulative_data_fraction, 
           cumulative_capture_rate, 
           cumulative_lift) %>%
    rename(gain = cumulative_capture_rate, lift = cumulative_lift) 
  
}

gain_lift_tbl<- get_gain_lift(h2o_performance, test_tbl_h2o)

p3 <- gain_lift_tbl %>% ggplot(aes(cumulative_data_fraction, gain)) +
  geom_line(size = 1.5, color = "red") +
  geom_segment(x = 0, y = 0, xend = 1, yend = 1, color = "black", size = 1) +
  theme_tq() +
  scale_color_tq() +
  expand_limits(x = c(0, 1), y = c(0, 1)) +
  labs(title = "Gain", x = "Cumulative Data Fraction", y = "Gain") +
  theme(legend.position = "none")
p3

p4 <- gain_lift_tbl %>% ggplot(aes(cumulative_data_fraction, lift)) +
  geom_line(size = 1.5, color = "red" ) +
  geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
               color = "black", 
               size = 1) +
  theme_tq() +
  scale_color_tq() +
  expand_limits(x = c(0, 1), y = c(0, 1)) +
  labs(title = "Lift", x = "Cumulative Data Fraction", y = "Lift")
p4                        


##Combined plot ----
plot_h2o_performance <- function(h2o_leaderboard, newdata, order_by = c("auc", "logloss"),
                                 max_models = 3, size = 1.5) {
  
  # Inputs
  
  leaderboard_tbl <- h2o_leaderboard %>%
    as_tibble() %>%
    slice(1:max_models)
  
  newdata_tbl <- newdata %>%
    as_tibble()
  
  order_by <- tolower(order_by[[1]])
  order_by_expr <- rlang::sym(order_by)
  
  h2o.no_progress()
  
  # 1. Model metrics
  
  get_model_performance_metrics <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl))
    
    perf_h2o %>%
      h2o.metric() %>%
      as_tibble() %>%
      select(threshold, tpr, fpr, precision, recall)
    
  }
  
  model_metrics_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_model_performance_metrics, newdata_tbl)) %>%
    unnest(metrics) %>%
    mutate(model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)), auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    )
  
  
  # 1A. ROC Plot
  
  p1 <- model_metrics_tbl %>%
    ggplot(aes_string("fpr", "tpr", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "ROC", x = "FPR", y = "TPR") +
    theme(legend.direction = "vertical")
  
  # 1B. Precision vs Recall
  
  p2 <- model_metrics_tbl %>%
    ggplot(aes_string("recall", "precision", color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    theme_tq() +
    scale_color_tq() +
    labs(title = "Precision Vs Recall", x = "Recall", y = "Precision") +
    theme(legend.position = "none")
  
  
  # 2. Gain / Lift
  
  get_gain_lift <- function(model_id, test_tbl) {
    
    model_h2o <- h2o.getModel(model_id)
    perf_h2o  <- h2o.performance(model_h2o, newdata = as.h2o(test_tbl)) 
    
    perf_h2o %>%
      h2o.gainsLift() %>%
      as.tibble() %>%
      select(group, cumulative_data_fraction, cumulative_capture_rate, cumulative_lift)
    
  }
  
  gain_lift_tbl <- leaderboard_tbl %>%
    mutate(metrics = map(model_id, get_gain_lift, newdata_tbl)) %>%
  unnest(metrics) %>%
    mutate(
      model_id = as_factor(model_id) %>% 
        fct_reorder(!! order_by_expr, .desc = ifelse(order_by == "auc", TRUE, FALSE)),
      auc  = auc %>% 
        round(3) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id)),
      logloss = logloss %>% 
        round(4) %>% 
        as.character() %>% 
        as_factor() %>% 
        fct_reorder(as.numeric(model_id))
    ) %>%
    rename(
      gain = cumulative_capture_rate,
      lift = cumulative_lift
    ) 
  
  # 2A. Gain Plot
  
  p3 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "gain", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 0, xend = 1, yend = 1, 
                 color = "black", size = size) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Gain",
         x = "Cumulative Data Fraction", y = "Gain") +
    theme(legend.position = "none")
  
  # 2B. Lift Plot
  
  p4 <- gain_lift_tbl %>%
    ggplot(aes_string("cumulative_data_fraction", "lift", 
                      color = "model_id", linetype = order_by)) +
    geom_line(size = size) +
    geom_segment(x = 0, y = 1, xend = 1, yend = 1, 
                 color = "black", size = size) +
    theme_tq() +
    scale_color_tq() +
    expand_limits(x = c(0, 1), y = c(0, 1)) +
    labs(title = "Lift",
         x = "Cumulative Data Fraction", y = "Lift") +
    theme(legend.position = "none")
  
  
  # Combine using cowplot
  p_legend <- get_legend(p1)
  p1 <- p1 + theme(legend.position = "none")
  
  p <- cowplot::plot_grid(p1, p2, p3, p4, ncol = 2) 
  
  p_title <- ggdraw() + 
    draw_label("H2O Model Metrics", size = 18, fontface = "bold", 
               colour = palette_light()[[1]])
  
  p_subtitle <- ggdraw() + 
    draw_label(glue::glue("Ordered by {toupper(order_by)}"), size = 10,  
               colour = palette_light()[[1]])
  
  ret <- plot_grid(p_title, p_subtitle, p, p_legend, 
                   ncol = 1, rel_heights = c(0.05, 0.05, 1, 0.05 * max_models))
  
  h2o.show_progress()
  
  return(ret)
  
}

plot_h2o_performance(automl_h2o_leaderboard, 
                     newdata = test_tbl_h2o, 
                     order_by = "auc", 
                     size = 1,
                     max_models = 4)



#Lime ----
prediction_tbl<- prediction_tbl %>% bind_cols(test_tbl %>% select(Attrition, EmployeeNumber))

test_tbl %>% slice(5) %>%
  glimpse()

##Lime single explanation  ----
explainer_obj<- train_tbl %>% select(-Attrition) %>%
  lime(model = best_model,
       bin_continuous = TRUE,
       n_bins = 4,
       quantile_bins = TRUE)


explanation<- test_tbl %>% slice(5) %>%
  select(-Attrition) %>%
  explain(explainer = explainer_obj,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 1)

explanation

explanation %>% as_tibble() %>%
  select(feature:prediction)

plot_features(explanation = explanation, ncol = 1)


##Multiple explanation ----
explanation_multpl<- test_tbl %>% slice(1:4) %>%
  select(-Attrition) %>%
  explain(explainer = explainer_obj,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 1)

explanation_multpl %>% as_tibble()


plot_features(explanation = explanation_multpl, ncol = 4)

explanation_multpl_2<- test_tbl %>% slice(1:25) %>%
  select(-Attrition) %>%
  explain(explainer = explainer_obj,
          n_labels = 1,
          n_features = 8,
          n_permutations = 5000,
          kernel_width = 1)

plot_explanations(explanation = explanation_multpl_2)

#Expected Value ----

##Calculate expected value with OT ----
prediction_wth_OT_tbl<- prediction_tbl %>% bind_cols(test_tbl %>% select(EmployeeNumber, 
                                                                         MonthlyIncome, 
                                                                         OverTime))


ev_wth_OT_tbl<- prediction_wth_OT_tbl %>% 
  mutate(attrition_cost = calculate_attrition_cost(salary = MonthlyIncome*12, net_revenue_per_employee = 250000)) %>%
  mutate(cost_of_policy_change = 0) %>%
  mutate(expected_attrition_cost = Yes * (attrition_cost + cost_of_policy_change) + No * cost_of_policy_change)

total_ev_wth_OT_tbl<- ev_wth_OT_tbl %>% summarise(total_expct_attrition_cost_0 = sum(expected_attrition_cost))

##Calculate expected value with NO OT ----

test_NO_OT_tbl<- test_tbl %>% mutate(OverTime = fct_recode(OverTime, "No" = "Yes"))

prediction_NO_OT_tbl<- h2o.predict(best_model, as.h2o(test_NO_OT_tbl)) %>% 
  as_tibble() %>%
  bind_cols(test_tbl %>% select(EmployeeNumber, MonthlyIncome, OverTime),
            test_NO_OT_tbl %>% select(OverTime)) %>%
  rename(OverTime_0 = OverTime...6, OverTime_1 = OverTime...7)

prediction_NO_OT_tbl

avg_overtime_pct<- 0.1

ev_wth_NO_OT_tbl<- prediction_NO_OT_tbl %>% 
  mutate(attrition_cost = calculate_attrition_cost(salary = MonthlyIncome*12, net_revenue_per_employee = 250000)) %>%
  mutate(cost_of_policy_change = case_when(OverTime_0 == "Yes" & OverTime_1 == "No" ~ avg_overtime_pct * attrition_cost,
                                           TRUE ~ 0 )) %>%
  mutate(expected_attrition_cost = Yes * (attrition_cost + cost_of_policy_change) + No * cost_of_policy_change)

ev_wth_NO_OT_tbl

total_ev_wth_NO_OT_tbl<- ev_wth_NO_OT_tbl %>% summarise(total_expct_attrition_cost_1 = sum(expected_attrition_cost))

total_ev_wth_NO_OT_tbl

##Savings calculation ----
bind_cols(total_ev_wth_OT_tbl, total_ev_wth_NO_OT_tbl) %>% 
  mutate(savings = total_expct_attrition_cost_0 - total_expct_attrition_cost_1,
         pct_savings = savings/total_expct_attrition_cost_0)

#Threshold & Rates ----
performance_h2o<- best_model %>% h2o.performance(test_tbl_h2o)

rates_by_threshold_tbl<- performance_h2o %>% h2o.metric() %>% as_tibble()

rates_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>%
  filter(f1  == max(f1)) %>%
  slice(1)

rates_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>%
  gather(key = "key", value = "value", tnr:tpr, factor_key = TRUE) %>%
  mutate(key = fct_reorder2(key, threshold, value)) %>%
  ggplot(aes(threshold, value, color = key)) +
  geom_point() +
  geom_smooth() +
  theme_tq() +
  scale_color_tq() +
  theme(legend.position = "right") +
  labs(title = "Expected Rates", x = "Threshold", y = "Value")


##Calculate expected value with targeted OT ----
max_f1_tbl<- rates_by_threshold_tbl %>% select(threshold, f1, tnr:tpr) %>% 
  filter(f1 == max(f1)) %>%
  slice(1)

tnr<- max_f1_tbl$tnr
fnr<- max_f1_tbl$fnr
fpr<- max_f1_tbl$fpr
tpr<- max_f1_tbl$tpr

threshold<- max_f1_tbl$threshold

test_targeted_OT_tbl<- test_tbl %>% add_column(Yes = predictions_with_OT_tbl$Yes) %>%
  mutate(OverTime = case_when(Yes >= threshold ~ factor("No", levels = levels(test_tbl$OverTime)),
                              TRUE ~ OverTime)) %>%
  select(-Yes)

predictions_targeted_OT_tbl <- best_model %>% h2o.predict(newdata = as.h2o(test_targeted_OT_tbl)) %>%
  as_tibble() %>%
  bind_cols(test_tbl %>% select(EmployeeNumber, MonthlyIncome, OverTime),
            test_targeted_OT_tbl %>% select(OverTime)) %>%
  rename(OverTime_0 = OverTime...6, OverTime_1 = OverTime...7)

avg_overtime_pct<- 0.1

ev_targeted_OT_tbl <- predictions_targeted_OT_tbl %>% mutate(attrition_cost = calculate_attrition_cost(n = 1,
                                                                                                       salary = MonthlyIncome * 12,
                                                                                                       net_revenue_per_employee = 250000))%>%
  mutate(cost_of_policy_change = case_when(OverTime_0 == "Yes" & OverTime_1 == "No" ~ attrition_cost * avg_overtime_pct,
                                           TRUE ~ 0)) %>%
  mutate(cb_tn = cost_of_policy_change,
         cb_fp = cost_of_policy_change,
         cb_tp = cost_of_policy_change + attrition_cost,
         cb_fn = cost_of_policy_change + attrition_cost,
         expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
           No *  (tnr*cb_tn + fpr*cb_fp)) 

ev_targeted_OT_tbl %>% select(attrition_cost, cost_of_policy_change, cb_tn, cb_fp, cb_tp, cb_fn, expected_attrition_cost)

total_ev_tgt_OT_tbl<- ev_targeted_OT_tbl %>% summarise(total_expct_attrition_cost_1 = sum(expected_attrition_cost))

total_ev_tgt_OT_tbl

##Savings Calculation ----

saving_tgt_tbl<- bind_cols(total_ev_wth_OT_tbl, total_ev_tgt_OT_tbl) %>% 
  mutate(savings = total_expct_attrition_cost_0 - total_expct_attrition_cost_1)

saving_tgt_tbl

#Optimizing by threshold ----
##make function to calculate savings by threshold ----
calculate_savings_by_threshold <- function(test_data, h2o_model, threshold = 0,
                                           tnr = 0, fpr = 1, fnr = 0, tpr = 1) {
  
  
  data_0_tbl <- as_tibble(test_data)
  
  # 4. Expected Value 
  
  # 4.1 Calculating Expected Value With OT 
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = 250000)
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost = 
        Yes * (attrition_cost + cost_of_policy_change) +
        No *  (cost_of_policy_change)
    )
  
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value With Targeted OT
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes) 
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime...6,
      OverTime_1 = OverTime...7
    )
  
  
  avg_overtime_pct <- 0.10
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = 250000)
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_1 == "No" & OverTime_0 == "Yes" 
        ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      ))%>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_fn = attrition_cost + cost_of_policy_change,
      cb_tp = attrition_cost + cost_of_policy_change,
      expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
        No * (tnr*cb_tn + fpr*cb_fp)
    )
  
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}



##Optimization ----

max_f1_savings<- calculate_savings_by_threshold(test_tbl, 
                                                best_model, 
                                                threshold = max_f1_tbl$threshold,
                                                tnr = max_f1_tbl$tnr,
                                                fpr = max_f1_tbl$fpr,
                                                fnr = max_f1_tbl$fnr,
                                                tpr = max_f1_tbl$tpr)


smpl<- seq(1,nrow(rates_by_threshold_tbl), length.out = 40) %>% round(digits = 0)

partial(calculate_savings_by_threshold,
        test_data = test_tbl,
        h2o_model = best_model)

rates_by_threshold_opt_tbl<- rates_by_threshold_tbl %>% 
  select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(savings = pmap_dbl(.l  = list(threshold = threshold,
                                       tnr = tnr,
                                       fnr = fnr,
                                       fpr = fpr,
                                       tpr = tpr),
                            .f =  partial(calculate_savings_by_threshold, 
                                          test_data = test_tbl, 
                                          h2o_model = best_model)))

rates_by_threshold_opt_tbl

rates_by_threshold_opt_tbl %>% ggplot(aes(threshold, savings)) +
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  #Target OT optimized saving
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl %>% filter(savings == max(savings))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -0.8, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl %>% filter(savings == max(savings))) +
  annotate("text", 
           x = rates_by_threshold_opt_tbl %>% filter(savings == max(savings)) %>% select(threshold) %>% as.numeric(),
           y = rates_by_threshold_opt_tbl %>% filter(savings == max(savings)) %>% select(savings) %>% as.numeric(),
           vjust = -4,
           label = "Targeted OT Policy", 
           color = palette_light()[[3]]) +
  #Saving at max F1
  geom_vline(xintercept = max_f1_tbl$threshold, 
             color = palette_light()[[5]],
             size = 2) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings),
           x = max_f1_tbl$threshold,
           y = max_f1_savings, 
           hjust = -0.1,
           vjust = -3,
           color = palette_light()[[3]]) +
  annotate("text", 
           x = max_f1_tbl$threshold, 
           hjust = -0.06,
           y = max_f1_savings, vjust = -4,
           label = "Saving a max F1", 
           color = palette_light()[[3]]) +
  #No OT saving
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl %>% filter(threshold == min(threshold))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -0.8, color = palette_light()[[2]],
             data = rates_by_threshold_opt_tbl %>% filter(threshold == min(threshold))) +
  annotate("text", 
           x = 0, 
           vjust = -4,
           y = rates_by_threshold_opt_tbl %>% filter(threshold == min(threshold)) %>% select(savings) %>% as.numeric(), 
           label = "NO OT Policy", 
           color = palette_light()[[2]]) +
  #No policy change
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl %>% filter(threshold == max(threshold))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -0.8, color = palette_light()[[2]],
             data = rates_by_threshold_opt_tbl %>% filter(threshold == max(threshold))) +
  annotate("text", 
           x = rates_by_threshold_opt_tbl %>% filter(threshold == max(threshold)) %>% select(threshold) %>% as.numeric(), 
           vjust = -4,
           y = rates_by_threshold_opt_tbl %>% filter(threshold == max(threshold)) %>% select(savings) %>% as.numeric(), 
           label = "No policy change", 
           color = palette_light()[[2]]) +
  theme_tq() +
  expand_limits(x = c(-.1,1.1), y = c(8e5)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = 'Saving optimization by threshold',
       subtitle = 'Targeted Overtime and Stock option policy')

#Sensitivity analysis ----
## Create sensitivity function ----
calculate_savings_by_threshold_2 <- function(test_data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000) {
  
  data_0_tbl <- as_tibble(test_data)
  
  
  # 4. Expected Value 
  
  # 4.1 Calculating Expected Value With OT 
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = net_revenue_per_employee) 
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost = 
        Yes * (attrition_cost + cost_of_policy_change) +
        No *  (cost_of_policy_change)
    )
  
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value With Targeted OT
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
    select(-Yes) 
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as_tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime),
      data_1_tbl %>%
        select(OverTime)
    ) %>%
    rename(
      OverTime_0 = OverTime...6,
      OverTime_1 = OverTime...7)
  
  
  avg_overtime_pct <- avg_overtime_pct 
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
    mutate(
      cost_of_policy_change = case_when(
        OverTime_1 == "No" & OverTime_0 == "Yes" 
        ~ attrition_cost * avg_overtime_pct,
        TRUE ~ 0
      ))%>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_fn = attrition_cost + cost_of_policy_change,
      cb_tp = attrition_cost + cost_of_policy_change,
      expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
        No * (tnr*cb_tn + fpr*cb_fp)
    )
  
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

##Perform analysis ----
max_saving_rates_tbl<- rates_by_threshold_opt_tbl %>% filter(savings == max(savings))

calculate_savings_by_threshold_2(test_data = test_tbl, 
                                 h2o_model = best_model, 
                                 threshold = max_saving_rates_tbl$threshold,
                                 tnr = max_saving_rates_tbl$tnr,
                                 fnr = max_saving_rates_tbl$fnr,
                                 tpr = max_saving_rates_tbl$tpr,
                                 fpr = max_saving_rates_tbl$fpr)


calculate_saving_by_threshold_2_preloaded<- partial(calculate_savings_by_threshold_2,
                                                    test_data = test_tbl, 
                                                    h2o_model = best_model, 
                                                    threshold = max_saving_rates_tbl$threshold,
                                                    tnr = max_saving_rates_tbl$tnr,
                                                    fnr = max_saving_rates_tbl$fnr,
                                                    tpr = max_saving_rates_tbl$tpr,
                                                    fpr = max_saving_rates_tbl$fpr)

sensitivity_tbl<- list(avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
                       net_revenue_per_employee = seq(200000, 400000, by = 50000)) %>%
  cross_df() %>%
  mutate(savings = pmap_dbl(.l = list(avg_overtime_pct = avg_overtime_pct,
                                      net_revenue_per_employee = net_revenue_per_employee),
                            .f = calculate_saving_by_threshold_2_preloaded))

sensitivity_tbl

sensitivity_tbl %>% ggplot(aes(avg_overtime_pct, net_revenue_per_employee)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = round(savings, 0) %>% scales::dollar())) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_fill_gradient2(low = palette_light()[[2]], 
                       mid = "white", 
                       high = palette_light()[[1]],
                       midpoint = 0) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.05, 0.3, by = 0.05)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
       subtitle = "How sensitivity is savings to net revenue per employee and average overtime %",
       x = "Average Overtime Percentage",
       y = "Net Revenue Per Employee")

#Stock option optimization by threshold----
stock_option_cost<- 5000

calculate_savings_by_threshold_3 <- function(data, h2o_model, threshold = 0,
                                             tnr = 0, fpr = 1, fnr = 0, tpr = 1,
                                             avg_overtime_pct = 0.10,
                                             net_revenue_per_employee = 250000,
                                             stock_option_cost = 5000) {
  
  data_0_tbl <- as.tibble(data)
  
  
  # 4. Expected Value 
  
  # 4.1 Calculating Expected Value With OT 
  
  pred_0_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_0_tbl)) %>%
    as.tibble() %>%
    bind_cols(
      data_0_tbl %>%
        select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel)
    )
  
  ev_0_tbl <- pred_0_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = net_revenue_per_employee) 
    ) %>%
    mutate(
      cost_of_policy_change = 0
    ) %>%
    mutate(
      expected_attrition_cost = 
        Yes * (attrition_cost + cost_of_policy_change) +
        No *  (cost_of_policy_change)
    )
  
  
  total_ev_0_tbl <- ev_0_tbl %>%
    summarise(
      total_expected_attrition_cost_0 = sum(expected_attrition_cost)
    )
  
  # 4.2 Calculating Expected Value With Targeted OT & Stock Option Policy
  
  data_1_tbl <- data_0_tbl %>%
    add_column(Yes = pred_0_tbl$Yes) %>%
    mutate(
      OverTime = case_when(
        Yes >= threshold ~ factor("No", levels = levels(data_0_tbl$OverTime)),
        TRUE ~ OverTime
      )
    ) %>%
  mutate(
    StockOptionLevel = case_when(
      Yes >= threshold & StockOptionLevel == 0 
      ~ factor("1", levels = levels(data_0_tbl$StockOptionLevel)),
      TRUE ~ StockOptionLevel
    )
  ) %>%
    select(-Yes) 
  
  pred_1_tbl <- h2o_model %>%
    h2o.predict(newdata = as.h2o(data_1_tbl)) %>%
    as.tibble() %>%
  bind_cols(
    data_0_tbl %>%
      select(EmployeeNumber, MonthlyIncome, OverTime, StockOptionLevel),
    data_1_tbl %>%
      select(OverTime, StockOptionLevel)
  ) %>%
    rename(
      OverTime_0 = OverTime...6,
      OverTime_1 = OverTime...8,
      StockOptionLevel_0 = StockOptionLevel...7,
      StockOptionLevel_1 = StockOptionLevel...9
    )
  
  
  avg_overtime_pct <- avg_overtime_pct 
  stock_option_cost <- stock_option_cost 
  
  ev_1_tbl <- pred_1_tbl %>%
    mutate(
      attrition_cost = calculate_attrition_cost(
        n = 1,
        salary = MonthlyIncome * 12,
        net_revenue_per_employee = net_revenue_per_employee)
    ) %>%
  # cost_OT
  mutate(
    cost_OT = case_when(
      OverTime_1 == "No" & OverTime_0 == "Yes" 
      ~ avg_overtime_pct * MonthlyIncome * 12,
      TRUE ~ 0
    )
  ) %>%
    # cost Stock Options
    mutate(
      cost_SO = case_when(
        StockOptionLevel_1 == "1" & StockOptionLevel_0 == "0"
        ~ stock_option_cost,
        TRUE ~ 0
      )
    ) %>%
    mutate(cost_of_policy_change = cost_OT + cost_SO) %>%
    mutate(
      cb_tn = cost_of_policy_change,
      cb_fp = cost_of_policy_change,
      cb_fn = attrition_cost + cost_of_policy_change,
      cb_tp = attrition_cost + cost_of_policy_change,
      expected_attrition_cost = Yes * (tpr*cb_tp + fnr*cb_fn) + 
        No * (tnr*cb_tn + fpr*cb_fp)
    ) 
  
  
  total_ev_1_tbl <- ev_1_tbl %>%
    summarise(
      total_expected_attrition_cost_1 = sum(expected_attrition_cost)
    )
  
  
  # 4.3 Savings Calculation
  
  savings_tbl <- bind_cols(
    total_ev_0_tbl,
    total_ev_1_tbl
  ) %>%
    mutate(
      savings = total_expected_attrition_cost_0 - total_expected_attrition_cost_1,
      pct_savings = savings / total_expected_attrition_cost_0
    )
  
  return(savings_tbl$savings)
  
}

max_f1_savings_2<- calculate_savings_by_threshold_3(data = test_tbl,
                                                    h2o_model = best_model,
                                                    threshold = max_f1_tbl$threshold,
                                                    tnr = max_f1_tbl$tnr,
                                                    tpr = max_f1_tbl$tpr,
                                                    fnr = max_f1_tbl$fnr,
                                                    fpr = max_f1_tbl$fpr,
                                                    stock_option_cost = stock_option_cost,
                                                    net_revenue_per_employee = 250000,
                                                    avg_overtime_pct = 0.10)



rates_by_threshold_opt_tbl_2<- rates_by_threshold_tbl %>% select(threshold, tnr:tpr) %>%
  slice(smpl) %>%
  mutate(savings = pmap_dbl(.l  = list(threshold = threshold,
                                       tnr = tnr,
                                       fnr = fnr,
                                       fpr = fpr,
                                       tpr = tpr),
                            .f =  partial(calculate_savings_by_threshold_3, 
                                          data = test_tbl, 
                                          h2o_model = best_model)))


rates_by_threshold_opt_tbl_2 %>% ggplot(aes(threshold, savings)) +
  geom_line(color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]]) +
  #Target OT optimized saving
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(savings == max(savings))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = 0.4, hjust = -0.3, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(savings == max(savings))) +
  annotate("text", 
           x = rates_by_threshold_opt_tbl_2 %>% filter(savings == max(savings)) %>% select(threshold) %>% as.numeric(), 
           hjust = -0.50,
           vjust = 2,
           y = rates_by_threshold_opt_tbl_2 %>% filter(savings == max(savings)) %>% select(savings) %>% as.numeric(), 
           label = "Targeted OT & SO", 
           color = palette_light()[[3]]) +
  #Saving at max F1
  geom_vline(xintercept = max_f1_tbl$threshold, 
             color = palette_light()[[5]],
             size = 2) +
  annotate(geom = "label", label = scales::dollar(max_f1_savings_2),
           x = max_f1_tbl$threshold,
           y = max_f1_savings_2, hjust = -0.1,
           color = palette_light()[[3]]) +
  annotate("text", 
           x = max_f1_tbl$threshold, hjust = -0.3,
           y = max_f1_savings_2, vjust = 2.4,
           label = "Saving @ max F1", 
           color = palette_light()[[3]]) +
  #No OT & SO saving
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(threshold == min(threshold))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = 1.5, color = palette_light()[[2]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(threshold == min(threshold))) +
  annotate("text", 
           x = 0, 
           hjust = 0.5,
           vjust = 4,
           y = rates_by_threshold_opt_tbl_2 %>% filter(threshold == min(threshold)) %>% select(savings) %>% as.numeric(), 
           label = "NO OT Policy", 
           color = palette_light()[[2]]) +
  #No policy change
  geom_point(shape = 21, size = 5, color = palette_light()[[3]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(threshold == max(threshold))) +
  geom_label(aes(label = scales::dollar(savings)),
             vjust = -0.8, color = palette_light()[[2]],
             data = rates_by_threshold_opt_tbl_2 %>% filter(threshold == max(threshold))) +
  annotate("text", 
           x = rates_by_threshold_opt_tbl_2 %>% filter(threshold == max(threshold)) %>% select(threshold) %>% as.numeric(), 
           hjust = 0.3,
           y = rates_by_threshold_opt_tbl_2 %>% filter(threshold == max(threshold)) %>% select(savings) %>% as.numeric(),
           vjust = -4,
           label = "No policy change", 
           color = palette_light()[[2]]) +
  theme_tq() +
  expand_limits(x = c(-.1,1.1), y = c(8e5)) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0, 1, by = 0.2)) +
  scale_y_continuous(labels = scales::dollar) +
  #
  geom_vline(aes(xintercept = threshold), 
             color = palette_light()[[3]], size = 2,
             data = rates_by_threshold_opt_tbl_2 %>%
               filter(savings == max(savings))) +
  labs(title = 'Savings optimization by threshold',
       subtitle = 'Targeted Overtime and Stock option policy')

#Sensitivity analysis for Stock option ----

max_saving_rates_tbl_2<- rates_by_threshold_opt_tbl_2 %>% filter(savings == max(savings))


calculate_saving_by_threshold_3_preloaded<- partial(calculate_savings_by_threshold_3,
                                                    data = test_tbl,
                                                    h2o_model = best_model,
                                                    threshold = max_saving_rates_tbl_2$threshold,
                                                    tnr = max_saving_rates_tbl_2$tnr,
                                                    tpr = max_saving_rates_tbl_2$tpr,
                                                    fnr = max_saving_rates_tbl_2$fnr,
                                                    fpr = max_saving_rates_tbl_2$fpr)


sensitivity_tbl_2<- list(avg_overtime_pct = seq(0.05, 0.30, by = 0.05),
                         stock_option_cost = seq(5000, 25000, by = 5000),
                         net_revenue_per_employee = 250000) %>%
  cross_df() %>%
  mutate(savings = pmap_dbl(.l = list(avg_overtime_pct = avg_overtime_pct,
                                      stock_option_cost = stock_option_cost,
                                      net_revenue_per_employee = net_revenue_per_employee),
                            .f = calculate_saving_by_threshold_3_preloaded))

sensitivity_tbl_2

sensitivity_tbl_2 %>% ggplot(aes(avg_overtime_pct, stock_option_cost)) +
  geom_tile(aes(fill = savings)) +
  geom_label(aes(label = round(savings, 0) %>% scales::dollar())) +
  theme_tq() +
  theme(legend.position = "none") +
  scale_fill_gradient2(low = palette_light()[[2]], 
                       mid = "white", 
                       high = palette_light()[[1]],
                       midpoint = 0) +
  scale_x_continuous(labels = scales::percent,
                     breaks = seq(0.05, 0.3, by = 0.05)) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = "Profitability Heatmap: Expected Savings Sensitivity Analysis",
       subtitle = "How sensitivity is savings to Stock Option Cost and average overtime %",
       x = "Average Overtime Percentage",
       y = "Stock Option Cost")

# RECOMMENDATION ALGORITHM ----

## 1.0 Discretizing features  ----
recipe_obj_3<- recipe(Attrition ~., data = train_readable_tbl) %>%
  step_zv(all_predictors()) %>%
  step_mutate_at(all_of(factor_names), fn = as.factor) %>%
  step_discretize(all_numeric(), min_unique = 1) %>%
  step_dummy(all_nominal(), one_hot = TRUE) %>%
  prep()

train_corr_tbl<- bake(recipe_obj_3, new_data = train_readable_tbl) 

corr_level<- 0.06

correlation_results_tbl<- train_corr_tbl %>% select(-Attrition_No) %>%
  get_cor(Attrition_Yes, fct_reorder = T, fct_rev = T) %>%
  filter(abs(Attrition_Yes) >= corr_level) %>%
  mutate(reletionship= case_when(Attrition_Yes > 0 ~ "Support", TRUE ~ "Contradicts")) %>%
  mutate(feature_text = as.character(feature)) %>%
  separate(feature_text, into = "feature_base", sep = "_", extra = "drop") %>%
  mutate(feature_base = as_factor(feature_base) %>% fct_rev())

length_unique_groups<- correlation_results_tbl %>% pull(feature_base) %>%
  unique() %>%
  length()

correlation_results_tbl %>% ggplot(aes(Attrition_Yes, feature_base, color = reletionship)) +
  geom_point() +
  geom_label(aes(label = feature), vjust = -0.5) +
  expand_limits(x = c(-0.3, 0.3), y = c(1, length_unique_groups + 1)) +
  theme_tq() +
  scale_color_tq() +
  labs(title = "Correlation Analysis: Reccomandation Strategy Development", 
       subtitle = "Discretizing features to help identify a strategy")

## 2.0 Recommendation Strategy Development Worksheet ----

## 3.0 Recommendation Algorithm Development ----
## 3.1 Personal Development (Mentorship, Education) ----
tidy(recipe_obj, number = 3) %>% filter(str_detect(terms, "Role"))

tdt <- function(inpdt){
  transposed <- t(inpdt[,-1,with=F]);
  colnames(transposed) <- inpdt[[1]];
  transposed <- data.table(transposed, keep.rownames=T);
  setnames(transposed, 1, names(inpdt)[1]);
  return(transposed);
}

train_readable_tbl %>% select(YearsAtCompany, 
                              TotalWorkingYears, 
                              YearsInCurrentRole, 
                              JobInvolvement, 
                              JobSatisfaction, 
                              PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(personal_development_strategy = case_when(#(Worst Case) Create Personal development
    PerformanceRating == 1| JobSatisfaction == 1 | JobInvolvement <= 2 ~
      "Create Personal development",
    #(Better Case) Promote Training and Formation
    YearsAtCompany < 3 | TotalWorkingYears < 6 ~
      "Promote Training and Formation",
    #(Best Case 1) Seek Mentorship Role
    (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
      PerformanceRating >= 3 &
      JobSatisfaction == 4 ~
      "Seek Mentorship Role",
    #(Best Case 2) Seek Leadership Role
    JobInvolvement >= 3 & JobSatisfaction >= 3 & PerformanceRating >= 3 ~
      "Seek Leadership Role",
    #Catch All
    TRUE ~"Retain and Mantain")) %>%
  pull(personal_development_strategy) %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(Freq, ., fill = .)) +
  geom_bar(stat = 'identity') +
  theme_tq() +
  scale_fill_tq() +
  labs(title = 'Employee and suggested treatment',
       subtitle = 'Personal Development',
       x = 'Number of employee')
  

# 4.2 Professional Development (Promotion Readiness) ----
train_readable_tbl %>% select(JobLevel,
                              YearsInCurrentRole,
                              JobInvolvement,
                              JobSatisfaction,
                              PerformanceRating) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(professiona_development_strategy = case_when(#Ready For rotation
    YearsInCurrentRole>= 2 & JobSatisfaction <= 2 ~
      "Ready For Rotation",
    #Ready For Promotion Level 2
    JobLevel == 1 & YearsInCurrentRole >= 2 & 
      JobInvolvement >= 3 & PerformanceRating >= 3 ~
      "Ready For Promotion",
    #Ready For Promotion Level 3
    JobLevel == 2 & YearsInCurrentRole >= 2  &
      JobInvolvement >= 4 & PerformanceRating >= 3 ~
      "Ready For Promotion",
    #Ready For Promotion Level 4
    JobLevel == 3 & YearsInCurrentRole >= 3  &
      JobInvolvement >= 4 & PerformanceRating >= 3 ~
      "Ready For Promotion",
    #Ready For Promotion Level 5
    JobLevel == 4 & YearsInCurrentRole >= 4  &
      JobInvolvement >= 4 & PerformanceRating >= 3 ~
      "Ready For Promotion",
    #Incentive Specialization
    YearsInCurrentRole >= 4 & JobSatisfaction >= 4 &
      PerformanceRating >= 3 ~
      "Incetive Specialization",
    #Catch All
    TRUE ~"Retain and Mantain")) %>%
  pull(professiona_development_strategy) %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(Freq, ., fill = .)) +
  geom_bar(stat = 'identity') +
  theme_tq() +
  scale_fill_tq() +
  labs(title = 'Employee and suggested treatment',
       subtitle = 'Professional Development',
       x = 'Number of employee')


# 4.3 Work Life Balance ----

train_readable_tbl %>% select(OverTime, 
                              WorkLifeBalance, 
                              BusinessTravel, 
                              JobInvolvement, 
                              DistanceFromHome, 
                              EnvironmentSatisfaction,
                              YearsInCurrentRole) %>%
  mutate(OverTime = as.character(OverTime)) %>%
  mutate_if(is.factor, as.numeric) %>%
  mutate(Work_enviroment_development_strategy = case_when(#Improve Work-life balance: OverTime, WorkLifeBalance
    OverTime == 'Yes' | WorkLifeBalance == 1 ~
      "Improve Work-life balance",
    #Monitor business travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
    (BusinessTravel == 3 | DistanceFromHome >= 10) & WorkLifeBalance == 2 ~
      "Monitor business travel",
    #Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
    YearsInCurrentRole >= 2 & EnvironmentSatisfaction == 1 ~
      'Review Job Assignment',
    #Promote Job Engagement: JobInvolvement
    JobInvolvement <= 2 ~
      "Promote Job Engagement",
    #Catch All
    TRUE ~"Retain and Mantain")) %>%
  pull(Work_enviroment_development_strategy) %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(Freq, ., fill = .)) +
  geom_bar(stat = 'identity') +
  theme_tq() +
  scale_fill_tq() +
  labs(title = 'Employee and suggested treatment',
       subtitle = 'Work Life Balance Development',
       x = 'Number of employee')


#5.0 Recommendation functions ----

recommend_strategies<- function(data, employee_number) {
  
  data %>% filter(EmployeeNumber == employee_number) %>%
    mutate_if(is.factor, as.numeric) %>%
    #personal dev strategy
    mutate(personal_development_strategy = case_when(#(Worst Case) Create Personal development
      PerformanceRating == 1| JobSatisfaction == 1 | JobInvolvement <= 2 ~
        "Create Personal development",
      #(Better Case) Promote Training and Formation
      YearsAtCompany < 3 | TotalWorkingYears < 6 ~
        "Promote Training and Formation",
      #(Best Case 1) Seek Mentorship Role
      (YearsInCurrentRole > 3 | YearsAtCompany >= 5) &
        PerformanceRating >= 3 &
        JobSatisfaction == 4 ~
        "Seek Mentorship Role",
      #(Best Case 2) Seek Leadership Role
      JobInvolvement >= 3 & JobSatisfaction >= 3 & PerformanceRating >= 3 ~
        "Seek Leadership Role",
      #Catch All
      TRUE ~"Retain and Mantain")) %>%
    #Professional dev strategy
    mutate(professiona_development_strategy = case_when(#Ready For rotation
      YearsInCurrentRole>= 2 & JobSatisfaction <= 2 ~
        "Ready For Rotation",
      #Ready For Promotion Level 2
      JobLevel == 1 & YearsInCurrentRole >= 2 & 
        JobInvolvement >= 3 & PerformanceRating >= 3 ~
        "Ready For Promotion",
      #Ready For Promotion Level 3
      JobLevel == 2 & YearsInCurrentRole >= 2  &
        JobInvolvement >= 4 & PerformanceRating >= 3 ~
        "Ready For Promotion",
      #Ready For Promotion Level 4
      JobLevel == 3 & YearsInCurrentRole >= 3  &
        JobInvolvement >= 4 & PerformanceRating >= 3 ~
        "Ready For Promotion",
      #Ready For Promotion Level 5
      JobLevel == 4 & YearsInCurrentRole >= 4  &
        JobInvolvement >= 4 & PerformanceRating >= 3 ~
        "Ready For Promotion",
      #Incentive Specialization
      YearsInCurrentRole >= 4 & JobSatisfaction >= 4 &
        PerformanceRating >= 3 ~
        "Incetive Specialization",
      #Catch All
      TRUE ~"Retain and Mantain")) %>%
    #Work Env strategy
    mutate(Work_enviroment_development_strategy = case_when(#Improve Work-life balance: OverTime, WorkLifeBalance
      OverTime == 'Yes' | WorkLifeBalance == 1 ~
        "Improve Work-life balance",
      #Monitor business travel: BusinessTravel, DistanceFromHome, WorkLifeBalance
      (BusinessTravel == 3 | DistanceFromHome >= 10) & WorkLifeBalance == 2 ~
        "Monitor business travel",
      #Review Job Assignment: EnvironmentSatisfaction, YearsInCurrentRole
      YearsInCurrentRole >= 2 & EnvironmentSatisfaction == 1 ~
        'Review Job Assignment',
      #Promote Job Engagement: JobInvolvement
      JobInvolvement <= 2 ~
        "Promote Job Engagement",
      #Catch All
      TRUE ~"Retain and Mantain")) %>%
    select(EmployeeNumber, 
           personal_development_strategy, 
           professiona_development_strategy,
           Work_enviroment_development_strategy)
}

recommend_strategies(train_readable_tbl, 19)
