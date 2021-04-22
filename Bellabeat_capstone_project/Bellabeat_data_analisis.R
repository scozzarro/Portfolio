# Bellabeat_capstone_project ----
#analyze smart device usage data in order to gain insight into how consumers 
#use non-Bellabeat smart devices

# 1.0 library & tools ----
library(tidyverse)
library(skimr)
library(visdat)
library(corrplot)
library(timetk)
library(RColorBrewer)
library(scales)
library(factoextra)
library(corrplot)
library(gridExtra)



#2.0 Data ----

#importing
daily_activity_data<- read_csv("Data/dailyActivity_merged.csv")

heart_rate_seconds_data<- read_csv("Data/heartrate_seconds_merged.csv")

hourly_calories_data<- read_csv("Data/hourlyCalories_merged.csv")
hourly_intensities_data<- read_csv("Data/hourlyIntensities_merged.csv")
hourly_steps_data<- read_csv("Data/hourlySteps_merged.csv")

sleep_day_data<- read_csv("Data/sleepDay_merged.csv")

weight_loss_data<- read_csv("Data/weightLogInfo_merged.csv")

#Data sanity check
skim_without_charts(daily_activity_data)
skim_without_charts(heart_rate_seconds_data)
skim_without_charts(hourly_calories_data)
skim_without_charts(hourly_intensities_data)
skim_without_charts(hourly_steps_data)
skim_without_charts(sleep_day_data)
skim_without_charts(weight_loss_data)


#Data fixing

daily_activity_data$ActivityDate<- as.Date(daily_activity_data$ActivityDate, format = c('%m/%d/%Y'))

heart_rate_seconds_data$Day<- as.Date(heart_rate_seconds_data$Time, format = c('%m/%d/%Y'))
heart_rate_seconds_data$Time<- strptime(heart_rate_seconds_data$Time, format = "%m/%d/%Y %I:%M:%S %p")
heart_rate_seconds_data$Time<- format(heart_rate_seconds_data$Time, format = '%I:%M:%S %p')

hourly_calories_data$Day<- as.Date(hourly_calories_data$ActivityHour, format = c('%m/%d/%Y'))
hourly_calories_data$ActivityHour<- strptime(hourly_calories_data$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_calories_data$ActivityHour<- format(hourly_calories_data$ActivityHour, format = '%I:%M:%S %p')

hourly_intensities_data$Day<- as.Date(hourly_intensities_data$ActivityHour, format = c('%m/%d/%Y'))
hourly_intensities_data$ActivityHour<- strptime(hourly_intensities_data$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_intensities_data$ActivityHour<- format(hourly_intensities_data$ActivityHour, format = '%I:%M:%S %p')

hourly_steps_data$Day<- as.Date(hourly_steps_data$ActivityHour, format = c('%m/%d/%Y'))
hourly_steps_data$ActivityHour<- strptime(hourly_steps_data$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p")
hourly_steps_data$ActivityHour<- format(hourly_steps_data$ActivityHour, format = '%I:%M:%S %p')

sleep_day_data$SleepDay<- as.Date(sleep_day_data$SleepDay, format = c('%m/%d/%Y'))

weight_loss_data$Date<- as.Date(weight_loss_data$Date, format = c('%m/%d/%Y'))


#3.0 EDA ----

##3.1 Activity analysis ----

length(unique(daily_activity_data$Id))

daily_activity_data$training_day<- ifelse(daily_activity_data$VeryActiveMinutes>= 10, 1, 0)

daily_activity_prcs<- daily_activity_data %>% group_by(Id) %>% 
                                              summarise(Usage = n(), 
                                              avg_workout_time = mean(VeryActiveMinutes+FairlyActiveMinutes), 
                                              workouts = sum(training_day),
                                              first_record = min(ActivityDate),
                                              last_record = max(ActivityDate),
                                              avg_distance = mean(TotalDistance),
                                              avg_steps = mean(TotalSteps))

daily_activity_prcs %>% ggplot(aes(Usage)) +
                        geom_density(col='lightseagreen', fill = 'grey', alpha = 0.7, size= 1 ) +
                        labs(title = 'Days of usage distribution', subtitle = 'Usage is express in days') +
                        theme_minimal()
  

daily_activity_prcs %>% ggplot(aes(workouts)) +
                        geom_density(col='lightseagreen', fill = 'grey', alpha = 0.7, size = 1) +
                        labs(title = '1 month workouts distribution', subtitle = '(workouts are counted if user registered >10 minutes of Very Active activity)') +
                        theme_minimal()


most_cmn_training_day<- daily_activity_data %>% filter(training_day == 1) %>%
                        count(format(ActivityDate, format ='%A'))

most_cmn_training_day[order(most_cmn_training_day$n, decreasing = TRUE),]

daily_activity_data %>% filter(training_day == 1) %>%
                        ggplot(aes(format(ActivityDate, format ='%A'), fill = format(ActivityDate, format ='%A'))) +
                        geom_bar(stat = 'count') +
                        scale_fill_brewer(palette = 'Set3') +
                        xlab('Days of week') +
                        labs(title = 'Most common day for training') +
                        theme_minimal() +
                        theme(legend.position = "none")

daily_activity_data %>% group_by(Id) %>%
                        mutate(Workout_time = VeryActiveMinutes+FairlyActiveMinutes) %>%
                        summarise(Usage = n(), ActivityDate, Workout_time) %>%
                        filter(Usage >= 15) %>%
                        ggplot(aes(ActivityDate, Workout_time)) +
                        geom_point() +
                        geom_smooth(method = 'loess', color = 'lightseagreen') +
                        facet_wrap(~Id, ) +
                        labs(title = 'Workout time trends over time') +
                        theme_minimal() +
                        theme(strip.background = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.x = element_text(angle=45))


daily_activity_data %>% group_by(Id) %>%
                        summarise(Usage = n(), SedentaryMinutes, ActivityDate) %>%
                        filter(Usage >= 15) %>%
                        ggplot(aes(ActivityDate, SedentaryMinutes)) +
                        geom_point() +
                        geom_smooth(method = 'loess', color = 'lightseagreen') +
                        facet_wrap(~Id) +
                        labs(title = 'Sedentary time trends over time') +
                        theme_minimal() +
                        theme(strip.background = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.x = element_text(angle=45))

daily_activity_data %>% group_by(Id) %>%
                        ggplot(aes(ActivityDate, Calories)) +
                        geom_point() +
                        geom_smooth(method = 'loess', color = 'lightseagreen') +
                        facet_wrap(~Id) +
                        labs(title = 'Clories burned trends over time')

daily_activity_clust<- daily_activity_data %>% group_by(Id) %>%
                                               summarise(workout = sum(training_day),
                                                         avg_steps = mean(TotalSteps),
                                                         avg_workout_time = mean(VeryActiveMinutes + FairlyActiveMinutes),
                                                         avg_calories = mean(Calories))


#fviz_nbclust(scale(daily_activity_clust[,2:5]), kmeans, method = "gap_stat")

cluster<- kmeans(scale(daily_activity_clust[,2:5]), 2, nstart = 25)

fviz_cluster(cluster, data = daily_activity_clust[,2:5], 
             palette = c("#2E9FDF", "#00AFBB"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_minimal())

daily_activity_clust$Cluster<- as.factor(cluster$cluster)

daily_activity_clust %>% group_by(Cluster) %>% 
                         summarise(Number_of_user = n(),
                                   workout = mean(workout),
                                   steps = mean(avg_steps)) %>%
                         ggplot(aes(Cluster, Number_of_user, fill = Cluster)) +
                         geom_bar(stat = 'identity') +
                         scale_fill_brewer(palette = 'Set3') +
                         labs(title = 'User cluster dimension') +
                         theme_minimal()

##3.2 Sleep Analysis ----

length(unique(sleep_day_data$Id))

sleep_day_data %>% group_by(Id) %>%
                   ggplot(aes(SleepDay, TotalMinutesAsleep)) +
                   geom_point() +
                   geom_smooth(method = 'loess', color = 'lightseagreen') +
                   facet_wrap(~Id, ) +
                   labs(title = 'Sleeping time over time') +
                   theme_minimal() +
                   theme(strip.background = element_blank(),
                          strip.text.x = element_blank(),
                          axis.text.x = element_text(angle=45))

## 3.3 Sleep and Activity Analysis ----


sleep_day_prc<- sleep_day_data %>% group_by(Id) %>%
                   summarise(Usage = n(), avg_sleep_time = mean(TotalMinutesAsleep)) %>%
                   filter(Usage >= 15)

Activity_sleep_merged_data<- inner_join(daily_activity_clust, sleep_day_prc, by = 'Id')
Activity_sleep_merged_data<- Activity_sleep_merged_data[,-7]

#Correlation analysis

corrplot(cor(Activity_sleep_merged_data[,-c(1,6)]), method = 'circle', addCoef.col = "black")

## 3.4 Weight loss Analysis ----

length(unique(weight_loss_data$Id))

prop.table(table(weight_loss_data$IsManualReport))

weight_loss_data %>% ggplot(aes(IsManualReport, fill = IsManualReport)) +
                     geom_bar(stat = 'count') +
                     labs(title = 'Manual weight reporting stat', subtitle = '61% of user reported weight manually') +
                     theme_minimal() +
                     theme(legend.position = 'none')

for (i in 1:length(weight_loss_data$BMI)) {
  if(weight_loss_data$BMI[i] <= 18.5){
    weight_loss_data$BMI_analysis[i] = 'Under weight'  
  } else if (18.5 < weight_loss_data$BMI[i] & weight_loss_data$BMI[i] <= 25) {
    weight_loss_data$BMI_analysis[i] = 'Normal weight'  
  } else if (25 < weight_loss_data$BMI[i] & weight_loss_data$BMI[i] <= 30) {
    weight_loss_data$BMI_analysis[i] = 'Over weight'
  } else if (weight_loss_data$BMI[i]> 30) {
    weight_loss_data$BMI_analysis[i] = 'Obese'
  }
}


weight_loss_prc<- weight_loss_data %>% group_by(Id) %>%
                                      summarise(First_record = min(Date),
                                                Start_Weight = WeightKg[which(Date == min(Date))],
                                                Last_record = max(Date),
                                                End_weight = WeightKg[which(Date == max(Date))],
                                                Weight_difference = abs(Start_Weight - End_weight),
                                                Start_BMI_val = BMI[which(Date == min(Date))],
                                                Final_BMI_value = BMI[which(Date == max(Date))],
                                                Start_BMI_condition = BMI_analysis[which(Date == min(Date))],
                                                Final_BMI_condition = BMI_analysis[which(Date == max(Date))])

best_weight_dif<- max(weight_loss_prc$Weight_difference)

avg_weight_dif<- mean(weight_loss_prc$Weight_difference)

start_weight_dist_plot<- weight_loss_prc %>% 
                         ggplot(aes(Start_Weight)) +
                         geom_density(col='lightseagreen', fill = 'grey', alpha = 0.7, size= 1 ) +
                         labs(title = 'User weight distribution at the start', subtitle = 'Weight is express in Kg') +
                         theme_minimal()

final_weight_dist_plot<- weight_loss_prc %>% 
                         ggplot(aes(End_weight)) +
                         geom_density(col='lightseagreen', fill = 'grey', alpha = 0.7, size= 1 ) +
                         annotate('text', x = 115, y = 0.015, label = 'Average weight loss = 0,46 Kg') +
                         annotate('text', x = 113, y = 0.0125, label = 'Max weight loss = 1.8 Kg') +
                         labs(title = 'User weight distribution at the end', subtitle = 'Weight is express in Kg') +
                         theme_minimal()

grid.arrange(start_weight_dist_plot, final_weight_dist_plot, ncol = 1)



weight_loss_data %>% group_by(Id) %>%
                     summarise(Start_BMI_condition = BMI_analysis[which(Date == min(Date))],
                               Final_BMI_condition = BMI_analysis[which(Date == max(Date))]) %>%
                     ggplot(aes(Start_BMI_condition, fill = Start_BMI_condition)) +
                     geom_bar() +
                     geom_text(aes(label = ..count..),
                     stat='count',
                     nudge_y = 0.125,
                     vjust = -1) +
                     labs(title = 'User BMI distribution') +
                     theme_minimal()

## 3.5 Weight loss and Activity analysis ----



Activity_weight_loss_merged<- inner_join(daily_activity_clust, weight_loss_prc, by = 'Id')

