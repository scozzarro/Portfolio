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
#    winners prediction and calculate MSE as KPI.
#######################################################################

#######################################################################
#PART 1: EDA
#######################################################################

#####################################################
# load data sets (train_V2, test_V2) and libraries (tidyverse,data.table,corrplot)

#####################################################



#Load packages.

library(tidyverse)
library(data.table)
library(corrplot)



#Load train and test set

train_set<- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/train_V2.csv")
test_set<- fread("C:/Users/elekt/OneDrive/Documenti/datascience_capstone/FPS Machine Learning project/Data input/test_V2.csv")


#An overview of the train_set

str(train_set)

#An overview on players and games

length(unique(train_set$Id))
length(unique(train_set$matchId))

#Take a deeper observation on the main event an multiplayer fps can have

summary(train_set$assists)
summary(train_set$damageDealt)
summary(train_set$headshotKills)
summary(train_set$kills)
summary(train_set$matchDuration)
summary(train_set$walkDistance)
summary(train_set$winPlacePerc)

#Remove NA

train_set$winPlacePerc[is.na(train_set$winPlacePerc)]<- 0



#Observe the main feature distribution in train_set

train_set %>%
  ggplot(aes(assists))+
  geom_density(color="#69b3a2", fill="#69b3a2")+
  ggtitle("Assist density")
  
train_set%>%
  ggplot(aes(damageDealt))+
  geom_density(color="#69b3a2", fill="#69b3a2")+
  ggtitle("Damage dealt density")


train_set%>%
  ggplot(aes(headshotKills))+
  geom_density(color="#69b3a2", fill="#69b3a2")+
  ggtitle("Head shot kill density")

train_set %>%
  ggplot(aes(kills))+
  geom_density(color="#69b3a2", fill="#69b3a2") +
  ggtitle("Kilss density")

train_set%>%
  ggplot(aes(winPlacePerc)) +
  geom_density(color="#69b3a2", fill="#69b3a2")+
  ggtitle("Win placement density")

#improving Win placement viz
train_set%>%
  ggplot(aes(winPlacePerc)) +
  geom_histogram(bins = 20, color="white", fill="#69b3a2") +
  ggtitle("Win placement density")

###########################

train_set%>%
  ggplot(aes(walkDistance)) +
  geom_density(color="#69b3a2", fill="#69b3a2") +
  ggtitle("Walk distance density")


#Investigate correlation between features and target 

train_sample<- train_set[,c(4:15,17:29)]
train_corr<- as.data.frame(lapply(train_sample, as.numeric))

corrplot(cor(train_corr), method = "circle")

#Match ranking (win place) with travelled distance (walk, swim, drive)

trav<- train_set %>%
  mutate(winPlacePerc_dec = ntile(winPlacePerc, 10)) %>%
  group_by(winPlacePerc_dec) %>%
  summarize(walk = mean(walkDistance),
            swim = mean(swimDistance),
            drive = mean(rideDistance)) %>%
  ungroup() %>%
  reshape2::melt(., measure.vars= c("walk", "swim", "drive"), variable.name = "travel_mode", value.name = 'avg_distance_trav' ) %>%
         as.data.table()

levels<- trav$winPlacePerc_dec %>%
  unique() %>%
  sort()

trav$winPlacePerc_dec<- factor(trav$winPlacePerc_dec, levels = levels)

trav %>% ggplot(aes(winPlacePerc_dec, avg_distance_trav, fill = travel_mode)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_grid(travel_mode ~ . , scales = "free") +
  ggtitle("Traveled distance by ranking place decile")


#Match ranking (win place) with Kills

kills<- train_set %>%
  mutate(winPlacePerc_dec = ntile(winPlacePerc, 10)) %>%
  group_by(winPlacePerc_dec) %>%
  summarize(kills = mean(kills)) %>%
  ungroup() %>%
  reshape2::melt(., measure.vars= c("kills"), value.name = 'avg_kills' ) %>%
  as.data.table()

levels2<- kills$winPlacePerc_dec %>%
  unique() %>%
  sort()

kills$winPlacePerc_dec<- factor(kills$winPlacePerc_dec, levels = levels2)

kills %>% ggplot(aes(winPlacePerc_dec, avg_kills)) +
  geom_bar(stat = "identity", position = "dodge") +
  ggtitle("Kills by ranking place decile")


#Head shot analysis

train_set %>%
  mutate(headShot_rate = ifelse(kills == 0, 0, headshotKills / kills)) %>%
  ggplot(aes(headShot_rate)) + 
  geom_histogram(bins = 20, color="white", fill="#69b3a2") + 
  ggtitle("Head shot rate density")

#Match head shot with ranking

train_set %>%
  select(Id, headshotKills, kills, winPlacePerc) %>%
  mutate(winPlacePerc = round(winPlacePerc,1)) %>%
  group_by(winPlacePerc) %>%
  summarize(aim_accuracy = mean(ifelse(kills == 0, 0, headshotKills / kills))) %>%
  as.data.table() %>%
  ggplot(aes(winPlacePerc, aim_accuracy)) +
  geom_point()+
  geom_smooth(method = "loess") +
  ggtitle("Match ranking with head shots")


#Explore if the number of teams during the same game influence the final ranking

summary(train_set$numGroups)

#what happen when there is only one team in the game?

train_set %>%
  filter(numGroups == 1) %>%
  mutate(groupId = order(groupId)) %>%
  group_by(matchId) %>% 
  select(matchId, groupId, winPlacePerc, numGroups) %>%
  head(20)



