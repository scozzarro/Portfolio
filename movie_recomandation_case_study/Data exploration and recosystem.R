library(tidyverse)
library(lubridate)
library(DescTools)
library(recosystem)
library(doParallel)


train_set<- edx
test_set<- validation

train_set<- train_set%>%
  mutate(timestamp = as_datetime(timestamp))


train_set%>% 
  mutate(year = year(timestamp))%>%
  group_by(year)%>%
  summarize(n_rating = n())%>%
  print(n = 15)%>%
  ggplot(aes(year,n_rating))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2)+
  geom_point(size=3, color="#69b3a2") +
  ggtitle("Number of rates per year")

train_set%>%
  group_by(rating)%>%
  summarize(dens = n())%>%
  ggplot(aes(rating,dens))+
  geom_area( fill="#69b3a2", alpha=0.4) +
  geom_line(color="#69b3a2", size=2)+
  geom_point(size=3, color="#69b3a2") +
  ggtitle("Rating distribution")

train_set%>%
  group_by(userId)%>%
  summarize(n_rating = n())%>%
  summary()

train_set %>% filter(genres %like% "%Drama%") %>% summarize(n())
train_set %>% filter(genres %like% "%Comedy%") %>% summarize(n())
train_set %>% filter(genres %like% "%Thriller%") %>% summarize(n())
train_set %>% filter(genres %like% "%Romance%") %>% summarize(n())

train_set%>%
  group_by(genres)%>%
  summarize(movies_per_genre = n())%>%
  top_n(10, movies_per_genre)

train_set %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 50000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

train_set%>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


RMSE<- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}



train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

# Create the model object
r <-  recosystem::Reco()

# Select the best tuning parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 6, niter = 10))

# Train the algorithm  
r$train(train_data, opts = c(opts$min, nthread = 6, niter = 20))

y_hat <-  r$predict(test_data, out_memory())

RMSE(test_set$rating, y_hat)
