library(tidyverse)
library(lubridate)
library(DescTools)



edx<- readRDS(file="edx.rds")
validation<- readRDS(file = "validation.rds")

sum(edx$rating==0)

sum(edx$rating == 3)

length(unique(edx$movieId))

length(unique(edx$userId))


edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

edx%>%group_by(rating)%>%
  summarize(given_rating = n())%>%
  arrange(desc(given_rating))

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


