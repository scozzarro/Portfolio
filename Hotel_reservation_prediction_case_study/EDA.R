library(tidyverse)
library(ROSE)
library(RColorBrewer)
library(viridis)


#=================================
#  Option
#=================================
theme_set(theme_minimal())


#=================================
#  Download
#=================================

download.file(url = "https://raw.githubusercontent.com/MGCodesandStats/hotel-modelling/master/notebooks%20and%20datasets/classification/H1full.csv", destfile = "H1full.csv")
download.file(url = "https://raw.githubusercontent.com/MGCodesandStats/hotel-modelling/master/notebooks%20and%20datasets/classification/H2full.csv", destfile = "H2full.csv")


#=================================
#  Data structure
#=================================

data<- read.csv("H1full.csv")

dim(data)

str(data)

summary(data)


#=================================
#  Data manipulation
#=================================

data$IsCanceled<- as.factor(data$IsCanceled)
data$ArrivalDateYear<- as.factor(data$ArrivalDateYear)
data$ArrivalDateWeekNumber<- as.factor(data$ArrivalDateWeekNumber)
data$ArrivalDateDayOfMonth<- as.factor(data$ArrivalDateDayOfMonth)
data$ArrivalDateMonth<- as.factor(data$ArrivalDateMonth)
data$Meal<- as.factor(data$Meal)
data$Country<- as.factor(data$Country)
data$MarketSegment<- as.factor(data$MarketSegment)
data$DistributionChannel<- as.factor(data$DistributionChannel)
data$IsRepeatedGuest<- as.logical(data$IsRepeatedGuest)
data$PreviousBookingsNotCanceled<- as.logical(data$PreviousBookingsNotCanceled)
data$ReservedRoomType<- as.factor(data$ReservedRoomType)
data$AssignedRoomType<- as.factor(data$AssignedRoomType)
data$DepositType<- as.factor(data$DepositType)
data$Agent<- as.factor(data$Agent)
data$Company<- as.factor(data$Company)
data$CustomerType<- as.factor(data$CustomerType)
data$RequiredCarParkingSpaces<- as.logical(data$RequiredCarParkingSpaces)
data$ReservationStatus<- as.factor(data$ReservationStatus)
data$ReservationStatusDate<- as.Date(data$ReservationStatusDate)


summary(data)


#=================================
#  EDA
#=================================

data %>%
  ggplot(aes(IsCanceled)) +
  geom_density(aes(fill = IsCanceled)) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("IsCanceled (target value) density")

data %>%
  ggplot(aes(IsCanceled)) +
  geom_bar(aes(fill = IsCanceled)) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("IsCanceled (target value) density")

topcountry<- data %>%
              group_by(Country) %>%
              summarise(count = n())

topcountry<- top_n(topcountry, 20)

topcountry %>%
  ggplot(aes(Country, count)) +
  geom_bar(fill = "darkgoldenrod1", stat = "identity") +
  coord_flip() +
  ggtitle("Reservations by country")

data %>%
  ggplot(aes(MarketSegment)) +
  geom_bar(aes(fill = MarketSegment)) +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Reservations by market segment")

data %>%
  ggplot(aes(IsRepeatedGuest)) +
  geom_bar(aes(fill = IsRepeatedGuest)) +
  scale_fill_brewer(palette = "Paired") +
  ggtitle("New customer vs Loyal customer")

data %>% 
  ggplot(aes(CustomerType)) +
  geom_bar(aes(fill = CustomerType)) +
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Reservations by type of customer")

data %>% 
  ggplot(aes(ArrivalDateMonth)) +
  geom_bar(aes(fill = ArrivalDateMonth)) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  labs(y = "Bookings") +
  theme(axis.title.y = element_blank())+
  geom_hline(yintercept = 3338, color="black", size=0.5,linetype="dotted") +
  ggtitle("Most booked month")

data$Deltaroomtype<- ifelse(as.character(data$ReservedRoomType) == as.character(data$AssignedRoomType), 1, 0)

data$Deltaroomtype<- as.logical(data$Deltaroomtype)

summary(data$Deltaroomtype)

data %>%
  ggplot(aes(Deltaroomtype)) +
  geom_bar(aes(fill = Deltaroomtype)) +
  scale_fill_brewer(palette = "Dark2") +
  ggtitle("Is the booked room the one that was assigned?")

country_canc<- data %>%
               group_by(Country, IsCanceled) %>%
               summarise(count = n())

top_country_index<- which(country_canc$Country %in% topcountry$Country)
  
country_canc<- country_canc[top_country_index,]

country_canc %>%
  ggplot(aes(x = Country, y = count)) +
  geom_bar(aes(fill = IsCanceled), stat = "identity") +
  scale_fill_manual(values = c("deepskyblue3","brown3"))+
  coord_flip() +
  ggtitle("Most country with cancelled reservations")


data %>%
  group_by(ArrivalDateMonth, IsCanceled) %>%
  summarise(count = n()) %>%
  ggplot(aes(ArrivalDateMonth, count)) +
  geom_bar(aes(fill = IsCanceled), stat = "identity") +
  scale_fill_manual(values = c("deepskyblue3","brown3")) +
  coord_flip() +
  ggtitle("Cancelled reservations by month")

data %>%
  ggplot(aes(ReservedRoomType)) +
  geom_bar(aes(fill = ReservedRoomType)) +
  scale_fill_brewer(palette = "Set3") +
  ggtitle("Most wanted room type")

data %>%
  group_by(ReservedRoomType, Deltaroomtype) %>%
  summarise(count = n()) %>%
  ggplot(aes(ReservedRoomType, count)) +
  geom_bar(aes(fill = Deltaroomtype), stat = "identity") +
  scale_fill_manual(values = c("brown3","deepskyblue3")) +
  ggtitle("Customer room type reservation sotisfied by room type")

data %>% 
  group_by(Deltaroomtype, IsCanceled) %>%
  summarise(count = n()) %>%
  filter(IsCanceled == 1) %>%
  ggplot(aes(Deltaroomtype, count)) +
  geom_bar(aes(fill = Deltaroomtype), stat = "identity") +
  scale_fill_manual(values = c("brown3","deepskyblue3")) +
  theme(axis.title.x = element_blank(), legend.position = "none")+
  ggtitle("Customer canceled reservations because they didn't get the room they wanted?", 
          subtitle = "False mean the dint't get the room the ask for")
  
