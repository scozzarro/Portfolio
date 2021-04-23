#######################################################################
# HR  Project 
# 26 Dic 2020
# Gabrielmaria Scozzarro
#    
#######################################################################

# 0.1 Tools ----
library(tidyverse)
library(corrplot)
library(kableExtra)
library(scales)
library(lubridate)
library(reshape2)
library(plyr)




# 1.0 Import data ----
hr_data<- read.csv("Data/HRDataset_v14.csv")


# 1.1 Data summary ----
glimpse(hr_data)
summary(hr_data)

# 1.2 Fix data ---- 
hr_data$ManagerID[is.na(hr_data$ManagerID)]<- 0

hr_data$MaritalDesc<- as_factor(hr_data$MaritalDesc)

useless_col<- c("GenderID", "MaritalStatusID")
hr_data<- hr_data[, -which(colnames(hr_data) %in% useless_col)]

hr_data$DateofHire<- as.POSIXlt(hr_data$DateofHire,format="%m/%d/%Y")
hr_data$DateofHire<- as.Date(hr_data$DateofHire)

hr_data$DateofTermination<- as.POSIXlt(hr_data$DateofTermination,format="%m/%d/%Y")
hr_data$DateofTermination<- as.Date(hr_data$DateofTermination)

hr_data$DOB<- as.Date(hr_data$DOB, "%m/%d/%y")
hr_data$DOB<- as.Date(ifelse(hr_data$DOB > "2020-01-01", format(hr_data$DOB, "19%y-%m-%d"), format(hr_data$DOB)))

hr_data$Age<- year(Sys.Date())-year(hr_data$DOB)
# 2.0 EDA ----

# Gender analysis
hr_data %>% ggplot(aes(Sex, fill = Sex)) +
            geom_bar(stat = "count") +
            ggtitle("Gender presence") +
            theme_minimal() +
            theme(legend.position =  "none")

  
hr_data %>% ggplot(aes(Age, fill = Sex)) +
            geom_density(alpha = 0.3) +
            theme_minimal()

all_roles = unique(hr_data$Position)
hr_data$Position = factor(hr_data$Position, levels = all_roles)
hr_data$order = as.numeric(hr_data$Position)/100 

hr_data %>% filter(!is.na(DateofTermination)) %>%
            ggplot(aes(x = factor(order), fill = Sex)) + 
            geom_bar(subset=.(Sex == "M")) + 
            geom_bar(subset=.(Sex == "F"), aes(y=..count..)) + 
            scale_x_discrete(labels = all_roles) +
            xlab("Role") +
            coord_flip() +
            theme(text = element_text(size = 16)) +
            ggtitle("Gender presence by role up to date") +
            theme_minimal()

hr_data %>% ggplot(aes(x = factor(order), fill = Sex)) + 
            geom_bar(subset=.(Sex == "M")) + 
            geom_bar(subset=.(Sex == "F"), aes(y=..count..)) + 
            scale_x_discrete(labels = all_roles) +
            xlab("Role") +
            coord_flip() +
            theme(text = element_text(size = 16)) +
            facet_wrap(~year(DateofHire)) +
            ggtitle("Enrolment by role and gender since the incorporation") +
            theme_minimal()

hr_data %>% group_by(Sex, year = year(DateofHire)) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(year, n, color = Sex)) +
            geom_line(aes(linetype=Sex), size = 1) +
            geom_point(size = 2) +
            ggtitle("Hiring trend by gender") +
            theme_minimal()


hr_data %>% group_by(Sex, Position) %>%
            dplyr::summarise(avgsal = mean(Salary)) %>%
            ggplot(aes(avgsal, Position, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Average salary by gender and role") +
            theme_minimal()

hr_data %>% filter(!is.na(DateofTermination)) %>%
            group_by(Sex) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, Sex, fill = Sex)) +
            geom_bar(stat = "identity") +
            ggtitle("Employes contract terminated by gender") +
            theme_minimal() +
            theme(legend.position = "none")

hr_data %>% group_by(Sex, RecruitmentSource) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, RecruitmentSource, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Recruitment source performance by gender") +
            theme_minimal()

hr_data %>% filter(!is.na(DateofTermination)) %>%
            group_by(Sex, TermReason) %>%
            dplyr::summarise(n = n()) %>%
            ggplot(aes(n, TermReason, fill = Sex)) +
            geom_bar(stat = "identity", position = "dodge") +
            ggtitle("Contract termination reasons by gender") +
            theme_minimal()


hr_data %>% group_by(Sex) %>%
            ggplot(aes(Sex, EmpSatisfaction, fill = Sex)) +
            geom_violin(width=1, alpha = 0.7) +
            geom_boxplot(width=0.1, color="black", alpha=0.2) +
            theme_minimal() +
            theme(legend.position="none") +
            ggtitle("Employes satisfaction by gender")

hr_data$Salary_level<- 0

for (i in 1:nrow(hr_data)) {
  if (hr_data$Salary[i] > mean(hr_data$Salary) + 2*sd(hr_data$Salary)) {
    hr_data$Salary_level[i]<- "High"
  }
  else if (hr_data$Salary[i] < mean(hr_data$Salary) + sd(hr_data$Salary)) {
    hr_data$Salary_level[i]<- "Medium"
  } 
  else {
    hr_data$Salary_level[i]<- "Low"
  }
  }

hr_data %>% group_by(Sex, Salary_level) %>%
            ggplot(aes(Salary_level, EmpSatisfaction, fill = Sex)) +
            geom_boxplot() +
            ggtitle("Employes satisfaction level by gender and salary level")
