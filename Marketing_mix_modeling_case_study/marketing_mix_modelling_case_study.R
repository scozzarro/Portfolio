#Tools ----

library(tidyverse)
library(scales)
library(gridExtra)
library(skimr)


library(hrbrthemes)
library(Cairo)
library(extrafont)
library(RColorBrewer)

extrafont::loadfonts()


#importing data ----
email_mk_data<- read.csv("Data/marketing_data.csv",header = TRUE, sep = ";")


#sanity check ----

glimpse(email_mk_data)

skim_without_charts(email_mk_data)

#fixing ----
colnames(email_mk_data)[1] <- "Campaign"

#EDA ----

email_mk_data %>% ggplot(aes(Campaign, Spend, fill = Campaign)) +
                  geom_bar(stat = "identity") +
                  scale_y_continuous(labels = dollar) +
                  scale_fill_brewer(palette = "Set3") +
                  theme_ipsum() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 0.75), 
                        legend.position = "none") +
                  xlab("") +
                  labs(title = "Invesment in different marketing campaign")

email_mk_data %>% ggplot(aes(Campaign, Return, fill = Campaign)) +
                  geom_bar(stat = "identity") +
                  scale_y_continuous(labels = dollar) +
                  scale_fill_brewer(palette = "Set3") +
                  theme_ipsum() +
                  theme(axis.text.x = element_text(angle = 45, hjust = 0.75), 
                        legend.position = "none") +
                  xlab("") +
                  labs(title = "ROI in different marketing campaign")

#MMM ----

email_mk_data %>% ggplot(aes(Spend, Return)) +
                  geom_point(size = 2) +
                  geom_smooth(method = "lm") +
                  scale_x_continuous(labels = dollar) +
                  scale_y_continuous(labels = dollar) +
                  theme_ipsum() +
                  labs(title = "Email campaing Spent - Return ratio")
  

#ADBUDG model

Ufun<-function(x, Spend, Return) {
  predictedReturn = x[2] + (x[1] - x[2])*((Spend^x[3])/(x[4] + (Spend^x[3])))
  errorSq = (predictedReturn - Return)^2
  sumSqError = sum(errorSq)
  return(sumSqError)
}
# The “x” variable is a vector of length four and represents the four parameters 
# of the ADBUDG function.  Spend and Return are also vectors which correspond with 
# the Spend and Return data from your data file.  Their length will be the same as 
# he number of data points in your data file.  The temp1 object calculates the predicted 
# Return from the ADBUDG equation.  That vector is then used in the next line when we 
# calculate the squared error for our predicted values, which are stored in the temp2 object.
# Lastly, we sum the temp2 object to get a single value for the sum of squared errors, which 
# is stored in the temp3 object.  The temp3 object is the return object for this function.  
# When we minimize this function, we are essentially minimizing the sum of squared error, 
# which is actually the same thing that is done in linear regression.


startValVec = c(25000,100,1.5,100000)
minValVec = c(0,0,1.01,0)
maxValVec = c(500000, 500000, 2, 10000000)

optim.parms<-nlminb(objective=Ufun,start=startValVec,
                    lower=minValVec,
                    upper=maxValVec,
                    control=list(iter.max=1000000,eval.max=20000),
                    Spend = email_mk_data$Spend,
                    Return = email_mk_data$Return)
optim.parms


a = optim.parms$par[1]
b = optim.parms$par[2]
c = optim.parms$par[3]
d = optim.parms$par[4]

curveDFx = seq(from=0, to=max(email_mk_data$Spend)*2, length.out=10000)
curveDFy = b+(a-b)*((curveDFx^c)/(d+(curveDFx^c)))
curveDF = data.frame(Spend = curveDFx, Return = curveDFy)

maxX = 1.05*max(curveDFx, max(email_mk_data$Spend))
maxY = 1.05*max(curveDFy, max(email_mk_data$Return))

myPlotDataDF = data.frame(Return = email_mk_data$Return, Spend = email_mk_data$Spend)
optimLineDF = data.frame(Spend = curveDFx, Return = curveDFy)

myPlotDataDF %>% ggplot(aes(Spend, Return)) +
                 geom_point(size = 2) +
                 geom_line(data = optimLineDF, aes(x = Spend, y = Return, color = "darkgreen")) +
                 scale_color_manual(labels = "Optimized ADBUDG Fit",values=c('darkgreen')) +
                 coord_cartesian(ylim = c(0,maxY), xlim = c(0,maxX)) +
                 scale_x_continuous(labels = dollar) +
                 scale_y_continuous(labels = dollar) +
                 theme_ipsum() +
                 theme(legend.title=element_blank(), legend.position = "bottom") +
                 labs(title = "Email campaign ROI model")

adbudgReturn = function(a,b,c,d,Spend){
  adbudgReturn = sum(b+(a-b)*((Spend^c)/(d+(Spend^c))))
  return(adbudgReturn)
}

returnGoal = 600000
increment = 1000
oldSpendVec = email_mk_data$Spend
oldReturn = adbudgReturn(a,b,c,d,oldSpendVec)
newSpendVec = oldSpendVec

totalSpend = sum(oldSpendVec)
totalReturn = oldReturn                 

while(totalReturn < returnGoal){
  incReturns = NULL
  for(i in 1:length(oldSpendVec)){
    oldSpendTemp = newSpendVec[i]
    newSpendTemp = newSpendVec[i] + increment
    
    oldReturnTemp = b+(a-b)*((oldSpendTemp^c)/(d+(oldSpendTemp^c)))
    newReturnTemp = b+(a-b)*((newSpendTemp^c)/(d+(newSpendTemp^c)))
    
    incReturns[i] = newReturnTemp - oldReturnTemp
    
  }
  
  winner = which.max(incReturns)
  newSpendVec[winner] = newSpendVec[winner] + increment
  
  totalSpend = totalSpend + increment
  totalReturn = adbudgReturn(a,b,c,d,newSpendVec)
  
}

newReturnVec = b+(a-b)*((newSpendVec^c)/(d+(newSpendVec^c)))
myRecommendedData = data.frame(Campaign = email_mk_data$Campaign,
                               Channel = email_mk_data$Channel,
                               Return = newReturnVec,
                               Spend = newSpendVec)

sum(myRecommendedData$Spend) # Recommended Spend
sum(myRecommendedData$Return)  # Estimated Return from Recommended Spend
sum(myRecommendedData$Spend)/sum(email_mk_data$Spend) - 1  # % Increase in Spend to get $600K


compareDF = data.frame(Campaign = rep(email_mk_data$Campaign,2), 
                       spendType = rep(c("Actual Spend","Recommended Spend"), 
                                       each=dim(email_mk_data)[1]), 
                       Spend = c(email_mk_data$Spend, myRecommendedData$Spend))


compareDF %>% ggplot(aes(Campaign, Spend, fill = spendType)) +
              geom_bar(stat = "identity", position=position_dodge()) +
              scale_fill_brewer(palette = "Set1", name = "") +
              scale_y_continuous(labels = dollar) +
              theme_ipsum() +
              theme(axis.text.x = element_text(angle = 45, hjust = .80)) +
              labs(title = "Campaign investments", 
                   subtitle = "Actual vs recommended to reach goal")
              