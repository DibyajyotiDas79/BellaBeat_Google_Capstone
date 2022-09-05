(For a more detailed code check the readme file)

library(tidyverse)
library(lubridate)
library(ggplot2)
library(readr)
library(tidyr)
library(dplyr)
library(skimr)
library(janitor)
library(scales)
library(plyr)

#importing csv files
daily_activities <- read.csv("C:\\Users\\Dibyajyoti Das\\Desktop\\case study 2\\Fitabase Data 4.12.16-5.12.16\\dailyActivity_merged.csv")
weight <- read.csv("C:\\Users\\Dibyajyoti Das\\Desktop\\case study 2\\Fitabase Data 4.12.16-5.12.16\\weightLogInfo_merged.csv")
sleep <- read.csv("C:\\Users\\Dibyajyoti Das\\Desktop\\case study 2\\Fitabase Data 4.12.16-5.12.16\\sleepDay_merged.csv")

#daily activities has 0 total steps data which need to be removed
daily_activities1 <- daily_activities %>% 
  filter(TotalSteps!=0)
View(daily_activities1)

#renaming the ActiviyDate column to date
colnames(daily_activities1)[2]<-"Date"
#separating date and time into different columns
weight1 <- weight %>% 
  separate(Date, c("Date","Time")," ")
View(weight1)

sleep1 <- sleep %>% 
  separate(SleepDay, c("Date","Time")," ")
View(sleep1)

#Converting date into weekday
weight1 <- weight1 %>% 
  mutate(Weekday = weekdays(as.Date(Date,"%m/%d/%Y")))

sleep1 <- sleep1 %>% 
  mutate(Weekday = weekdays(as.Date(Date,"%m/%d/%Y")))

daily_activities1 <- daily_activities1 %>% 
  mutate(Weekday = weekdays(as.Date(Date,"%m/%d/%Y")))

colnames(weight1)
colnames(sleep1)
colnames(daily_activities1)

#removing duplicates and Na
sum(duplicated(sleep1))
sleep1 <- sleep1[!duplicated(sleep), ]

sum(duplicated(weight1))
sum(duplicated(daily_activities1))

#merge
combined_data <- merge(daily_activities1,sleep1, by = "Id")
combined_data_final <- merge(combined_data,weight1, by = "Id")
View(combined_data_final)
summary(combined_data_final)

#order weekdays into proper order for plotting
combined_data_final$Weekday <- factor(combined_data_final$Weekday,
                                      levels = c("Monday","Tuesday","Wednesday","Thursday",
                                                 "Friday","Saturday","Sunday"))
combined_data_final[order(combined_data_final$Weekday), ]

summary(daily_activities1)
summary(sleep1)
summary(weight1)
write.csv(combined_data_final,"combined.csv")

#visualization
#Data recording during week
ggplot(data=combined_data_final,mapping = aes(x=Weekday))+geom_bar(fill="steelblue")+labs(title = "Data Recording during week")

ggplot(data=combined_data_final,mapping = aes(x=Weekday,y=TotalSteps))+geom_bar(stat='identity',fill='steelblue')+labs(title = "Steps throughout the week",y="Total steps")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=combined_data_final,mapping = aes(x=Weekday,y=Calories))+geom_bar(stat='identity',fill="violet")+
  labs(title = "Calories burn throughout the week",y="Calories")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(data=combined_data_final,mapping = aes(x=Weekday,y=TotalMinutesAsleep))+geom_bar(stat='identity',fill='red')+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+labs(title = "Total Minute Asleep",y="Total Minute Asleep")

#Relationship between calories burned and total steps
ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalSteps,y=Calories),color='red')+
  geom_smooth(mapping = aes(x=TotalSteps,y=Calories))
#sedentary minutes vs total steps
ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalSteps,y=SedentaryMinutes,color=Calories))+
  geom_smooth(mapping = aes(x=TotalSteps,y=SedentaryMinutes))+scale_color_gradient(low="blue",high = "yellow")

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalSteps,y=SedentaryMinutes,color=TotalDistance))+
  geom_smooth(mapping = aes(x=TotalSteps,y=SedentaryMinutes))+scale_color_gradient(low="blue",high = "yellow")

#very active minutes vs total steps
ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalSteps,y=VeryActiveMinutes,color=Calories))+
  geom_smooth(mapping = aes(x=TotalSteps,y=VeryActiveMinutes))+scale_color_gradient(low="blue",high = "yellow")

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalSteps,y=VeryActiveMinutes,color=TotalDistance))+
  geom_smooth(mapping = aes(x=TotalSteps,y=VeryActiveMinutes))+scale_color_gradient(low="blue",high = "yellow")

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalMinutesAsleep,y=VeryActiveMinutes,color="VeryActiveMinutes"))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep,y=VeryActiveMinutes))

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalMinutesAsleep,y=FairlyActiveMinutes,color="FairlyActiveMinutes"))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep,y=FairlyActiveMinutes))

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalMinutesAsleep,y=LightlyActiveMinutes,color="LightlyActiveMinutes"))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep,y=LightlyActiveMinutes))

ggplot(data=combined_data_final)+geom_point(mapping = aes(x=TotalMinutesAsleep,y=SedentaryMinutes,color="SedentaryMinutes"))+
  geom_smooth(mapping = aes(x=TotalMinutesAsleep,y=SedentaryMinutes))

#pie chart
VeryActive <- sum(combined_data_final$VeryActiveMinutes)
FairlyActive <- sum(combined_data_final$FairlyActiveMinutes)
LightlyActive <- sum(combined_data_final$LightlyActiveMinutes)
Sedentary <- sum(combined_data_final$SedentaryMinutes)

slices <- c(VeryActive,FairlyActive,LightlyActive,Sedentary)
lbls <- c("Very Active","Fairly Active","Lightly Active","Sedentary")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct)
lbls <- paste(lbls, "%",sep = "")
pie(slices, labels = lbls, col = rainbow(length(lbls)), main = "Percentage of Activity in Minutes")
