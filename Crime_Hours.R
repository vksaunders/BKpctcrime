library(tidyverse)
library(dplyr)
library(lubridate)

crimehrs <- read.csv("Datasets for R/Crime Data/crime1418parksasprecincts.csv", header =TRUE, stringsAsFactors = FALSE)

#fix date type
crimehrs$DATE <- mdy(crimehrs$DATE)
#glimpse(crime)

sum(is.na(crimehrs$TIME))

#Extract the first two digits of Time column to create a new variable: "Hour"

#crimehrs$TIME <- substr(crimehrs$TIME, 0, 2)
crimehrs <- crimehrs%>%
  mutate(Hour= substr(crimehrs$TIME, 0, 2))

crimehrs$Hour <- as.character(crimehrs$Hour)

sum(is.na(crimehrs$Hour))
glimpse(crimehrs)
tail(crimehrs)

unique(crimehrs$Hour)

crimehrs$Hour<- str_replace_all(crimehrs$Hour, ":", "")

crimehrs$Hour <- as.integer(crimehrs$Hour)
 
head(crimehrs)

#Prospect Park Sum of Offenses by Hour of Day Over 5 Years

crime_hrs78PP <- crimehrs %>%
  filter(PRECINCT == "78PP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(Hour, CRIME_LEVEL) %>%
  
  summarise(
    Incidents = n()
  ) %>%
  arrange(Hour)

glimpse(crime_hrs78PP)
tail(crime_hrs78PP)

ggplot(crime_hrs78PP, aes(x= Hour, y= Incidents, color = CRIME_LEVEL)) + 
  geom_point() +
  scale_y_continuous("Number of Incidents", breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20))+
  #ylim(0,20)+
  scale_x_continuous("Hour",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),labels=c("12A","1A","2A","3A","4A","5A","6A","7A","8A","9A","10A","11A","12P","1P","2P","3P","4P","5P","6P","7P","8P","9P","10P","11P"))+
  labs(title= "Prospect Park Sum of Offenses by Hour of Day Over 5 Years", x= "Hour", y = "Number of Offenses")+
  theme_grey()

#FGP Sum of Offenses by Hour of Day Over 5 Years
crime_hrs88FGP <- crimehrs %>%
  filter(PRECINCT == "88FGP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(Hour, CRIME_LEVEL) %>%
  
  summarise(
    Incidents = n()
  ) %>%
  arrange(Hour)

glimpse(crime_hrs88FGP)

##FGPSumbyHourOver5Yrs

ggplot(crime_hrs88FGP, aes(x= Hour, y= Incidents, color = CRIME_LEVEL)) + 
  geom_point() +
  scale_y_continuous("Number of Incidents", breaks = c(0,1,2,3,4,5,6,7,8,9,10))+
  #ylim(0,10)+
  scale_x_continuous("Hour",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),labels=c("12A","1A","2A","3A","4A","5A","6A","7A","8A","9A","10A","11A","12P","1P","2P","3P","4P","5P","6P","7P","8P","9P","10P","11P"))+
  labs(title= "Fort Greene Park Sum of Offenses by Hour of Day Over 5 Years")+
  theme_grey()

#Mean of crimes by hour

crime_hrsMeanPP <- crimehrs %>%
  filter(PRECINCT == "78PP",CRIME_LEVEL != "VIOLATION") %>%
  group_by(Hour, CRIME_LEVEL) %>%
  
  summarise(
    Incidents = n()/5
  ) %>%
  arrange(Hour)

##PPAvgOffensesHrly
ggplot(crime_hrsMeanPP, aes(x= Hour, y= Incidents, color = CRIME_LEVEL)) + 
  geom_point() +
  scale_y_continuous("Number of Offenses", breaks = c(0,1,2,3,4,5))+
  #ylim(0,10)+
  scale_x_continuous("Hour",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),labels=c("12A","1A","2A","3A","4A","5A","6A","7A","8A","9A","10A","11A","12P","1P","2P","3P","4P","5P","6P","7P","8P","9P","10P","11P"))+
  labs(title= "Prospect Park Average Number Offenses Hourly Over 5 Years")+
  theme_grey()

#FGP Mean Hourly
crime_hrsMeanFGP <- crimehrs %>%
  filter(PRECINCT == "88FGP",CRIME_LEVEL != "VIOLATION") %>%
  group_by(Hour, CRIME_LEVEL) %>%
  
  summarise(
    Incidents = n()/5
  ) %>%
  arrange(Hour)

##FGPAvgOffensesHrly
ggplot(crime_hrsMeanFGP, aes(x= Hour, y= Incidents, color = CRIME_LEVEL)) + 
  geom_point() +
  scale_y_continuous("Number of Offenses", breaks = c(0,1,2,3,4,5))+
  #ylim(0,10)+
  scale_x_continuous("Hour",breaks=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),labels=c("12A","1A","2A","3A","4A","5A","6A","7A","8A","9A","10A","11A","12P","1P","2P","3P","4P","5P","6P","7P","8P","9P","10P","11P"))+
  labs(title= "Fort Greene Park Average Number Offenses Hourly Over 5 Years")+
  theme_grey()
