library(tidyverse)
library(dplyr)
library(lubridate)

crimetype3 <- read.csv("Datasets for R/Crime Data/crimetype.csv", header =TRUE, stringsAsFactors = FALSE)

glimpse(crimetype3)

#months must be numeric/dbl, not integer to use continuous x-axis
crimetype3$month <- as.numeric(crimetype3$month)

#trim white space
crimetype3$PARK_NAME <- str_trim(crimetype3$PARK_NAME)

#crimes by month, aggregated across all years: which months have most and least complaints

glimpse(crimetype3)

#Precinct Incidents: Sums by Month
#by month for all precincts excluding park data
crime_by_month_pcts <- crimetype3 %>%
  filter(PRECINCT!="78PP", PRECINCT!="88FGP") %>%
  group_by(month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()
  )

crime_by_month_pcts$Incidents <- as.numeric(crime_by_month_pcts$Incidents)

glimpse(crime_by_month_pcts)


ggplot(crime_by_month_pcts, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  #xlim(1,12)+
  ylim(0,9000)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  #scale_y_continuous("Incidents Totals Over 5 Years", breaks=c(0,1000,3000,5000,7000,9000), labels=c("0","1000","3000","5000","7000","9000"))+
  labs(title= "Precinct Incidents: Sums by Month", x= "Month", y = "Incidents Totals Over 5 Years") +
                   
  theme_grey()

  #Prospect Park Incidents Sums by Month
##PPSumsbyMonth
#by month for only Prospect Park, excluding violations
crime_by_month_PP <- crimetype3 %>%
    filter(PRECINCT=="78PP", CRIME_LEVEL != "VIOLATION") %>%
    group_by(month, CRIME_LEVEL) %>%
    summarise(
      Incidents = n()
    )

#Incidents must be numeric
crime_by_month_PP$Incidents <- as.numeric(crime_by_month_PP$Incidents)


ggplot(crime_by_month_PP, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  #xlim(1,12)+
  ylim(0,40)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  #scale_y_continuous("Incidents Totals Over 5 Years", breaks=c(0,10,30,50,70,90), labels=c("0","10","30","50","70","90"))+
  
  labs(title= "Prospect Park Incidents: Sums by Month Last 5 Years", x= "Month", y = "Number of Incidents Over 5 Years") +
  
  theme_grey()

#PPSumsbyMonthBars
ggplot(crime_by_month_PP, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_col(
    mapping = aes(x=month, fill=CRIME_LEVEL),
    position = "dodge"
  )+
  #xlim(1,12)+
  ylim(0,40)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  #scale_y_continuous("Incidents Totals Over 5 Years", breaks=c(0,10,30,50,70,90), labels=c("0","10","30","50","70","90"))+
  
  labs(title= "Prospect Park Incidents: Sums by Month", x= "Month", y = "Number of Incidents Over 5 Years") +
  scale_fill_manual(values=c("firebrick","dodgerblue"))+
  theme_grey()

#felony-misdemeanor bar chart columns chart
ggplot(crime_by_month_PP, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_col(
    mapping = aes(x=month, fill=CRIME_LEVEL),
    position = "dodge"
  )+
  scale_fill_manual(values=c("firebrick","dodgerblue"))+
  #xlim(1,12)+
  #ylim(0,50)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  #scale_y_continuous("Incidents Totals Over 5 Years", breaks=c(0,10,30,50,70,90), labels=c("0","10","30","50","70","90"))+
  
  labs(title= "Prospect Park Incidents: Sums by Month", x= "Month", y = "Number of Incidents Over 5 Years") +
  
  theme_grey()


#Fort Greene Park Incidents Sums by Month
#by month for only Fort Greene Park, excluding violations

crime_by_month_FGP <- crimetype3 %>%
  filter(PRECINCT=="88FGP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()
  )

#Incidents must be numeric
crime_by_month_FGP$Incidents <- as.numeric(crime_by_month_FGP$Incidents)

##FGPSumsbyMonth
ggplot(crime_by_month_FGP, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  ylim(0,15)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  
  labs(title= "Fort Greene Park Incidents: Sums by Month", x= "Month", y = "Number of Incidents") +
  
  theme_grey()


#FGPSumsbyMonthBars
ggplot(crime_by_month_FGP, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_col(
    mapping = aes(x=month, fill=CRIME_LEVEL),
    position = "dodge"
  )+
  scale_fill_manual(values=c("firebrick","dodgerblue"))+
  ylim(0,15)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  
  labs(title= "Fort Greene Park Incidents: Sums by Month", x= "Month", y = "Number of Incidents") +
  
  theme_grey()


#Calculating mean of felonies and misdemeanors by month in FGP over 5 years

crime_by_month_FGPmean <- crimetype3 %>%
  filter(PRECINCT=="88FGP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()/5
  )

#plot by points
ggplot(crime_by_month_FGPmean, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  ylim(0,3)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  labs(title= "Mean of Fort Greene Park Crimes by Month Over Last 5 Years", x= "Month", y = "Number of Incidents") +
  
  theme_grey()

#stacked bars
ggplot(crime_by_month_FGPmean, aes(x=month, y=Incidents, fill=CRIME_LEVEL, group = Incidents))+
  geom_col()+
  ylim(0,5)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  
  labs(title= "Mean of Fort Greene Park Crimes by Month Over Last 5 Years", x= "Month", y = "Number of Incidents") +
  
  theme_grey()


print(crime_by_month_FGPmean, n=24)

#Calculating mean of felonies and misdemeanors by month in PP

crime_by_month_PPmean <- crimetype3 %>%
  filter(PRECINCT=="78PP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()/5
  )

ggplot(crime_by_month_PPmean, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  ylim(0,10)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  
  labs(title= "Mean of Prospect Park Crimes by Month Over Last 5 Years", x= "Month", y = "Number of Incidents") +
  theme_grey()

#Precinct Incidents: Mean of felonies and misdemeanors by Month
#by month for all precincts excluding park data
crime_by_month_pctsmean <- crimetype3 %>%
  filter(PRECINCT!="78PP", PRECINCT!="88FGP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(PRECINCT, month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()/5
  )

ggplot(crime_by_month_pctsmean, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  ylim(0,450)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  facet_wrap(PRECINCT~.)+
  labs(title= "Mean of Precinct Crimes by Month Over Last 5 Years", x= "Month", y = "Number of Incidents") +
  
  theme_gray()

#plot by points
ggplot(crime_by_month_FGPmean, aes(x=month, y=Incidents, color= CRIME_LEVEL))+
  geom_point()+
  ylim(0,5)+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  
  labs(title= "Mean of Fort Greene Park Crimes by Month Over Last 5 Years", x= "Month", y = "Number of Incidents") +
  
  theme_grey()

#Write to CSV the means for all precincts and parks to create calculations by population density

crime_by_month_pctsmean <- crimetype3 %>%
  filter(CRIME_LEVEL != "VIOLATION") %>%
  group_by(PRECINCT, month, CRIME_LEVEL) %>%
  summarise(
    Incidents = n()/5
  )

write.csv(crime_by_month_pctsmean, "Datasets for R/Crime Data/precinctmeans.csv")

#Add Precinct Populations to the CSV
pop70 <- as.integer(160664)
pop71 <- as.integer(98429)
pop72 <- as.integer(126230)
pop77 <- as.integer(96309)
pop78 <- as.integer(61071)
pop88 <- as.integer(51421)
pop78PP <- as.integer(27397)
pop88FGP <- as.integer(2740)

pctmeans <- read.csv("Datasets for R/Crime Data/precinctmeans.csv", header =TRUE, stringsAsFactors = FALSE)

glimpse(pctmeans)

#Calculating and Plotting the means by population density

pop_pctsmean <- pctmeans %>%
  mutate(Crime_rate = Incidents/Population)

#Precincts Only
##MeanPctCrimeMonthPopDsty
pop_pctsmean <- pop_pctsmean %>%
  filter(PRECINCT!="78PP", PRECINCT!="88FGP") %>%
  group_by(PRECINCT, month, CRIME_LEVEL, Crime_rate)

ggplot(pop_pctsmean, aes(x=month, y=Crime_rate, color= CRIME_LEVEL))+
  geom_point()+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  facet_wrap(PRECINCT~.)+
  labs(title= "Mean Crime Rates by Month and Population: 5 Years", x= "Month", y = "Monthly Mean Crime Rate Per Person") +
  
  theme_gray()

##MeanCrimebyMonPopDstyAll

pop_pctsmeanAll <- pctmeans %>%
  mutate(Crime_rate = Incidents/Population)

pop_pctsmeanAll <- pop_pctsmeanAll %>%
  group_by(PRECINCT, month, CRIME_LEVEL, Crime_rate)


ggplot(pop_pctsmeanAll, aes(x=month, y=Crime_rate, color= CRIME_LEVEL))+
  geom_jitter()+
  scale_x_continuous("Month",breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("J","F","M","A","M","J","J","A","S","O","N","D"))+
  facet_wrap(PRECINCT~.)+
  labs(title= "Mean Crime Rates by Month and Population: 5 Years", x= "Month", y = "Monthly Mean Crime Rate") +
  
  theme_gray()

  