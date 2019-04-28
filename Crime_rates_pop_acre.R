library(tidyverse)
library(dplyr)
library(lubridate)

crimestats <- read.csv("Datasets for R/Crime Data/crime1418parksasprecincts.csv", header =TRUE, stringsAsFactors = FALSE)

crimestats$DATE <- mdy(crimestats$DATE)
#crimestats$TIME <- hms(crimestats$TIME)
#crimestats$CRIME_LEVEL <- as.character(crimestats$CRIME_LEVEL)
#crimestats$DESCRIPTION <- as.character(crimestats$DESCRIPTION)
#crimestats$PARK_NAME <- as.character(crimestats$PARK_NAME)
#crimestats$PRECINCT <- as.character(crimestats$PRECINCT)

#Add columns for month, year, hour and convert format of extra columns to "date" type
crimestats <- crimestats %>%
  mutate(month = format(DATE, "%m"), year = format(DATE, "%Y"))


pctstats <- read.csv("Datasets for R/Crime Data/crimestats2.3.csv", header = TRUE)

pctstats$Precinct <- as.character(pctstats$Precinct)
pctstats$Record <- as.character(pctstats$Record)

glimpse(pctstats)

#create variables to work with square acres and population

acres <- pctstats$Acres 

#sum of all crimes by precinct, year, crime level
sumcrime_by_yr <- crimestats %>%
  group_by(PRECINCT, CRIME_LEVEL, year=floor_date(DATE, "year")) %>%
  summarise(
    incidents = n()
  )

glimpse(sumcrime_by_yr)

#populations and square area for all precincts

pop70 <- as.integer(160664)
pop71 <- as.integer(98429)
pop72 <- as.integer(126230)
pop77 <- as.integer(96309)
pop78 <- as.integer(61071)
pop88 <- as.integer(51421)
pop78PP <- as.integer(27397)
pop88FGP <- as.integer(2740)

acr70 <- as.integer(1887)
acr71 <- as.integer(1041)
acr72 <- as.integer(2402)
acr77 <- as.integer(979)
acr78 <- as.integer(969) #585 acres subtracted for PP
acr78PP <- as.integer(585)
acr88 <- as.integer(943) #30 acres subtracted for FGP
acr88FGP <- as.integer(30)

popdsty70 <- as.integer(85.14)
popdsty71 <- as.integer(94.55)
popdsty72 <- as.integer(52.55)
popdsty77 <- as.integer(98.37)
popdsty78 <- as.integer(39.30)
popdsty78PP <- as.integer(52.09)
popdsty88 <- as.integer(52.85)
popdsty88FGP <- as.integer(91.33)

#FGP crime rate per 1000 people

rtpopcrime_by_yr88FGP <- crimestats %>%
  filter(PRECINCT == "88FGP") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop88FGP*1000)
  )

print(rtpopcrime_by_yr88FGP)

ggplot(rtpopcrime_by_yr88FGP, aes(x= year, y= crimerate, color = CRIME_LEVEL))+ 
  geom_point()+
  labs(title= "Fort Greene Park Crime Rate 2014-2018", x= "Year", y = "Rate per 1000 visitors")+
  theme_grey()

#PP crime rate per 1000 people
rtpopcrime_by_yr78PP <- crimestats %>%
  filter(PRECINCT == "78PP") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop78PP*1000)
  )

ggplot(rtpopcrime_by_yr78PP, aes(x= year, y= crimerate, color = CRIME_LEVEL))+ 
  geom_point()+
  labs(title= "Prospect Park Crime Rate 2014-2018", x= "Year", y = "Rate per 1000 visitors")+
  theme_grey()

print(rtpopcrime_by_yr78PP)

#70th pct crime rate per 1000 people
rtpopcrime_by_yr70 <- crimestats %>%
  filter(PRECINCT == "70") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop70*1000)
  )

print(rtpopcrime_by_yr70)

ggplot(rtpopcrime_by_yr70, aes(x= year, y= crimerate, color = CRIME_LEVEL))+ 
  geom_point()+
  labs(title= "70th Pct Crime Rate 2014-2018", x= "Year", y = "Rate per 1000 visitors")+
  theme_grey()


#71st pct crime rate per 1000 people
rtpopcrime_by_yr71 <- crimestats %>%
  filter(PRECINCT == "71") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop71*1000)
  )

print(rtpopcrime_by_yr71)

#72nd pct crime rate per 1000 people
rtpopcrime_by_yr72 <- crimestats %>%
  filter(PRECINCT == "72") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop72*1000)
  )

print(rtpopcrime_by_yr72)

#77nd pct crime rate per 1000 people
rtpopcrime_by_yr77 <- crimestats %>%
  filter(PRECINCT == "77") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop77*1000)
  )

print(rtpopcrime_by_yr77)

#78th pct crime rate per 1000 people
rtpopcrime_by_yr78 <- crimestats %>%
  filter(PRECINCT == "78") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop78*1000)
  )

print(rtpopcrime_by_yr88)

#88th pct crime rate per 1000 people
rtpopcrime_by_yr88 <- crimestats %>%
  filter(PRECINCT == "88") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(pop88*1000)
  )

print(rtpopcrime_by_yr88)

#Calculate crimes per acre for each precinct by year
#FGP crime rate per acre

rtacrcrime_by_yr88FGP <- crimestats %>%
  filter(PRECINCT == "88FGP") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(acr88FGP)
  )

print(rtacrcrime_by_yr88FGP)

ggplot(sumcrime_by_yr88FGP, aes(x= year, y= crimerate, color = CRIME_LEVEL))+ 
  geom_point()+
  labs(title= "Fort Greene Park Crime Rate 2014-2018", x= "Year", y = "Rate per 1000 visitors")+
  theme_grey()

#PP crime rate per acr
rtacrcrime_by_yr78PP <- crimestats %>%
  filter(PRECINCT == "78PP") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(acr78PP)
  )

print(rtacrcrime_by_yr78PP)

#70th pct crime rate per acre
rtacrcrime_by_yr70 <- crimestats %>%
  filter(PRECINCT == "70") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimeacr = n()/(acr70)
  )

print(rtacrcrime_by_yr70)

#71st pct crime rate per acr
rtacrcrime_by_yr71 <- crimestats %>%
  filter(PRECINCT == "71") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimeacr = n()/(acr71)
  )

print(rtacrcrime_by_yr71)

#72nd pct crime rate per 1000 people
rtacrcrime_by_yr72 <- crimestats %>%
  filter(PRECINCT == "72") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimeacr = n()/(acr72)
  )

print(rtacrcrime_by_yr72)

#77nd pct crime rate per acre
rtacrcrime_by_yr77 <- crimestats %>%
  filter(PRECINCT == "77") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimertacr = n()/(acr77)
  )

print(rtacrcrime_by_yr77)

#78th pct crime rate per 1000 people
rtacrcrime_by_yr78 <- crimestats %>%
  filter(PRECINCT == "78") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimertbyacr = n()/(acr78)
  )

print(rtacrcrime_by_yr78)

#88th pct crime rate per acre
rtacrcrime_by_yr88 <- crimestats %>%
  filter(PRECINCT == "88") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimertbyacr = n()/(acr88)
  )

print(rtacrcrime_by_yr88)

 
#write these to new CSV: crimeratepops.csv

crimerts <- read.csv("Datasets for R/Crime Data/crimeratepops.csv", header =TRUE, stringsAsFactors = FALSE)

glimpse(crimerts)

#Compare crime rates by acre

rtacrcrime <- crimerts %>%
  group_by(PRECINCT, CRIME_LEVEL, YEAR)

#CrimeRatesbyAcreCompare

ggplot(rtacrcrime, aes(x=YEAR, y=RATEBYACRE, color= CRIME_LEVEL))+
  geom_jitter()+
  facet_wrap(.~PRECINCT)+
  labs(title= "Crime Rates By Acre for Precincts and Parks", x= "Year", y = "Crime Rate per Sq. Acre")+
  theme_grey()

#CrimeRatesbyAcreCompare Bar Chart

ggplot(rtacrcrime, aes(x=YEAR, y=RATEBYACRE, color= CRIME_LEVEL))+
  geom_col(
    mapping = aes(x=YEAR, fill = CRIME_LEVEL),
    position = "dodge"
  )+
  facet_wrap(.~PRECINCT)+
  labs(title= "Crime Rates By Acre for Precincts and Parks", x= "Year", y = "Crime Rate per Sq. Acre")+
  theme_grey()

#CrimeRatesbyPopCompare
ggplot(rtacrcrime, aes(x=YEAR, y=RATEBYPOP, color= CRIME_LEVEL))+
  geom_jitter()+
  facet_wrap(.~PRECINCT)+
  labs(title= "Crime Rates By Population for Precincts and Parks", x= "Year", y = "Crime Rate by Population")+
  theme_grey()

#CrimeRatesbyPopCompare Bar Chart
##RtsbyPopAllPctsBars
ggplot(rtacrcrime, aes(x=YEAR, y=RATEBYPOP, color= CRIME_LEVEL))+
  geom_col(
    mapping = aes(x=YEAR, fill=CRIME_LEVEL),
    position = "dodge"
  )+
  facet_wrap(.~PRECINCT)+
  scale_fill_manual(values=c("firebrick","dodgerblue"))+
  labs(title= "Crime Rates By Population for Precincts and Parks", x= "Year", y = "Crime Rate by Population (per 1000 people)")+
  theme_grey()

#round calculations to 2 decimals
#ratecrime_by_yr70 <- ratecrime_by_yr70$ratebypop %>%
#  mutate_if(is.numeric, round, digits= 2)


#glimpse(sumcrime_by_yr70)
