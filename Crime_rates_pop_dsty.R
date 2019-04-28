library(tidyverse)
library(dplyr)
library(lubridate)

crimestats <- read.csv("Datasets for R/Crime Data/crime1418parksasprecincts.csv", header =TRUE, stringsAsFactors = FALSE)

glimpse(crimestats)

#Fix dates
crimestats$DATE <- mdy(crimestats$DATE)


#Add columns for month, year, and convert format of extra columns to "date" type
crimestats <- crimestats %>%
  mutate(month = format(DATE, "%m"), year = format(DATE, "%Y"))


#create variables to work with population density for each precinct, including parks
popdsty70 <- as.integer(85.14)
popdsty71 <- as.integer(94.55)
popdsty72 <- as.integer(52.55)
popdsty77 <- as.integer(98.37)
popdsty78 <- as.integer(39.30)
popdsty78PP <- as.integer(52.09)
popdsty88 <- as.integer(52.85)
popdsty88FGP <- as.integer(91.33)

#Calculating crime rate by pop density for each precinct, for crime levels each year
rtcrimepopdsty_by_yr88FGP <- crimestats %>%
  filter(PRECINCT == "88FGP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(popdsty88FGP)
  )

print(rtcrimepopdsty_by_yr88FGP)

#Plot This
ggplot(rtcrimepopdsty_by_yr88FGP, aes(x= year, y= crimerate, color = CRIME_LEVEL))+ 
  geom_point()+
  labs(title= "Fort Greene Park Crime Rate 2014-2018", x= "Year", y = "Crime Rate by Pop Density")+
  theme_grey()

rtcrimepopdsty_by_yr78PP <- crimestats %>%
  filter(PRECINCT == "78PP", CRIME_LEVEL != "VIOLATION") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(popdsty78PP)
  )

print(rtcrimepopdsty_by_yr78PP)

#Change for each precinct to get population density

rtcrimepopdsty_by_yr78 <- crimestats %>%
  filter(PRECINCT == "78", CRIME_LEVEL != "VIOLATION") %>%
  group_by(CRIME_LEVEL, year) %>%
  summarise(
    crimerate = n()/(popdsty78)
  )

print(rtcrimepopdsty_by_yr78)

#print the population density and copy to CSV as popdensity column

rtdstycrime <- read.csv("Datasets for R/Crime Data/crimeratepops.csv", header =TRUE, stringsAsFactors = FALSE)

glimpse(rtdstycrime)

##Plot the precincts and parks for crime by population density variable
rtdstycrime <- rtdstycrime %>%
  group_by(PRECINCT, CRIME_LEVEL, YEAR)

#CrimeRatesbyAcreCompare

ggplot(rtdstycrime, aes(x=YEAR, y=RATEBYPOPDSTY, color= CRIME_LEVEL))+
  geom_jitter()+
  facet_wrap(.~PRECINCT)+
  labs(title= "Crime Rates By Population Density for Precincts and Parks", x= "Year", y = "Crime Rate by Pop Density")+
  theme_grey()

