library(tidyverse)
library(dplyr)
library(lubridate)

crimetype <- read.csv("Datasets for R/Crime Data/crime1418parksasprecincts.csv", header =TRUE, stringsAsFactors = FALSE)

typeof(crimetype)

summary(crimetype)
glimpse(crimetype)

crimetype$DATE <- mdy(crimetype$DATE)
glimpse(crimetype)

#crimetype$TIME <- hms(crimetype$TIME)

#Add columns for month, year
crimetype <- crimetype %>%
  mutate(month = format(DATE, "%m"), year = format(DATE, "%Y"))

write.csv(crimetype,"Datasets for R/Crime Data/crimetype.csv")

#Use StringsAsFactors to take care of variable types
crimetype1 <- read.csv("Datasets for R/Crime Data/crimetype.csv", header =TRUE, stringsAsFactors = FALSE)
glimpse(crimetype1)

sum(is.na(crimetype1))

#which crime decriptions come up most frequently for 70th precinct?

crimetype70 <- crimetype1 %>%
  filter(PRECINCT == 70) %>%
  group_by(DESCRIPTION, CRIME_LEVEL)%>%
  summarise(
    Offense = n()
  ) %>%
  arrange(desc(Offense))
  
glimpse(crimetype70)
summary(crimetype70)

#show the top 3 crimes for each year in 70th pct

head(crimetype70, n=15)

tail(crimetype70, n=15)

#Exclude violations
crimetype70fm <- crimetype1 %>%
  filter(PRECINCT == 70, CRIME_LEVEL!= "VIOLATION") %>%
  group_by(DESCRIPTION)%>%
  summarise(
    Offense = n()
  ) %>%
  top_n(5)%>%
  arrange(desc(Offense))

##NEED BARS GOING OTHER DIRECTION AND TOP 5 FOR EACH YEAR (MIGHT BE DIFFERENT)
ggplot(crimetype70fm, aes(x=Offense, y=DESCRIPTION))+
  geom_col()+
 # facet_wrap(.~year)+
  labs(title= "70th Precinct: Top 5 Reported Crimes(5 Years)", x= "Number of Offenses", y = "Felony Description")+
  theme_grey()

summary(crimetype70fm)
head(crimetype70fm, n=15)
tail(crimetype70fel, n=15)



#how many murders have occurred in this precinct each year?

crimetype70mur <- crimetype1 %>%
  filter(PRECINCT==70, DESCRIPTION == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  group_by(year) %>%
  summarise(
    Murders = n()
  )

head(crimetype70mur)

#which crime decriptions come up most frequently for each precinct, all years combined?
##
crimetypepcts <- crimetype1 %>%
  filter(CRIME_LEVEL != "VIOLATION")%>%
  group_by(PRECINCT, DESCRIPTION)%>%
  
  summarise(
    Offense = n()
  ) %>%
  top_n(5) %>%
  arrange(desc(Offense))

##HOW DO I MAKE A PLOT THAT SHOWS bars of x=# and y=top 5 crime descriptions
ggplot(crimetypepcts, aes(x=Offense, y=DESCRIPTION))+
  geom_col(show.legend=FALSE)+
  facet_wrap(.~PRECINCT)
  #coord_flip()

glimpse(crimetypepcts)
summary(crimetypepcts)
head(crimetypepcts, n=20)  
tail(crimetypepcts, n=20)

#show top felonies for all pcts
crimetypepctsfel <- crimetype1 %>%
  filter(CRIME_LEVEL== "FELONY") %>%
  group_by(PRECINCT, year, DESCRIPTION)%>%
  summarise(
    Offense = n()
  ) %>%
  arrange(desc(Offense))

summary(crimetypepctsfel)
head(crimetypepctsfel, n=15)
tail(crimetypepctsfel, n=15)

#how many murders have occurred across these precincts each year?
##MurdersforPCTsFacetbyYear
crimetypepctsmur <- crimetype1 %>%
  filter(DESCRIPTION == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  group_by(PRECINCT,year) %>%
  summarise(
    Murders = n()
  )

head(crimetypepctsmur, n=20)
tail(crimetypepctsmur, n=20)

ggplot(crimetypepctsmur, aes(x=PRECINCT, y=Murders))+
  geom_col()+
  facet_wrap(.~year)

#Let's look at the park data alone
#top crimes in Prospect Park each year

crimetype78PP <- crimetype1 %>%
  filter(PRECINCT== "78PP", CRIME_LEVEL=="FELONY") %>%
  group_by(DESCRIPTION)%>%
  summarise(
    Offense = n()
  ) %>%
  arrange(desc(Offense))

print(crimetype88FGP)

crimetype88FGP <- crimetype1 %>%
  filter(PRECINCT== "88FGP", CRIME_LEVEL=="FELONY") %>%
  group_by(DESCRIPTION)%>%
  summarise(
    Offense = n()
  ) %>%
  arrange(desc(Offense))

print(crimetype88FGP)

##HOW DO I PLOT THIS
ggplot(crimetype78PP, aes(x=DESCRIPTION, y=Offense))+
  geom_col()+
  facet_wrap(.~year)
  

glimpse(crimetype78PP)
summary(crimetype78PP)
head(crimetype78PP, n=20)

#how many murders have occurred in Prospect Park & Fort Greene Park each year? Answer is 0

crimetype78PPmur <- crimetype1 %>%
  filter(PRECINCT=="78PP", DESCRIPTION == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  group_by(year) %>%
  summarise(
    Murders = n()
  )

head(crimetype78PPmur)

crimetype88FGPmur <- crimetype1 %>%
  filter(PRECINCT=="88FGP", DESCRIPTION == "MURDER & NON-NEGL. MANSLAUGHTER") %>%
  group_by(year) %>%
  summarise(
    Murders = n()
  )

head(crimetype88FGPmur)
