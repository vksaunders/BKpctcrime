library(tidyverse)
library(dplyr)
library(lubridate)

crime <- read.csv("Datasets for R/Crime Data/NYPD_Complaint_Data_Merged.csv", header =TRUE)

typeof(crime)

summary(crime)

#select variables for our data research

crime_cnt <- select(crime, CMPLNT_FR_DT, CMPLNT_FR_TM, ADDR_PCT_CD, LAW_CAT_CD, OFNS_DESC, PARKS_NM)
head(crime_cnt)


#filter to keep the needed precincts
crime_cnt_area = filter(crime_cnt, ADDR_PCT_CD %in% c(70,71,72,77,78,88))
head(crime_cnt_area)
sum(is.na(crime_cnt_area$PARKS_NM))
tail(crime_cnt_area)

#change factors to characters
crime_cnt_area$LAW_CAT_CD <- as.character(crime_cnt_area$LAW_CAT_CD)
crime_cnt_area$OFNS_DESC <- as.character(crime_cnt_area$OFNS_DESC)
crime_cnt_area$PARKS_NM <- as.character(crime_cnt_area$PARKS_NM)
glimpse(crime_cnt_area)

#crime_cnt_area$PARKS_NM[crime_cnt_area$PARKS_NM == ""] <- "NA"
#crime_cnt_area$PARKS_NM[crime_cnt_area$PARKS_NM == " "] <- "NA"

sum(is.na(crime_cnt_area$PARKS_NM))
head(crime_cnt_area)

#change NA values to NOTPARK
crime_cnt_area$PARKS_NM[is.na(crime_cnt_area$PARKS_NM)] <- 'NOTPARK'
crime_cnt_area$PARKS_NM[crime_cnt_area$PARKS_NM == "NA"] <- 'NOTPARK'
crime_cnt_area$PARKS_NM[crime_cnt_area$PARKS_NM == ""] <- 'NOTPARK'

unique(crime_cnt_area$PARKS_NM)

#fix dates
crime_cnt_area$CMPLNT_FR_DT <- mdy(crime_cnt_area$CMPLNT_FR_DT)
head(crime_cnt_area)
glimpse(crime_cnt_area) 
tail(crime_cnt_area)

#change the header names
colnames(crime_cnt_area)[1] <- "DATE"
colnames(crime_cnt_area)[2] <- "TIME"
colnames(crime_cnt_area)[3] <- "PRECINCT"
colnames(crime_cnt_area)[4] <- "CRIME_LEVEL"
colnames(crime_cnt_area)[5] <- "DESCRIPTION"
colnames(crime_cnt_area)[6] <- "PARK_NAME"
head(crime_cnt_area)

#Look at date ranges
crime_cnt_area %>% arrange(DATE)

#subset for the date range 2014 and later
crime2014_2018 <- crime_cnt_area %>%
  filter(DATE >= as.Date("2014-01-01")) %>%
  arrange(DATE)

head(crime2014_2018)

#write this to a new file for our analysis
write.csv(crime2014_2018,"Datasets for R/Crime Data/crime1418.csv")

