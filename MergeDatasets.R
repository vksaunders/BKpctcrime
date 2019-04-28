library(tidyverse)
library(dplyr)

#datafile1 <- read.csv("c:/datafile1.csv", header=T, sep=",")
#datafile2 <- read.csv("c:/datafile2.csv", header=T, sep=",")

#dim(datafile1)
#dim(datafile2)

#datafile <- rbind(datafile1,datafile2)
#dim(datafile)

#write.csv(datafile,"c:/datafile.csv")

datafile1 <- read.csv("Datasets for R/Crime Data/MergeCSV/NYPD_Complaint_Data_Historic.csv", header=T, sep=",")
datafile2 <- read.csv("Datasets for R/Crime Data/MergeCSV/NYPD_Complaint_Data_Current__Year_To_Date_.csv", header=T, sep=",")
dim(datafile1)
dim(datafile2)

allcrime <- rbind(datafile1,datafile2)
dim(allcrime)

write.csv(allcrime,"Datasets for R/Crime Data/MergeCSV/NYPD_Complaint_Data_Merged.csv")
