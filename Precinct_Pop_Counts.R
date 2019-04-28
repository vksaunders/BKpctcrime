library(tidyverse)
library(dplyr)

pctpop <- read.csv("Datasets for R/Crime Data/NYC_Blocks_2010CensusData_Plus_Precincts.csv", header =TRUE)

head(pctpop)

pctpop1 <- select(pctpop, precinct, P0010001)

tail(pctpop1)

head(pctpop1)

glimpse(pctpop1)

colnames(pctpop1) <- c("precinct", "total_population")

tail(pctpop1)

view(pctpop1)

#pivot table with R

#filter to keep the needed precincts
popsums = filter(pctpop1, precinct %in% c(70,71,72,77,78,88))

popsumdata %>% group_by(precinct)

popsumcalc = summarise(popsumdata,
              total_population = sum(total_population))

#Subtract 28 people from 78th Precinct to account for 2 blocks that straddle 2 precincts

popsumcalc$total_population[popsumcalc$total_population == 61099] <- 61071

print(popsumcalc)

#write to a new CSV
write.csv(popsumcalc, "Datasets for R/Crime Data/Output Scripts/PctPopCts.csv")

#Another way to do this

#find population of all precincts surrounding Prospect Park one by one

#filtering for population in all 70th precinct blocks and then summing

poppct70 <- pctpop1 %>%filter(precinct == 70)

#Are there any NA values?
sum(is.na(poppct70))

totpop70 <- sum(poppct70$P0010001)

print(totpop70)

#same drill for all other needed precincts

poppct71 <- pctpop1 %>%filter(precinct == 71)

sum(is.na(poppct71))

glimpse(poppct71)

totpop71 <- sum(poppct71$P0010001)

print(totpop71)

poppct72 <- pctpop1 %>%filter(precinct == 72)

sum(is.na(poppct72))

glimpse(poppct72)

totpop72 <- sum(poppct72$P0010001)

print(totpop72)

poppct77 <- pctpop1 %>%filter(precinct == 77)

sum(is.na(poppct77))

totpop77 <- sum(poppct77$P0010001)

print(totpop77)

poppct78 <- pctpop1 %>%filter(precinct == 78)

sum(is.na(poppct78))

totpop78over <- sum(poppct78$P0010001)

#56 people in the 78th pct block data are counted twice because their blocks straddle two precincts, subtract half from 78th

totpop78 <- totpop78over-28

print(totpop78over) #make sure new total is 28 less than this number
print(totpop78)

#Find population of 88th precinct, inside which all of Fort Greene Park is located

poppct88 <- pctpop1 %>%filter(precinct == 88)

sum(is.na(poppct88))

totpop88 <- sum(poppct88$P0010001)

print(totpop88)




