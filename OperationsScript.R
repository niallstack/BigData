library(stringr)
library(dplyr)
library(date)
library(lubridate)
library(tidyverse)
library(knitr)

#Replace blank spaces with NA
operations <- read.csv("S:/Niall/Documents/Big Data Project/BigData/operations.csv", na.strings=c("", "NA"))
#------------------------------------Data Cleaning---------------------------------------------------
#I used the following from https://www.kaggle.com/cswingle/d/usaf/world-war-ii/preliminary-look-at-the-data just so 
#I could understand the data and what was missing

#We'll read in the data, snake-case the column names, and process the mm/dd/yy dates.


names(operations) <- gsub('[()]', '', gsub(' ', '_', str_to_lower(names(operations))))
operations <- operations %>%mutate(mission.date = mdy(mission.date))

#Here's a couple quick summaries mostly to see how much missing data there is. It doesn't look good...

#--Missing Theater and Country
missing_theater_Country <- operations %>% 
select(theater.of.operations, country) %>% 
group_by(theater.of.operations, country) %>% 
summarize(n = n()) %>%
arrange(desc(n))
View(missing_theater_Country)

#Replace missing Theater with Unknown
operations$theater.of.operations <- as.character(operations$theater.of.operations)
operations$theater.of.operations[is.na(operations$theater.of.operations)] <- "UNKNOWN"


#Replace MissingCountry with Unknown
operations$country <- as.character(operations$country)
operations$country[is.na(operations$country)] <- "UNKNOWN"

#--Missing target countries
missing_target_country <- operations %>% 
select(target.country) %>% 
group_by(target.country) %>%
summarize(n = n()) %>%
arrange(desc(n))
View(missing_target_country)

#Replace Missing Target Countries
operations$target.country <- as.character(operations$target.country)
operations$target.country[is.na(operations$target.country)] <- "UNKNOWN"
#Combine the previous missing entries which were labelled "Unknown or not indicated
operations$target.country <- as.character(operations$target.country)
operations$target.country[operations$target.country=="UNKNOWN OR NOT INDICATED"] <- "UNKNOWN"

#--Missing Target Long and Latit
operations %>%
  select(target.latitude, target.longitude) %>%
  mutate(total = 1,
         not_null = ifelse(!is.na(target.latitude) & !is.na(target.longitude), 1, 0)) %>%
  summarize(total = sum(total),
            not_null = sum(not_null)) %>%
  mutate(good_data = not_null / total * 100)

#Replace Target Missing Longitute With 0
operations$target.longitude <- as.character(operations$target.longitude)
operations$target.longitude[is.na(operations$target.longitude)] <- 0

#Replace Target Missing Latitude With 0
operations$target.latitude <- as.character(operations$target.latitude)
operations$target.latitude[is.na(operations$target.latitude)] <- 0

#--Missing mission date and weight of high explosives
operations %>%
  select(mission.date, high.explosives.weight..tons.) %>%
  mutate(year = year(mission.date),
         total = 1,
         not_null = ifelse(!is.na(high.explosives.weight..tons.), 1, 0)) %>%
  group_by(year) %>%
  summarize(total = sum(total),
            not_null = sum(not_null)) %>%
  mutate(good_data = not_null / total * 100)

#--Summing the high explosives by year removing null
explosives <- operations %>%
  select(mission.date, high.explosives.weight..tons.) %>%
  filter(!is.na(high.explosives.weight..tons.)) %>%
  mutate(year = factor(year(mission.date))) %>%
  group_by(year) %>%
  summarize(high.explosives.weight..tons. = sum(high.explosives.weight..tons.))

knit_hooks$set(inline = function(x) {
  if (is.numeric(x)) {
    prettyNum(x, big.mark = ",")
  } else {
    x
  }
})
kable(explosives, format = "rst", digits = 0)
#I stopped using the mentioned preliminary look at the data here

#--------Looking at Callsgin column
#Callsign summary
summary(operations$callsign)

#Finding percentage of callsing that is null
callsignsNull <- is.na(operations$callsign)
callsignNotNull <- !is.na(operations$callsign)
prop.table(table(callsignsNull, callsignNotNull))

#Also Remove callsign data
rm(callsignNotNull, callsignsNull)

#Callsigns is clearly poor so might as well remove it
operations$callsign <- NULL

#--------Looking at the Air force column
force_null <- is.na(operations$air.force)
force_not_null <- !is.na(operations$air.force)
prop.table(table(force_null, force_not_null))

#Air Force Missing
missing_force <- operations %>% 
  select(air.force) %>% 
  group_by(air.force) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_force)

#Replace Missing Air Force
operations$air.force <- as.character(operations$air.force)
operations$air.force[is.na(operations$air.force)] <- "UNKNOWN"

#--------Looking at the Unit ID column
#UnitID Missing
missing_unit_id <- operations %>% 
  select(unit.id) %>% 
  group_by(unit.id) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_unit_id)

#Percent of UnitID's missing
unit_id_null <- is.na(operations$unit.id)
unit_id_not_null <- !is.na(operations$unit.id)
prop.table(table(unit_id_null, unit_id_not_null))

#Removing UnitID
operations$unit.id <- NULL
#Also remove missing unitID data
rm(missing_unit_id, unit_id_not_null, unit_id_null)


#--------Looking at the Aircraft Series column
#Aircraft Series missing
missing_series <- operations %>% 
  select(aircraft.series) %>% 
  group_by(aircraft.series) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_series)

#Replace Missing Series
operations$aircraft.series <- as.character(operations$aircraft.series)
operations$aircraft.series[is.na(operations$aircraft.series)] <- "UNKNOWN"

#--------Looking at the Mission type column
#--This column had numbers, I found what these numbers meant by finding the original creators of the dataset the Theater history of operations(THOR), glossary
#--This had mission types ranging with ID's from 1-16, with 99 designated "other or not indicated". I decided to change the
#---column to have the corresponding string instead of the ID and any ID's not between 1-16 would become "Unknown or Other"
#--This gloassary can be found at - https://www.dds.mil/data/thor_data_dictionary_2016.pdf
#--For some reason on the list of operations that had 1-16, there was no 8 or 15 which seemed to be skipped, so I had to make all those Unknown or Other

all_mission_types <- operations %>% 
  select(mission.type) %>% 
  group_by(mission.type) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_mission_types)

#Replace ID 1
operations$mission.type <- as.character(operations$mission.type)
operations$mission.type[operations$mission.type==1] <- "OBJECTIVE BOMBING"

#Replace ID 2
operations$mission.type[operations$mission.type==2] <- "PATROL"

#Replace ID 3
operations$mission.type[operations$mission.type==3] <- "ESCORT OR COVER"

#Replace ID 4
operations$mission.type[operations$mission.type==4] <- "INTERCEPTION OR INTRUDER"

#Replace ID 5
operations$mission.type[operations$mission.type==5] <- "STRAFING"

#Replace ID 6
operations$mission.type[operations$mission.type==6] <- "RECONNAISSANCE OR SEARCH"

#Replace ID 7
operations$mission.type[operations$mission.type==7] <- "DIVERSIONARY"

#Skip 8 for now

#Replace ID 9
operations$mission.type[operations$mission.type==9] <- "FIGHTER SWEEP"

#Replace ID 10
operations$mission.type[operations$mission.type==10] <- "COMBINED BOMBING AND STRAFING"

#Replace ID 11
operations$mission.type[operations$mission.type==11] <- "TRANSPORT (NON-COMBAT)"

#Replace ID 12
operations$mission.type[operations$mission.type==12] <- "SEA SEARCH/ATTACK/PATROL"

#Replace ID 13
operations$mission.type[operations$mission.type==13] <- "LEAFLET DROPPING"

#Replace ID 14
operations$mission.type[operations$mission.type==14] <- "TRANSPORT (COMBAT)"

#Skip 15 for now

#Replace ID 16
operations$mission.type[operations$mission.type==16] <- "STAGING"

#Change appropriate values to UNKOWN OR OTHER
operations$mission.type[operations$mission.type=="B17"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="BEAUFORT"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="P38"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="WELLINGTON"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="99"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="17"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="8"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="30"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="18"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="19"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="31"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="40"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="55"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="95"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="20"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="24"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="32"] <- "UNKNOWN OR OTHER"
operations$mission.type[operations$mission.type=="41"] <- "UNKNOWN OR OTHER"
operations$mission.type[is.na(operations$mission.type)] <- "UNKNOWN OR OTHER"


all_mission_types <- operations %>% 
  select(mission.type) %>% 
  group_by(mission.type) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_mission_types)
























