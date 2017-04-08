library(stringr)
library(dplyr)
library(date)
library(lubridate)
library(tidyverse)
library(knitr)
library(data.table)
library(ggplot2)
library(ggmap)

#Remove all variable and data command - rm(list=ls())

#Replace blank spaces with NA
operations <- read.csv("S:/Niall/Documents/Big Data Project/BigData/operations.csv", na.strings=c("", "NA"))
operations <- read.csv("C:/Users/Niall/Documents/Big Data/BigData/operations.csv", na.strings=c("", "NA"))
#------------------------------------Data Cleaning---------------------------------------------------
#I used the following from https://www.kaggle.com/cswingle/d/usaf/world-war-ii/preliminary-look-at-the-data just so 
#I could understand the data and what was missing

#We'll read in the data, snake-case the column names, and process the mm/dd/yy dates.


names(operations) <- gsub('[()]', '', gsub(' ', '_', str_to_lower(names(operations))))
operations <- operations %>%mutate(mission.date = mdy(mission.date))

#I used this command to look at the percentage of na's in all columns
colMeans(is.na(operations))

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

all_target_long_lat <- operations %>% 
  select(target.latitude, target.longitude) %>% 
  group_by(target.latitude, target.longitude) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_target_long_lat)

#-Originally I made the targetr longitudes and latitudes UNKOWN but this caused problems
#-when I wanted to map them, so I left them as NA
#Replace Target Missing Longitute With 0
#operations$target.longitude <- as.character(operations$target.longitude)
#operations$target.longitude[is.na(operations$target.longitude)] <- "UNKNOWN"

#Replace Target Missing Latitude With 0
#operations$target.latitude <- as.character(operations$target.latitude)
#operations$target.latitude[is.na(operations$target.latitude)] <- "UNKNOWN"

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

operations$mission.type[operations$mission.type==15] <- "UNKNOWN OR OTHER"

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


#--------Looking at the takeoff base column
#Look at all bases
missing_base <- operations %>% 
  select(takeoff.base) %>% 
  group_by(takeoff.base) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_base)

#Replace Missing Base
operations$takeoff.base <- as.character(operations$takeoff.base)
operations$takeoff.base[is.na(operations$takeoff.base)] <- "UNKNOWN"


#--------Looking at the takeoff location column
#Look at all takeoff locations
missing_takoff_loc <- operations %>% 
  select(takeoff.location) %>% 
  group_by(takeoff.location) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_takoff_loc)

#Replace Missing location
operations$takeoff.location <- as.character(operations$takeoff.location)
operations$takeoff.location[is.na(operations$takeoff.location)] <- "UNKNOWN"

#--------Looking at the targetID column
#Look at all targetID's
all_targetID <- operations %>% 
  select(target.id) %>% 
  group_by(target.id) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_targetID)
#Replace Missing targetID
operations$target.id <- as.character(operations$target.id)
operations$target.id[is.na(operations$target.id)] <- "UNKNOWN"

#--------Looking at the target city column
#Look at all target cities
all_target_city <- operations %>% 
  select(target.city) %>% 
  group_by(target.city) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_target_city)
#Replace Missing target city
operations$target.city <- as.character(operations$target.city)
operations$target.city[is.na(operations$target.city)] <- "UNKNOWN"
#Combine the previous missing entries which were labelled "Unidentified
operations$target.city[operations$target.city=="UNIDENTIFIED"] <- "UNKNOWN"

#When an area that wasnt a city was bombed, its target city was given coordinates instead
#Because of this I decided to rename the column from target.city to target.city.or.area 
colnames(operations)[14] <- "target.city.or.area"

#--------Looking at the target longitude and latitude column
#--all takeoff longitude and latitude
missing_takeoff_long_lat <- operations %>% 
  select(takeoff.latitude, takeoff.longitude) %>% 
  group_by(takeoff.latitude, takeoff.longitude) %>% 
  summarize(n = n()) %>%
  arrange(desc(n))
View(missing_takeoff_long_lat)

#Replace Target Missing Longitute With 0
operations$takeoff.longitude <- as.character(operations$takeoff.longitude)
operations$takeoff.longitude[is.na(operations$takeoff.longitude)] <- "UNKNOWN"

#Replace Target Missing Latitude With 0
operations$takeoff.latitude <- as.character(operations$takeoff.latitude)
operations$takeoff.latitude[is.na(operations$takeoff.latitude)] <- "UNKNOWN"

#--------Looking at the target type column
#Look at all target type
all_target_type <- operations %>% 
  select(target.type) %>% 
  group_by(target.type) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_target_type)

#Replace Missing target type
operations$target.type <- as.character(operations$target.type)
operations$target.type[is.na(operations$target.type)] <- "UNKNOWN"
#Combine the previous missing entries which were labelled "Unidentified
operations$target.type[operations$target.type=="UNIDENTIFIED TARGET"] <- "UNKNOWN"


#--------Looking at the target industry column
#Look at all target industry
all_target_industry <- operations %>% 
  select(target.industry) %>% 
  group_by(target.industry) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_target_industry)

#Replace Missing target type
operations$target.industry <- as.character(operations$target.industry)
operations$target.industry[is.na(operations$target.industry)] <- "UNKNOWN"
#Combine the previous missing entries which were labelled "Unidentified
operations$target.industry[operations$target.industry=="UNIDENTIFIED TARGETS"] <- "UNKNOWN"

#--------Looking at the target priority column
#The priority is represensted by number, the THOR data dictionary states that
#-1 Primary Target
#-2 Seconary Target
#-3 Target of Opportunity
#-4 Target of Last Resort
#-9 Not Indicated
#I decided to replace the NA's with 9's. I chose not to replace the numbers with their corresponding meaning as I
# wanted to use the numbers to rank them

#Look at all target priorities
all_target_priority <- operations %>% 
  select(target.priority) %>% 
  group_by(target.priority) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_target_priority)

#Replace Missing target priorities
operations$target.priority <- as.character(operations$target.priority)
operations$target.priority[is.na(operations$target.priority)] <- 9

#Replace all the incorrect values
operations$target.priority[operations$target.priority==0] <- 9
operations$target.priority[operations$target.priority==5] <- 9
operations$target.priority[operations$target.priority==6] <- 9
operations$target.priority[operations$target.priority==7] <- 9
operations$target.priority[operations$target.priority==8] <- 9
operations$target.priority[operations$target.priority=="SAAF"] <- 9
operations$target.priority[operations$target.priority=="RNAS"] <- 9
operations$target.priority[operations$target.priority=="P"] <- 9
operations$target.priority[operations$target.priority=="A"] <- 9
operations$target.priority[operations$target.priority=="O"] <- 9

#--------Looking at the altitude column
all_altitude <- operations %>% 
  select(altitude..hundreds.of.feet.) %>% 
  group_by(altitude..hundreds.of.feet.) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_altitude)

#This time, instead of choosing to replace the na's with unknown I decided to take the mean of the column and add 
#add that where there a na's in the data

operations$altitude..hundreds.of.feet.[is.na(operations$altitude..hundreds.of.feet.)] <- round(mean(operations$altitude..hundreds.of.feet., na.rm = TRUE))

#--------Looking at the airborne aircraft column
all_aircraft_airborne <- operations %>% 
  select(airborne.aircraft) %>% 
  group_by(airborne.aircraft) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_aircraft_airborne)


colMeans(is.na(operations))


#--------Looking at other aircraft columns
#airborne.aircraft, bombing.aircraft, aircraft.returned, aircraft.failed, aircraft.damaged and aircraft.lost all
#have above 50% na's in them. Because of this it would not be the best decision to simply use the mean or median
#to replace the missing data. Also being numerical it didn't make sense to put unknown in the missing values instead.
#So I decided to remove the columns, this also meant removing attacking aircraft as it would be useless by its self.

operations$airborne.aircraft <- NULL
operations$bombing.aircraft <- NULL
operations$aircraft.returned <- NULL
operations$aircraft.failed <- NULL
operations$aircraft.damaged <- NULL
operations$aircraft.lost <- NULL
operations$attacking.aircraft <- NULL

#--------Looking at explosives columns
#The data again here is filled with empty data, this is also because the categories only have entires if those types of
#explosives were dropped, other wise its left null. I also knew from some domain knowledge that there were two important
#one offs were the bombs dropped were not listed, the two atomic bombs which were dropped during the war. These two
#bombings were far too important to leave out so I decided to merge the columns into a single column called bomb type
#then I also wouldnt have to worry about the amount of bombs on board.

operations["bomb.type"] <- NA

operations$bomb.type[!is.na(operations$high.explosives)] <- "HIGH EXPLOSIVES"
operations$bomb.type[!is.na(operations$incendiary.devices)] <- "INCENDIARY"
operations$bomb.type[!is.na(operations$fragmentation.devices)] <- "FRAGMENTATION"
operations$bomb.type[operations$high.explosives.type=="ATOMIC BOMB (LITTLE BOY)"] <- "ATOMIC BOMB"
operations$bomb.type[operations$high.explosives.type=="ATOMIC BOMB (FAT MAN)"] <- "ATOMIC BOMB"
operations$bomb.type[operations$target.type=="PROPAGANDA"] <- "LEAFLET DROPPING"
operations$bomb.type[operations$mission.type=="LEAFLET DROPPING"] <- "LEAFLET DROPPING"
operations$bomb.type[is.na(operations$bomb.type)] <- "UNKNOWN"

all_bomb_types <- operations %>% 
  select(bomb.type) %>% 
  group_by(bomb.type) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_bomb_types)

#Now I can remove all the exlosives column
operations$high.explosives <- NULL
operations$high.explosives.type <- NULL
operations$high.explosives.weight..pounds. <- NULL
operations$high.explosives.weight..tons. <- NULL
operations$incendiary.devices <- NULL
operations$incendiary.devices.type <- NULL
operations$incendiary.devices.weight..pounds. <- NULL
operations$incendiary.devices.weight..tons. <- NULL
operations$fragmentation.devices <- NULL
operations$fragmentation.devices.weight..pounds. <- NULL
operations$fragmentation.devices.weight..tons. <- NULL
operations$fragmentation.devices.type <- NULL
operations$total.weight..pounds. <- NULL
operations$total.weight..tons. <- NULL


#--------Looking at the final columns
colMeans(is.na(operations))

#Both time over target and bomb damage assesment have over 99% of their data missing so it's best to
#just remove them. For sourceID I will just add UNKOWN
operations$time.over.target <- NULL
operations$bomb.damage.assessment <- NULL

all_source_id <- operations %>% 
  select(source.id) %>% 
  group_by(source.id) %>%
  summarize(n = n()) %>%
  arrange(desc(n))
View(all_source_id)

operations$source.id[is.na(operations$source.id)] <- "UNKNOWN"


#------------------------------------Data Analytics---------------------------------------------------


#Here we can see 1944 had the greatest amount of bombings and 1939 had none because very little
#happened during the first 8 months of the war, a period which is often refered to as the "phoney war"

year <- format(as.Date(operations$mission.date, format="%d/%m/%Y"),"%Y")

colours <- c("blue", "orange")
year_counts <- table(year)
barplot(year_counts, main="Years of The War", 
        xlab="Individual Years", ylab="Amount of Bombing Runs", border="black", col=colours)

#Most popular bombers

plane_counts <- table(operations$aircraft.series)
barplot(plane_counts, main="Most Popular Bomber of WWII", 
        xlab="Different Bombers", ylab="Amount of Bombing Runs", border="black", col=colours)

#Most common type of target
target_type_counts <- table(operations$target.type)
barplot(target_type_counts, main="Most Common Target Type", 
        xlab="Different Targets", ylab="Amount of Bombing Runs", border="black", col=colours)

#--Map the co-ordinates onto a map

#Code borrowed from https://stackoverflow.com/questions/23130604/plot-coordinates-on-map
#While trying to do this I asked two seperate questions myself on stackoverflow:
#-https://stackoverflow.com/questions/43233044/error-when-trying-to-plot-coordinates-in-r
#-https://stackoverflow.com/questions/43296028/no-non-missing-arguments-to-min-and-max

#Create a subset of data where the target longitude and latitudes are known
write.csv(operations, file = "S:/Niall/Documents/Big Data Project/BigData/CleanOperations.csv")
smalloperations <- read.csv("S:/Niall/Documents/Big Data Project/BigData/smalloperations.csv", na.strings=c("", "NA"))
smalloperations <- read.csv("C:/Users/Niall/Documents/Big Data/BigData/smalloperations.csv", na.strings=c("", "NA"))
newdata <- na.omit(operations)
lon <- c(newdata$target.longitude)
lat <- c(newdata$target.latitude)
df <- as.data.frame(cbind(lon,lat))


#-European and Mediterranean Theaters

#getting the map
mapgilbert <- get_map(location = c(lon = 10.458676, lat = 50.296548), zoom = 4,
                      maptype = "satellite", scale = 1)

#plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#-The Pacific Theater
#getting the map
mapgilbert <- get_map(location = c(lon = 152.954403, lat = 28.873755), zoom = 3,
                      maptype = "satellite", scale = 1)

#plotting the map with some points on it
ggmap(mapgilbert) +
  geom_point(data = df, aes(x = lon, y = lat, fill = "red", alpha = 0.8), size = 1, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)






