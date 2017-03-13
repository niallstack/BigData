library(stringr)
library(dplyr)
library(date)
library(lubridate)
library(tidyverse)
library(knitr)

#Replace blank spaces with NA
operations <- read.csv("S:/Niall/Documents/Big Data Project/BigData/operations.csv", na.strings=c("", "NA"))
#I used the following from https://www.kaggle.com/cswingle/d/usaf/world-war-ii/preliminary-look-at-the-data just so 
#I could understand the data and what was missing

#We'll read in the data, snake-case the column names, and process the mm/dd/yy dates.


names(operations) <- gsub('[()]', '', gsub(' ', '_', str_to_lower(names(operations))))
operations <- operations %>%mutate(mission.date = mdy(mission.date))

#Here's a couple quick summaries mostly to see how much missing data there is. It doesn't look good...

#Missing Theater and Country
operations %>% 
select(theater.of.operations, country) %>% 
group_by(theater.of.operations, country) %>% 
summarize(n = n()) %>%
arrange(desc(n))

#Missing target countries
operations %>% 
select(target.country) %>% 
group_by(target.country) %>%
summarize(n = n()) %>%
arrange(desc(n))

#Missing Long and Latit
operations %>%
  select(target.latitude, target.longitude) %>%
  mutate(total = 1,
         not_null = ifelse(!is.na(target.latitude) & !is.na(target.longitude), 1, 0)) %>%
  summarize(total = sum(total),
            not_null = sum(not_null)) %>%
  mutate(good_data = not_null / total * 100)

#Missing mission date and weight of high explosives
operations %>%
  select(mission.date, high.explosives.weight..tons.) %>%
  mutate(year = year(mission.date),
         total = 1,
         not_null = ifelse(!is.na(high.explosives.weight..tons.), 1, 0)) %>%
  group_by(year) %>%
  summarize(total = sum(total),
            not_null = sum(not_null)) %>%
  mutate(good_data = not_null / total * 100)

#Summing the high explosives by year removing null
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

#Callsign summary
summary(operations$callsign)

#Finding percentage of callsing that is null
callsignsNull <- is.na(operations$callsign)
callsignNotNull <- !is.na(operations$callsign)
prop.table(table(callsignsNull, callsignNotNull))
#prop.table(table(!is.na(operations$callsign),is.na(operations$callsign)))

operations %>% select_(.dots=setdiff(names(.),drop.cols))


