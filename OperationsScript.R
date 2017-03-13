library(stringr)
library(dplyr)
library(date)
library(lubridate)
library(tidyverse)
library(knitr)
#Replace blank spaces with NA
operations <- read.csv("C:/Users/t00174406/Desktop/operations.csv", na.strings=c("", "NA")) # if your file is tab delimited
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

#Callsign not null
operations %>%
  select(callsign) %>%
  mutate(total = 1,
         not_null = ifelse(!is.na(callsign), 1)) %>%
  summarize(total = sum(total),
            not_null = sum(not_null)) %>%
  mutate(good_data = not_null / total * 100)

