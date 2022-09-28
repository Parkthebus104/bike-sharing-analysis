### Part 1

##Importing data

data_q1 <- read.csv("Trips_2019_Q1.csv")
data_q2 <- read.csv("Trips_2019_Q2.csv")
data_q3 <- read.csv("Trips_2019_Q3.csv")
data_q4 <- read.csv("Trips_2019_Q4.csv")

##Homogenizing column names

colnames(data_q2) <- c('trip_id', 'start_time', 'end_time', 'bikeid','tripduration',
                       'from_station_id','from_station_name',	'to_station_id',
                       'to_station_name',	'usertype',	'gender',	'birthyear')

##Aggregating data into one dataframe

trips_data <- rbind(data_q1, data_q2, data_q3, data_q4)
write.csv(trips_data, file = "trips_data.csv")

### Part 2

##Setting up the environment

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
trips_data <- read.csv("trips_data.csv")

##Verifying dataframe structure, checking for duplicates and inconsistent formatting
str(trips_data)
get_dupes(trips_data, trip_id)
table(trips_data$usertype)
table(trips_data$gender)

trips_data$usertype <- sub("Customer", "Casual", trips_data$usertype)
trips_data$gender <- sub("^$", "NA", trips_data$gender)
trips_data$tripduration <- gsub(",","",trips_data$tripduration)
trips_data$tripduration <- as.numeric(trips_data$tripduration)

##Adding new column for day of week and ride length in HMS format

trips_data$day_of_week <- wday(trips_data$start_time)
trips_data$month <- month(trips_data$start_time, label = TRUE)
trips_data$ride_length <- hms::as_hms(difftime(trips_data$end_time,
                                          trips_data$start_time))
trips_data$age = 2019 - trips_data$birthyear
trips_data$age_category <- cut(trips_data$age, breaks = c(-Inf,18,30,59,Inf),
                               labels = c("Children","YA","Adults","Seniors"))

##Descriptive Analysis

#Breakdown of usertype with gender & age
tabyl(trips_data, usertype, gender, age_category) %>%
  adorn_percentages("col") %>%
  adorn_pct_formatting(digits = 1)
ggplot(trips_data) + geom_bar(mapping = aes(x = age_category, fill = usertype)) +
  scale_y_continuous(trans = 'log10')

#Trip length summary across different categories
trips_data %>% 
  select(usertype, gender, age_category, tripduration) %>% 
  group_by(usertype, gender, age_category) %>%
  summarise(mean_duration = mean(tripduration), 
            median_duration = median(tripduration), 
            minimum = min(tripduration),
            maximum = max(tripduration))

#Month-wise trends in ridership
tabyl(trips_data, month, usertype)
ggplot(trips_data) + geom_bar(mapping = aes(x = month, fill = usertype)) + 
  scale_y_continuous(labels = comma)