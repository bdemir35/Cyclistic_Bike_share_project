# Loading necessary libraries

library(tidyverse)
library(lubridate)
library(here)
library(janitor)
library(ggplot2)
library(dplyr)

# Loading all csv files to our project

df1 <- read_csv(here("Csv Files", "202101-divvy-tripdata.csv"))
df2 <- read_csv(here("Csv Files", "202102-divvy-tripdata.csv"))
df3 <- read_csv(here("Csv Files", "202103-divvy-tripdata.csv"))
df4 <- read_csv(here("Csv Files", "202104-divvy-tripdata.csv"))
df5 <- read_csv(here("Csv Files", "202105-divvy-tripdata.csv"))
df6 <- read_csv(here("Csv Files", "202106-divvy-tripdata.csv"))
df7 <- read_csv(here("Csv Files", "202107-divvy-tripdata.csv"))
df8 <- read_csv(here("Csv Files", "202108-divvy-tripdata.csv"))
df9 <- read_csv(here("Csv Files", "202109-divvy-tripdata.csv"))
df10 <- read_csv(here("Csv Files", "202110-divvy-tripdata.csv"))
df11 <- read_csv(here("Csv Files", "202111-divvy-tripdata.csv"))
df12 <- read_csv(here("Csv Files", "202112-divvy-tripdata.csv"))

# Combining all data frames into one big data frame

tripdata_2021 <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Writing new csv files 

write.csv(tripdata_2021, file = "tripdata_2021.csv", row.names = FALSE)

# Removing all csv files to make our environment clear

rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

# Exploring combined data frame

View(tripdata_2021)

# Removing unnecessary columns from our data frame

tripdata_2021 <- subset(tripdata_2021, select = -c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))

# Giving member_casual column a meaningful name

tripdata_2021 <- rename(tripdata_2021, customer_type = member_casual)

# Exploring new cleaned data frame

colnames(tripdata_2021) # Gives us name of columns in our data frame
nrow(tripdata_2021) # Gives us number of rows in our data frame
dim_desc(tripdata_2021) # Gives us dimensions of our data frame 
glimpse(tripdata_2021) # Gives us general information about our data frame 
str(tripdata_2021) # Gives us structure information about our data frame
summary(tripdata_2021) # Gives us quick summary of our data frame inc min max median

# We can add new columns that shows us Day, Month, Year for each ride.
# This will allow us to aggregate ride information in different time categories

tripdata_2021$date <- as.Date(tripdata_2021$started_at)
tripdata_2021$day <- format(as.Date(tripdata_2021$date), "%d")
tripdata_2021$month <- format(as.Date(tripdata_2021$date), "%m")
tripdata_2021$year <- format(as.Date(tripdata_2021$date), "%Y")
tripdata_2021$day_of_week <- format(as.Date(tripdata_2021$date), "%A")
tripdata_2021$time <- format(tripdata_2021$started_at, format= "%H:%M")
tripdata_2021$time <- as.POSIXct(tripdata_2021$time, format= "%H:%M")

# Adding ride length calculation to our data frame shows each ride length

tripdata_2021$ride_length <- (as.double(difftime(tripdata_2021$ended_at, tripdata_2021$started_at)))/60

# Checking new data frame for each data type

str(tripdata_2021)

# Removing some rows from our data frame. Ride length is equals to zero or less than zero indicates that bike is taken
# for the quality assurance by the company or, start station name equals to "HQ" or "QR".  

tripdata_2021 <- tripdata_2021[!(tripdata_2021$start_station_name == "HQ QR" | tripdata_2021$ride_length<0),]

# Quick Summary for new dataset 

summary(tripdata_2021$ride_length)

# There are lots of NA entries in ride_length column, Let's remove them

tripdata_2021 <- na.omit(tripdata_2021)

summary(tripdata_2021$ride_length)

# Another save point for the cleaned data

write.csv(tripdata_2021, file = "tripdata_2021_cleaned.csv", row.names = FALSE)

# It is time to compare customer types in ride_length

aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = mean)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = median)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = max)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = min)

# Let's find out the each day of the week 

aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type + tripdata_2021$day_of_week, FUN = mean)

# Make days of the week in order

tripdata_2021$day_of_week <- ordered(tripdata_2021$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type + tripdata_2021$day_of_week, FUN = mean)

# Analyzing the data by customer type

tripdata_2021 %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week) 


# DATA VISUALIZATION


tripdata_2021 %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>% 
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week) %>%
  ggplot(aes(x = day_of_week, y = number_of_rides, fill = customer_type)) + geom_col(position = "dodge")+
  labs(x = "Day of Week", y = "Number of Rides", title = "Rides Per Day of Week", fill = "Type of Membership")+
  scale_y_continuous(breaks = c(150000, 300000, 450000), labels = c("150K", "300K", "450K"))+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))

# This plot shows us casual members use bikes on weekends, and usage in weekdays dramatically lower
# On the other hand members use bikes regularly in whole week for their commute, on weekends like leisure usage drops slightly


tripdata_2021 %>%
  group_by(customer_type, month) %>%
  summarise(number_of_rides = n(), `average_duration(mins)` = mean(ride_length)) %>%
  arrange(customer_type) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) + geom_col(position = "dodge") +
  labs(x = "Month", y = "Number of Rides", title = "Rides Per Month", fill = "Member Type")+
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K"))+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))

# This plot shows us Casual riders are more active in summer seasons and their activity sharply decrease in October
# Member riders shows same trend like Casual riders but their activity decrease slightly. Especially members are more active 
# in Autumn

tripdata_2021 %>%
  group_by(customer_type, rideable_type) %>%
  summarise(number_of_usage = n()) %>%
  ggplot(aes(x = rideable_type, y = number_of_usage, fill = customer_type)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Type", y = "Number of Usage", title = "Bike Usage", fill = "Customer Type") +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mn", "1.5Mn"))+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))

# Classic bike and Electric bike are the most popular bike types using by the customers. Members are the highest user of classic bikes and the electric bikes
# On the other hand docked bikes are only used by the casual members

tripdata_2021 %>%      
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") + labs(x='Days of the week', y='Average duration - Mins', title='Average Ride Time', fill='Type of Membership')+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))

# In general the average  time for each ride for Casual riders are more than the member riders.

  
