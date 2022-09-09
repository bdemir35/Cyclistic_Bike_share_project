# Cyclistic Bike share project
Google Analytics Capstone Project

The bike sharing company wants to make decisions on their customers and products by using Fitbit Tracker Data to find the main differences in behaviour between their two types of users, the “casual” who pays for each ride and the annual member who pays a yearly subscription to the service.

The company wants to improve their earnings reaching out to their “casual” riders, and for that they have to analyze in what aspects the “casual” and the annual customers differ, to be able to create a focused and successful marketing message to the “casual” customers that makes them change to the annual subscription.


## Case Study: How Does a Bike-Share Navigate Speedy Success ?  
### About Company  
In 2016, Cyclistic launched a successful bike-share offering. Since then, the program has grown to a fleet of 5,824 bicycles that
are geotracked and locked into a network of 692 stations across Chicago. The bikes can be unlocked from one station and
returned to any other station in the system anytime.

This case study will follow 6 steps of data analysis

1)**ASK**

Our main focus will be these questions.

* **How do annual members and casual riders use Cyclistic bikes differently?**

* **Why would a casual rider to buy our annual membership?**

Key Stakeholders

* Lily Moreno: The director of marketing and your manager. Moreno is responsible for the development of campaigns
and initiatives to promote the bike-share program. These may include email, social media, and other channels.

* Cyclistic marketing analytics team: A team of data analysts who are responsible for collecting, analyzing, and
reporting data that helps guide Cyclistic marketing strategy. 

* Cyclistic executive team: The notoriously detail-oriented executive team will decide whether to approve the
recommended marketing program

2)**PREPARE**

* We will use Cyclistic’s historical trip [data](https://divvy-tripdata.s3.amazonaws.com/index.html) to analyze and identify trends. The data has been made available by
Motivate International Inc. under this [license](https://ride.divvybikes.com/data-license-agreement).
This is public data that we can use to explore how different customer types are
using Cyclistic bikes. But note that data-privacy issues prohibit us from using riders’ personally identifiable information. This means that we won’t be able to connect pass purchases to credit card numbers to determine if casual riders live in the
Cyclistic service area or if they have purchased multiple single passes.

* The data used in this project includes data between the dates 01.2021 - 12.2021
* R will be used in this project because of the perform calculation easily on this big dataset

3)**PROCESS**

**3.1)** First of all we load necessary libraries 


```{r message=FALSE}
# Loading necessary libraries
library(tidyverse)
library(lubridate)
library(here)
```

**3.2)** Loading our csv files the data between (01.2021 - 12.2021)

```{r message=FALSE}
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
```

**3.3)** Combining all data frames into one data frame

```{r}
tripdata_2021 <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
```

**3.4)** Writing new csv files for our aggregated data frame

```{r}
write.csv(tripdata_2021, file = "tripdata_2021.csv", row.names = FALSE)
```
 
**3.5)** Removing all csv files to make our environment clear

```{r}
rm(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
```

**3.6)** Exploring combined data frame

```{r}
View(tripdata_2021)
```
 
**3.7)** Removing unnecessary columns from our data frame

```{r}
tripdata_2021 <- subset(tripdata_2021, select = -c(start_lat, start_lng, end_lat, end_lng, start_station_id, end_station_id))
```

**3.8)** Giving member_casual column a meaningful name
```{r}
tripdata_2021 <- rename(tripdata_2021, customer_type = member_casual)
```

**3.9)** Exploring new cleaned data frame
```{r}
colnames(tripdata_2021) # Gives us name of columns in our data frame
nrow(tripdata_2021) # Gives us number of rows in our data frame
dim_desc(tripdata_2021) # Gives us dimensions of our data frame 
glimpse(tripdata_2021) # Gives us general information about our data frame 
str(tripdata_2021) # Gives us structure information about our data frame
summary(tripdata_2021) # Gives us quick summary of our data frame inc min max median
```

**3.10)** We can add new columns that shows us Day, Month, Year for each ride.
This will allow us to aggregate ride information in different time categories
```{r}
tripdata_2021$date <- as.Date(tripdata_2021$started_at)
tripdata_2021$day <- format(as.Date(tripdata_2021$date), "%d")
tripdata_2021$month <- format(as.Date(tripdata_2021$date), "%m")
tripdata_2021$year <- format(as.Date(tripdata_2021$date), "%Y")
tripdata_2021$day_of_week <- format(as.Date(tripdata_2021$date), "%A")
tripdata_2021$time <- format(tripdata_2021$started_at, format= "%H:%M")
tripdata_2021$time <- as.POSIXct(tripdata_2021$time, format= "%H:%M")
```

**3.11)** Adding ride length calculation to our data frame shows each ride length
```{r}
tripdata_2021$ride_length <- (as.double(difftime(tripdata_2021$ended_at, tripdata_2021$started_at)))/60
```

**3.12)** Checking new data frame for each data type

```{r}
str(tripdata_2021)
```

**3.13)** Removing some rows from our data frame. Ride length is equals to zero or less than zero indicates that bike is taken
for the quality assurance by the company or, start station name equals to "HQ" or "QR".

```{r}
tripdata_2021 <- tripdata_2021[!(tripdata_2021$start_station_name == "HQ QR" | tripdata_2021$ride_length<0),]
```

**3.14)** Quick Summary for new dataset 

```{r}
summary(tripdata_2021$ride_length)
```

**3.17)**There are lots of NA entries in ride_length column, Let's remove them

```{r}
tripdata_2021 <- na.omit(tripdata_2021)
summary(tripdata_2021$ride_length)
```

**3.16)** Another save point for the cleaned data

```{r}
write.csv(tripdata_2021, file = "tripdata_2021_cleaned.csv", row.names = FALSE)
```
4)**ANALYZE**

Now our data has cleaned and ready to work on it. 

**4.1)** It is time to compare customer types in ride_length

```{r}
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = mean)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = median)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = max)
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type, FUN = min)
```


**4.2)** Let's find out the each day of the week

```{r}
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type + tripdata_2021$day_of_week, FUN = mean)
```

**4.3)** Make days of the week in order

```{r}
tripdata_2021$day_of_week <- ordered(tripdata_2021$day_of_week, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
aggregate(tripdata_2021$ride_length ~ tripdata_2021$customer_type + tripdata_2021$day_of_week, FUN = mean)
```


**4.4)** Analyzing the data by customer type

```{r}
tripdata_2021 %>%
  mutate(day_of_week = wday(started_at, label = TRUE)) %>%
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n() 
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week) 
```

**4.5)** Data Visualization

```{r}
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
```

![](https://github.com/bdemir35/Cyclistic_Bike_share_project/blob/main/Avg_time_usage.png)

+ This plot shows us casual members use bikes on weekends, and usage in weekdays dramatically lower.
On the other hand members use bikes regularly in whole week for their commute, on weekends like leisure usage drops slightly

```{r}
tripdata_2021 %>%
  group_by(customer_type, month) %>%
  summarise(number_of_rides = n(), `average_duration(mins)` = mean(ride_length)) %>%
  arrange(customer_type) %>%
  ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) + geom_col(position = "dodge") +
  labs(x = "Month", y = "Number of Rides", title = "Rides Per Month", fill = "Member Type")+
  scale_y_continuous(breaks = c(100000, 200000, 300000, 400000), labels = c("100K", "200K", "300K", "400K"))+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))
```

+ This plot shows us Casual riders are more active in summer seasons and their activity sharply decrease in October
+ Member riders shows same trend like Casual riders but their activity decrease slightly. Especially members are more active 
in Autumn

```{r}
tripdata_2021 %>%
  group_by(customer_type, rideable_type) %>%
  summarise(number_of_usage = n()) %>%
  ggplot(aes(x = rideable_type, y = number_of_usage, fill = customer_type)) +
  geom_col(position = "dodge") +
  labs(x = "Bike Type", y = "Number of Usage", title = "Bike Usage", fill = "Customer Type") +
  scale_y_continuous(breaks = c(500000, 1000000, 1500000), labels = c("500K", "1Mn", "1.5Mn"))+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))
```

+ Classic bike and Electric bike are the most popular bike types using by the customers. Members are the highest user of classic bikes and the electric bikes
+ On the other hand docked bikes are only used by the casual members

```{r}
tripdata_2021 %>%      
  group_by(customer_type, day_of_week) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, day_of_week)  %>% 
  ggplot(aes(x = day_of_week, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge") + labs(x='Days of the week', y='Average duration - Mins', title='Average Ride Time', fill='Type of Membership')+
  scale_fill_manual(values = c("casual"="Red", "member"="Black"))
```

+ In general the average  time for each ride for Casual riders are more than the member riders.

5)**SHARE**

Our findings from visualizations

**5.1)** Usage Per Day of the Week  
Casual riders use service more often in weekends.

**5.2)** Usage Per Month of the Year  
General service usage sharply increases from March to July for the both customer type. In summer time casual rider category makes peak in July.

**5.3)** Bike Type Usage  
Classic bike and the electric bike are the most popular bikes for each customer type. The docked bike type is using by the casual members only.

**5.4)** Average Time of Usage
Casual riders use bike extremely more than member riders whole through the week.

6)**ACT**

**Recommendations** 

To convert casual riders into member riders,

+ The information we have obtained from the analysis and visuals, if the company organizes annual membership campaigns for casual riders on weekends, it may increase the number of annual memberships.

+ In the same way, looking at the number of usage of bicycles on a monthly basis, there has been a significant increase in both user types since the beginning of spring, annual membership benefits can be delivered to casual riders with a number of marketing campaigns, and an email or application notification can be sent as an example containing the advantages of membership purchase.

+ The docked bike is used almost exclusively by casual riders, so a strategy can be developed to reach users who use this type of bike and convert their membership type to annual membership.

+ Average usage time shows that casual riders use bikes longer than riders with membership. Generally casual riders use the bikes for 25 minutes or longer on average, a number of strategies can be developed in this section and the number of memberships can be increased.
