#loading packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(janitor)
library(tidyr)

##uploading data
getwd()
setwd( "C:/Users/hp/Desktop/Extracted_2021_jan_to_april")

oct_2020 <- read_csv("202010-divvy-tripdata.csv")
nov_2020 <- read_csv("202011-divvy-tripdata.csv")
dec_2020 <- read_csv("202012-divvy-tripdata.csv")
jan_2021 <- read_csv("202101-divvy-tripdata.csv")
feb_2021 <- read_csv("202102-divvy-tripdata.csv")
mar_2021 <- read_csv("202103-divvy-tripdata.csv")
apr_2021 <- read_csv("202104-divvy-tripdata.csv")
may_2021 <- read_csv("202105-divvy-tripdata.csv")
jun_2021 <- read_csv("202106-divvy-tripdata.csv")
jul_2021 <- read_csv("202107-divvy-tripdata.csv")
aug_2021 <- read_csv("202108-divvy-tripdata.csv")
sep_2021 <- read_csv("202109-divvy-tripdata.csv")

colnames(oct_2020)
colnames(nov_2020)
colnames(dec_2020)
colnames(jan_2021)
colnames(feb_2021)
colnames(mar_2021)
colnames(apr_2021)
colnames(may_2021)
colnames(jun_2021)
colnames(jul_2021)
colnames(aug_2021)
colnames(sep_2021)

### Changing the data type for started_at & eneded_at columns

oct_2020$started_at <- as.POSIXct(oct_2020$started_at, format = "%m-%d-%y %H:%M")
oct_2020$ended_at <- as.POSIXct(oct_2020$ended_at, format = "%m-%d-%y %H:%M")

nov_2020$started_at <- as.POSIXct(nov_2020$started_at, format = "%m-%d-%y %H:%M")
nov_2020$ended_at <- as.POSIXct(nov_2020$ended_at, format = "%m-%d-%y %H:%M")

dec_2020$started_at <- as.POSIXct(dec_2020$started_at, format = "%m-%d-%y %H:%M")
dec_2020$ended_at <- as.POSIXct(dec_2020$ended_at, format = "%m-%d-%y %H:%M")

jan_2021$started_at <- as.POSIXct(jan_2021$started_at, format = "%m-%d-%y %H:%M")
jan_2021$ended_at <- as.POSIXct(jan_2021$ended_at, format = "%m-%d-%y %H:%M")

feb_2021$started_at <- as.POSIXct(feb_2021$started_at, format = "%m-%d-%y %H:%M")
feb_2021$ended_at <- as.POSIXct(feb_2021$ended_at, format = "%m-%d-%y %H:%M")

mar_2021$started_at <- as.POSIXct(mar_2021$started_at, format = "%m-%d-%y %H:%M")
mar_2021$ended_at <- as.POSIXct(mar_2021$ended_at, format = "%m-%d-%y %H:%M")

apr_2021$started_at <- as.POSIXct(apr_2021$started_at, format = "%m-%d-%y %H:%M")
apr_2021$ended_at <- as.POSIXct(apr_2021$ended_at, format = "%m-%d-%y %H:%M")

may_2021$started_at <- as.POSIXct(may_2021$started_at, format = "%m-%d-%y %H:%M")
may_2021$ended_at <- as.POSIXct(may_2021$ended_at, format = "%m-%d-%y %H:%M")

jun_2021$started_at <- as.POSIXct(jun_2021$started_at, format = "%m-%d-%y %H:%M")
jun_2021$ended_at <- as.POSIXct(jun_2021$ended_at, format = "%m-%d-%y %H:%M")

jul_2021$started_at <- as.POSIXct(jul_2021$started_at, format = "%m-%d-%y %H:%M")
jul_2021$ended_at <- as.POSIXct(jul_2021$ended_at, format = "%m-%d-%y %H:%M")

aug_2021$started_at <- as.POSIXct(aug_2021$started_at, format = "%m-%d-%y %H:%M")
aug_2021$ended_at <- as.POSIXct(aug_2021$ended_at, format = "%m-%d-%y %H:%M")

sep_2021$started_at <- as.POSIXct(sep_2021$started_at, format = "%m-%d-%y %H:%M")
sep_2021$ended_at <- as.POSIXct(sep_2021$ended_at, format = "%m-%d-%y %H:%M")



###combining datasets into one
bike_df <- rbind(oct_2020,nov_2020,dec_2020,jan_2021,feb_2021,
                 mar_2021,apr_2021,may_2021,jun_2021,jul_2021,aug_2021,
                 sep_2021)

glimpse(bike_df)

bike_df<- janitor::remove_empty(bike_df,which = c("cols"))
bike_df<- janitor::remove_empty(bike_df,which = c("rows"))

complete.cases(bike_df)

bike_df_1 <- bike_df[complete.cases(bike_df), ]

bike_df_1  <- na.omit(bike_df)


bike_df_1<- mutate(bike_df_1, start_station_id = as.numeric(start_station_id), end_station_id =
                    as.numeric(end_station_id))
str(bike_df_1)

###renaming columns
bike_df_1 <- bike_df_1 %>% rename(trip_id = ride_id, ride_type = rideable_type, start_time = started_at, end_time= ended_at)
glimpse(bike_df_1)

###removing columns that are out of scope

bike_df_1 <- subset(bike_df_1, select = -c(start_lat:end_lng))
glimpse(bike_df_1)

bike_df_1 <- bike_df_1%>% distinct()

str(bike_df_1)

### Calculating ride length in minutes and create a new column 


bike_df_1$ride_length <- as.double(bike_df_1$end_time- bike_df_1$start_time)/60
str(bike_df_1)
summary(bike_df_1$ride_length)

### Creating new columns(day_of_week, month, time)


bike_df_1$day_of_week <- weekdays(bike_df_1$start_time)
bike_df_1$month <- months(bike_df_1$start_time)
bike_df_1$time <- hour(bike_df_1$start_time)
str(bike_df_1)


### Removing the entries with ride length less than 0


bike_df_1 <- subset(bike_df_1, -ride_length<0)
str(bike_df_1)



## Analysis

### Some calculations on the dataset

####total ride_length by group

bike_df_1 %>% group_by (member_casual) %>% summarize(total_time = sum(ride_length))



summary(bike_df_1$ride_length)



####summary of ride length for each group

bike_df_1 %>% 
  group_by(member_casual) %>% 
  summarize(min_ride_length = min(ride_length), max_ride_length = max(ride_length), 
            mean_ride_length = mean(ride_length), median_ride_length = median(ride_length))


####no of rides in each day of the week


bike_df_1 %>% group_by(member_casual, day_of_week) %>% summarize(num_of_rides = n())     

###no of rides per month and no of rides per ride type for each group


bike_df_1 %>%  group_by(member_casual, month) %>%  summarize(num_of_rides= n())
bike_df_1 %>%  group_by(member_casual, ride_type) %>% summarize(num_of_rides = n())


## Visualizations

### Total Rides Vs. Day of the week

options(scipen = 999)
bike_df_1 %>%  
  group_by(member_casual, day_of_week) %>% 
  summarize(num_of_rides = n()) %>% 
  arrange(day_of_week) %>% 
  ggplot(aes(x = day_of_week, y = num_of_rides, fill = member_casual))+
  geom_col(position = "dodge", width = 0.5)+
  labs(title ="Total rides Vs. Day of the week",caption = "By: kirui ian mark")+
  scale_y_continuous()


The graph shows that the causal riders using the service more on saturdays and sundays,
while for the rest of the week the demand is pretty average. On the other hand, the demand 
from the members is quite the same throughout the week and more than casual riders in the weekdays.


### Total rides Vs. month

bike_df_1 %>%  
  group_by(member_casual, month) %>% 
  summarize(num_of_rides = n()) %>% 
  arrange(desc(num_of_rides)) %>% 
  ggplot(aes(x = month, y = num_of_rides, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(position = "dodge", width = 0.5)+
  labs(title ="Total rides Vs. month",caption = "By :Kirui Ian Mark") +
  scale_y_continuous()

From the visual we can understand that the demand was at peak from both causual and
membership riders in the months of June and July, and it was low from December to March.
Also per month we can see that membership riders take more rides than casual riders
throughout the year.

### Average number of rides Vs. day of week

bike_df_1 %>%  
  group_by(member_casual, day_of_week) %>% 
  summarize(avg_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x = day_of_week, y = avg_ride_length, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(position = "dodge", width = 0.5)+
  labs(title ="Average number of rides Vs. day of week", caption = "By: Kirui Ian Mark")+
  scale_y_continuous()

Throughout the week average ride length of casual riders is more than that of a 
member.On weekends the average ride length was the maximum from both the member
types compared to weekdays.


### Average number of rides Vs. month

bike_df_1 %>%  
  group_by(member_casual, month) %>% 
  summarize(avg_ride_length = mean(ride_length)) %>% 
  ggplot(aes(x = month, y = avg_ride_length, fill = member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(position = "dodge", width = 0.5)+
  labs(title ="Average number of rides Vs. month", caption = "By: Kirui Ian Mark")+
  scale_y_continuous()

The average ride length by casual riders was more than membership riders throughout
the year. Its more than 30 minutes for most of the months by casual riders(except the months of
august,december and september) and maximum in February accounting to 50 minutes. While 
membership riders average ride length never crossed 20 minutes.



### Ride_type Vs Number of rides

bike_df_1 %>% 
  group_by(ride_type, member_casual) %>% 
  summarize(num_of_rides = n()) %>% 
  ggplot(aes(x = ride_type, y = num_of_rides, fill= member_casual))+
  theme(axis.text.x = element_text(angle = 45))+
  geom_col(position = "dodge", width = 0.5)+
  labs(title ="Ride_type Vs Number of rides", caption = "By: Kirui Ian Mark")+
  scale_y_continuous()

Casual riders use classical bikes more followed by electric bikes. While membership
riders use classic bikes more while the demand was pretty much same for both electric
and docked bikes.


### Number of rides throughout the day

bike_df_1 %>% group_by(member_casual, time) %>% 
 summarize(num_of_rides = n()) %>% 
 ggplot(aes(x= time, y = num_of_rides, color = member_casual))+ 
geom_line() + labs(title = "Number of rides throughout the day",caption = "By: Kirui Ian Mark")
scale_x_continuous()

The peak demand from both the types of riders was from 5 PM to 7 PM and the lowest
was during the morning hours 2 AM to 4 AM. We can also see the seocnd peak from 
membership riders 7 AM to 9 AM.


## Takeaways:

$ The average ride length by casual riders is almost twice to that of membership riders throughout the day and months of the year.

$ Casual riders use the service more during weekends, while the demand from membership riders was same for the entire week.

$ Casual riders prefer docked bikes more while membership riders prefer classic bikes.


## Recommendations:

$ Provide attractive packages during the weekdays to attract more casual riders, as their demand is very low in weekdays.

$ Neeed to provide memberships with some discount to ensure more casual riders take up the membership.