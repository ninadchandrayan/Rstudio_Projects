#2nd Phase of Data Analysis- PREPARE

#Load libraries

library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)

#Load Data sets by importing CSV files

tripdata_202004 <- read.csv("202004-divvy-tripdata.csv")
tripdata_202005 <- read.csv("202005-divvy-tripdata.csv")
tripdata_202006 <- read.csv("202006-divvy-tripdata.csv")
tripdata_202007 <- read.csv("202007-divvy-tripdata.csv")
tripdata_202008 <- read.csv("202008-divvy-tripdata.csv")
tripdata_202009 <- read.csv("202009-divvy-tripdata.csv")
tripdata_202010 <- read.csv("202010-divvy-tripdata.csv")
tripdata_202011 <- read.csv("202011-divvy-tripdata.csv")
tripdata_202012 <- read.csv("202012-divvy-tripdata.csv")
tripdata_202101 <- read.csv("202101-divvy-tripdata.csv")
tripdata_202102 <- read.csv("202102-divvy-tripdata.csv")
tripdata_202103 <- read.csv("202103-divvy-tripdata.csv")
tripdata_202104 <- read.csv("202104-divvy-tripdata.csv")
tripdata_202105 <- read.csv("202105-divvy-tripdata.csv")
tripdata_202106 <- read.csv("202006-divvy-tripdata.csv")
tripdata_202107 <- read.csv("202107-divvy-tripdata.csv")


#Checking the column names of each data set for consistency
colnames(tripdata_202004)
colnames(tripdata_202005)
colnames(tripdata_202006)
colnames(tripdata_202007)
colnames(tripdata_202008)
colnames(tripdata_202009)
colnames(tripdata_202010)
colnames(tripdata_202011)
colnames(tripdata_202012)
colnames(tripdata_202101)
colnames(tripdata_202102)
colnames(tripdata_202103)
colnames(tripdata_202104)
colnames(tripdata_202105)
colnames(tripdata_202106)
colnames(tripdata_202107)

##The column names of each data set are similar indeed! 

#Now Checking data structures and data types for all data frames

str(tripdata_202004)
str(tripdata_202005)
str(tripdata_202006)
str(tripdata_202007)
str(tripdata_202008)
str(tripdata_202009)
str(tripdata_202010)
str(tripdata_202011)
str(tripdata_202012)
str(tripdata_202101)
str(tripdata_202102)
str(tripdata_202103)
str(tripdata_202104)
str(tripdata_202105)
str(tripdata_202106)
str(tripdata_202107)

##from the above result it can be observed that start_station_id and end_station_id are not consistent in all data sets. Data cleaning is required

#DATA TRANSFORMATIoN AND CLEANING
 #As it was noticed that start_station_id and end_station_id are not consistent in all data, the ones in tripdata_202004,tripdata_202005, tripdata_202006, tripdata_202007, tripdata_202008, tripdata_202009, tripdata_202010, tripdata_202011, tripdata_202106 are integers and others are char.
#Convert the inconsistent ones from integer to char data type using 'mutate' and 'pipe' function

tripdata_202004 <- tripdata_202004 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202005 <- tripdata_202005 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202006 <- tripdata_202006 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202007 <- tripdata_202007 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202008 <- tripdata_202008 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202009 <- tripdata_202009 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202010 <- tripdata_202010 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202011 <- tripdata_202011 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
tripdata_202106 <- tripdata_202106 %>% mutate(start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))


#3rd step- PROCESS

#Combining all the datasets into one single dataframe using 'bind_rows' function

all_trips <- bind_rows(tripdata_202004,tripdata_202005,tripdata_202006,tripdata_202007,tripdata_202008,tripdata_202009,tripdata_202010,tripdata_202011,tripdata_202012,tripdata_202101,tripdata_202102,tripdata_202103,tripdata_202104,tripdata_202105,tripdata_202106,tripdata_202107)

str(all_trips)

##It can be observed that the 'started_at' and 'ended_at' is in char instead of daytime  datatype. Further cleaning is required.

#Chean up further
 #Converting from char to daytime datatype using 'ymd_hms' (Universal Coordinated Time Zone)

all_trips[['started_at']] <- ymd_hms(all_trips[['started_at']])
all_trips[['ended_at']] <- ymd_hms(all_trips[['ended_at']])

str(all_trips) 
##All looks Okay!

#Removing columns that are beyond the scope of the project/ Not required

all_trips <- all_trips %>% 
            select(-c(start_lat:end_lat))
glimpse(all_trips)

#Renaming columns for better understanding

all_trips <- all_trips %>%
          rename(ride_type = rideable_type,
                 start_time = started_at,
                 end_time = ended_at,
                 customer_type = member_casual)
glimpse(all_trips) ##Now its readable

----------------------------------

#Adding new columns that can be used for aggregrate functions

#Column for the day of the week the trip started

all_trips$day_of_the_week <- format(as.Date(all_trips$start_time), '%a')

#column for the month when the trip started

all_trips$month <- format(as.Date(all_trips$start_time), '%b_%y')

#column for time of the day when the trip started
#Time element needs to be extracted from start_time. However, as the times must be in POSIXct
#(only times of class POSIXct are supported in ggplot2), a two-step conversion is needed. 
#First the time is converted to a character vector, effectively stripping all the date information. 
#The time is then converted back to POSIXct with today’s date – the date is of no interest to us,

#only the hours-minutes-seconds are.

all_trips$time <- format(all_trips$start_time, format = "%H:%M")
all_trips$time <- as.POSIXct(all_trips$time, format = "%H:%M") 

#column for trip duration in minutes

all_trips$trip_duration <- (as.double(difftime(all_trips$end_time, all_trips$start_time)))/ 60

#checking the data frame

glimpse(all_trips)

#Let's check to see if the trip_duration column has any negative values, as this may cause problem while creating visualizations. Also, we do not want to include the trips that were part of quality tests by the company. These trips are usually identified by string 'test' in the start_station_name column.

#checking for the trip lengths less the 0

nrow(subset(all_trips, trip_duration < 0))

#checking for the test rides run by company for the quality checks

nrow(subset(all_trips, start_station_name %like% "TEST"))
nrow(subset(all_trips, start_station_name %like% "test"))
nrow(subset(all_trips, start_station_name %like% "test"))

#As there are 11041 rows with trip_duration less than 0 mins and 3515 trips that were test rides, we will remove these observations from our dataframe.
#We will create a new dataframe deviod of these obseravtions without making any changes to the existing dataframe.

#removing negative trip duration

all_trips_V2 <- all_trips[!(all_trips$trip_duration < 0),]

#removing test rides
all_trips_V2 <- all_trips[!((all_trips_V2$start_station_name %like% "TEST"| all_trips_V2$start_station_name %like% "test")),]

#checking the dataframe
glimpse(all_trips_V2)

#Now its important to make sure that customer_type column has only two distinct values. Let's confirm the same.

#checking the count of distinct values

table(all_trips_V2$customer_type)

#aggregating total trip duration by customer type
setNames(aggregate(trip_duration ~ customer_type, all_trips_V2, sum),c("customer_type", "total_trip_duration(mins)"))

##The dataframe is now ready for descriptive analysis that will help us uncover some insights on how the casual riders and members use Cyclistic rideshare differently.
-----------------------------------------------------

#4th Phase: ANALYZE

#Firstly, let's try to get some simple statistics on trip_duration for all customers, and do the same by customer_type.
  
#statistical summary of trip_duration for all trips
  
summary(all_trips_V2$trip_duration)

#statistical summary of trip_duration by customer_type

all_trips_V2 %>%
  group_by(customer_type) %>%
  summarise(min_trip_duration = min(trip_duration), max_trip_duration = max(trip_duration),
            median_trip_duration = median(trip_duration), mean_trip_duration = mean(trip_duration))

#Total number of trips by customer type and day of the week

# fixing the order for the day_of_the_week and month variable so that they show up 
# in the same sequence in output tables and visualizations
all_trips_v2$day_of_the_week <- ordered(all_trips_v2$day_of_the_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
all_trips_v2$month <- ordered(all_trips_v2$month, levels=c("Apr_20", "May_20", "Jun_20", "Jul_20", "Aug_20", "Sep_20", "Oct_20",
                                                           "Nov_20", "Dec_20", "Jan_21", "Feb_21", "Mar_21", 
                                                           "Apr_21", "May_21", "Jun_21", "Jul_21"))
all_trips_v2 %>% 
  group_by(customer_type, day_of_the_week) %>%  
  summarise(number_of_rides = n(),average_duration_mins = mean(trip_duration)) %>% 
  arrange(customer_type, desc(number_of_rides))


#summarise()` has grouped output by 'customer_type'. You can override using the `.groups` argument.

----------------------------------------
  
#Visualization

all_trips_v2 %>%  
group_by(customer_type, day_of_the_week) %>% 
summarise(number_of_rides = n()) %>% 
arrange(customer_type, day_of_the_week)  %>% 
ggplot(aes(x = day_of_the_week, y = number_of_rides, fill = customer_type)) +
labs(title ="Total trips by customer type Vs. Day of the week") +
geom_col(width=0.5, position = position_dodge(width=0.5)) +
scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#From the table and graph above, casual customers are most busy on Sundays followed by Saturdays, while members are most busy on later half of the week extending into the weekend. Interesting pattern to note though is the consistent trip numbers among members with less spread over entire week as compared to casual riders who don't seem to use the bikeshare services much during weekdays.


-------------------------------------------------------------------

#Average number of trips by customer type and month

unique(all_trips$month)

all_trips_v2 %>% 
  group_by(customer_type, month) %>%  
  summarise(number_of_rides = n(),`average_duration_(mins)` = mean(trip_duration)) %>% 
  arrange(customer_type,desc(number_of_rides))

-----------------------------------------------------------------

all_trips_v2 %>%  
group_by(customer_type, month) %>% 
summarise(number_of_rides = n()) %>% 
arrange(customer_type, month)  %>% 
ggplot(aes(x = month, y = number_of_rides, fill = customer_type)) +
labs(title ="Total trips by customer type Vs. Month") +
theme(axis.text.x = element_text(angle = 30)) +
geom_col(width=0.5, position = position_dodge(width=0.5)) +
scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#`summarise()` has grouped output by 'customer_type'. You can override using the `.groups` argument.

#The data shows that the months of July, August and September are the most busy time of the year among both members and casual riders. This could be attributed to an external factor (eg. cold weather, major quality issue) that might have hindered with customer needs. 2021 is a tough year when Covid comes. People care more about their health. The charts shows that the no.of rides in 2021 is higher than 2020 in general. However, the number of trips made by members is always higher than the casual riders across all months of the year.


-------------------------------------------------------------
#Visualization of average trip duration by customer type on each day of the week
  
all_trips_v2 %>%  
  group_by(customer_type, day_of_the_week) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = day_of_the_week, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Day of the week")
#The average trip duration of a casual rider is more than twice that of a member. Note that this necessarily does not mean that casual riders travel farther distance. It is also interesting to note that weekends not only contribute to more number of trips but also longer trips on average when compared to weekdays.

---------------------------------------------------------------------------
#Visualisaton of average trip duration by customer type Vs. month

all_trips_v2 %>%  
  group_by(customer_type, month) %>% 
  summarise(average_trip_duration = mean(trip_duration)) %>%
  ggplot(aes(x = month, y = average_trip_duration, fill = customer_type)) +
  geom_col(width=0.5, position = position_dodge(width=0.5)) + 
  labs(title ="Average trip duration by customer type Vs. Month") +
  theme(axis.text.x = element_text(angle = 30))  
#Average trip duration of member riders is anywhere between 10-20 minutes throughout the year, exception being April when it goes slightly over 20 minutes. However, there seems to be a distinct pattern when it comes to casual riders, whose average trip duration swings wildly from as low as ~25 minutes to more than an hour depending on time of the year. It is worth noting unusually long trip durations by casual riders in the month of April.

------------------------------------------------------------------------
#Visualizaton of bike demand over 24 hr period (a day)
  
  
all_trips_v2 %>%  
  group_by(customer_type, time) %>% 
  summarise(number_of_trips = n()) %>%
  ggplot(aes(x = time, y = number_of_trips, color = customer_type, group = customer_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 hour", minor_breaks = NULL,
                   date_labels = "%H:%M", expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title ="Demand over 24 hours of a day", x = "Time of the day")

#summarise()` has grouped output by 'customer_type'. You can override using the `.groups` argument.

#For the members, there seems to be two distict peak demand hours: 7-9 AM and 5-7 PM, the latter one coinciding with the peak demand hours of casual riders as well. One could probably hypothesize that office-goers make up majority of the members profile due to demand in both morning and evening hours, but we need more data to substabtiate this assumption.

---------------------------------
#Visualizaton of ride type Vs. number of trips by customer type
all_trips_v2 %>%
  group_by(ride_type, customer_type) %>%
  summarise(number_of_trips = n()) %>%  
  ggplot(aes(x= ride_type, y=number_of_trips, fill= customer_type))+
  geom_bar(stat='identity') +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  labs(title ="Ride type Vs. Number of trips")

#Classic bikes are predominantly used by members. Docked bikes are in most demand and equally used by both members as well as casual riders. Electric bikes are more favored by members. If electric bikes costs the highest among all 3 types, it would be a financially sound move to increase their fleet while reducing docked bikes, as they are already preferred by members who make up for the majority of the trips.

------------------------------------------------------------------------------
  
#Creating a csv file of the clean data for futher analysis or visualizations in other tools like SQL, Tableau, Power BI, etc.

clean_data <- aggregate(all_trips_V2$trip_duration ~ all_trips_V2$customer_type + all_trips_V2$day_of_the_week, FUN = mean)

write.csv(clean_data, "Clean_Data.csv", row.names = F)

--------------------------------------------------------------------------------------
  
  #6th Phase = ACT
  
#Key Takeaways
#Casual riders made 41% of total trips contributing to 66% of total trip duration between Apr'20 - Mar'21. Member riders make up 59% of total trips contributing to 34% of total trip duration between Apr'20 - Mar'21

#Usage (based on trip duration) of bikes by casual riders is almost twice that of member riders.

#Casual customers use bikeshare services more during weekends, while members use them consistently over the entire week.

#Average trip duration of casual riders is more than twice that of member rider over any given day of the week cumulatively.

#Casual riders ride longer during first half of the year compared to the second half, while members clock relatively similar average trip duration month over month.

#Recommendations
#Provide attractive promotions for casual riders on weekdays so that casual members use the bikeshare services ore uniformly across the entire week.

#Offer discounted membership fee for renewals after the first year. It might nudge casual riders to take up membership.

#Offer discounted pricing during non-busy hours so that casual riders might choose to use bikes more often and level out demand over the day.

#Casual riders prefer docked bikes the most while classic bikes are popular among members.

