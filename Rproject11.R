#cleaning data in R

library(tidyverse)
library(janitor)
library(lubridate)

getwd()

#upload the data
df1 <- read_csv("202004-divvy-tripdata.csv")
df2 <- read_csv("202005-divvy-tripdata.csv")
df3 <- read_csv("202006-divvy-tripdata.csv")
df4 <- read_csv("202007-divvy-tripdata.csv")
df5 <- read_csv("202008-divvy-tripdata.csv")
df6 <- read_csv("202009-divvy-tripdata.csv")
df7 <- read_csv("202010-divvy-tripdata.csv")
df8 <- read_csv("202011-divvy-tripdata.csv")
df9 <- read_csv("202012-divvy-tripdata.csv")
df10 <- read_csv("202101-divvy-tripdata.csv")
df11 <- read_csv("202102-divvy-tripdata.csv")
df12 <- read_csv("202103-divvy-tripdata.csv")

#combine data frames into one data frame 

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))

##Convert Data/Time stamp to Date/Time ...
##
bike_rides$Ymd <- as.Date(bike_rides$started_at)
bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)  
bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)



bike_rides$Hours <- 
  difftime(bike_rides$ended_at,bike_rides$started_at,
                                units = c("hours"))

bike_rides$Minutes <- 
  difftime(bike_rides$ended_at,bike_rides$started_at,
           units = c("mins"))

df <- bike_rides %>% filter(Minutes >0) %>% drop_na() 
# select(-ride_id,-end_station_name,-end_station_id,-end_station_name)

## Data Wrangling Change Log 
May 5, 2022

Total rows: 3,489,748
I raised three concerns about this data with Lily Moreno
(The director off marketing) 
1: 10,552 rows with negative trip durations 
2: 122,174 rows with missing starting station names (and ID)

I was advised by Lily to ignore the rows with trip duration <=0
We could also ignore with missing start station ID

##Create a summary data frame 
bikerides2 <- bike_rides %>% group_by(weekly =
floor_date(Ymd,"week"),start_hour) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
    ) %>% ungroup()

##Summary of hourly Counts 
summary(bikerides2$Count)

#Table of counts by Hour
xtabs(bikerides2$Count~bikerides2$start_hour)


bikerides2$Monthly <- lubridate::month(bikerides2$Weekly)

library(scales)

bikerides2 %>% ggplot() + geom_col(aes(x=weekly,y=Count)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides per Day",
       subtitle = "(Bases on 28 day moving average",
       y="Average rides per day")

bikerides2 %>% ggplot() + geom_col(aes(x=start_hour,y=Count)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Count of Rides by Hours",
       y="Rides per Hour")

##Count of rides by bike type 
###Summary of Bike Types 

bikestype <- bike_rides %>% group_by(rideable_type, weekly =
                                        floor_date(Ymd,"week")) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

ggplot(bikestype) +
  geom_area(aes(x=weekly,y=Count,fill=rideable_type)) +
  scale_y_continuous(labels=comma) +
  labs(title="Count of Rides by Bike Type")

bikestype <- bike_rides %>% group_by(member_casual,rideable_type,weekly =
                                       floor_date(Ymd,"week")) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    Median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()

ggplot(bikestype) +
  geom_col(aes(x=weekly,y=Count,fill=member_casual)) +
  scale_y_continuous(labels = comma) +
  labs(title="Count of Rides by Rider Type")

bike_rides %>% count(start_station_name,sort = TRUE)

bike_rides %>% count(start_station_name,sort = TRUE) %>%
  top_n(20) %>% ggplot() +
  geom_col(aes(x=reorder(start_station_name,n),y=n)) +
  coord_flip() + labs(title = "Top 20 Start Stations by Ride Count",
                      y = "Station Name",x="Count of Rides")





