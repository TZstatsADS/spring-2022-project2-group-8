library(dplyr)
library(lubridate)

# Importing 2019 data ...

taxi_2019_10 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2019/yellow_tripdata_2019-10.csv")
taxi_2019_11 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2019/yellow_tripdata_2019-11.csv")
taxi_2019_12 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2019/yellow_tripdata_2019-12.csv")

# Merge 2019 data ...
taxi_2019 <- do.call("rbind", list(taxi_2019_10, taxi_2019_11, taxi_2019_12))
rm("taxi_2019_10")
rm("taxi_2019_11")
rm("taxi_2019_12")

# Importing 2020 data ...
taxi_2020_01 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-01.csv")
taxi_2020_02 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-02.csv")
taxi_2020_03 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-03.csv")
taxi_2020_04 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-04.csv")
taxi_2020_05 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-05.csv")
taxi_2020_06 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-06.csv")
taxi_2020_07 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-07.csv")
taxi_2020_08 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-08.csv")
taxi_2020_09 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-09.csv")
taxi_2020_10 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-10.csv")
taxi_2020_11 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-11.csv")
taxi_2020_12 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2020/yellow_tripdata_2020-12.csv")

# Merge 2020 data ...
taxi_2020 <- do.call("rbind", list(taxi_2020_01, taxi_2020_02, taxi_2020_03, taxi_2020_04, taxi_2020_05, taxi_2020_06,taxi_2020_07,taxi_2020_08,taxi_2020_09,taxi_2020_10,taxi_2020_11,taxi_2020_12))
rm("taxi_2020_01")
rm("taxi_2020_02")
rm("taxi_2020_03")
rm("taxi_2020_04")
rm("taxi_2020_05")
rm("taxi_2020_06")
rm("taxi_2020_07")
rm("taxi_2020_08")
rm("taxi_2020_09")
rm("taxi_2020_10")
rm("taxi_2020_11")
rm("taxi_2020_12")

# Importing 2021 (Jan - Jul) data ... 
taxi_2021_01 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-01.csv")
taxi_2021_02 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-02.csv")
taxi_2021_03 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-03.csv")
taxi_2021_04 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-04.csv")
taxi_2021_05 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-05.csv")
taxi_2021_06 <- read.csv("/Users/marvinlimpijankit/Desktop/SHINY/2021/yellow_tripdata_2021-06.csv")

# Merge 2021 data ...
taxi_2021 <- do.call("rbind", list(taxi_2021_01, taxi_2021_02, taxi_2021_03, taxi_2021_04, taxi_2021_05, taxi_2021_06))
rm("taxi_2021_01")
rm("taxi_2021_02")
rm("taxi_2021_03")
rm("taxi_2021_04")
rm("taxi_2021_05")
rm("taxi_2021_06")

# Import Zones Data
zones = read.csv("/Users/marvinlimpijankit/Desktop/SHINY/taxi_zone_lookup.csv")
zones <- zones[c("LocationID", "Borough")]

# Filter only the columns that might be interesting to us
taxi_2019 <- taxi_2019[c("PULocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")]
taxi_2020 <- taxi_2020[c("PULocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")]
taxi_2021 <- taxi_2021[c("PULocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")]

# Change the name of PULocationID to LocationID
colnames(taxi_2019) <- c("LocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")
colnames(taxi_2020) <- c("LocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")
colnames(taxi_2021) <- c("LocationID", "tpep_pickup_datetime", "tpep_dropoff_datetime", "passenger_count","trip_distance", "total_amount")

# Get the duration of the trip (in hours for mph later)
taxi_2019 <- taxi_2019 %>%
  mutate(trip_time = difftime(as.POSIXlt(tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S"), units = "hours"))
taxi_2020 <- taxi_2020 %>%
  mutate(trip_time = difftime(as.POSIXlt(tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S"), units = "hours"))
taxi_2021 <- taxi_2021 %>%
  mutate(trip_time = difftime(as.POSIXlt(tpep_dropoff_datetime, format = "%Y-%m-%d %H:%M:%S"),as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d %H:%M:%S"), units = "hours"))

# Get the distance/time = speed
taxi_2019 <- taxi_2019 %>%
  mutate(avg_speed = trip_distance/(as.numeric(trip_time)))
taxi_2020 <- taxi_2020 %>%
  mutate(avg_speed = trip_distance/(as.numeric(trip_time)))
taxi_2021 <- taxi_2021 %>%
  mutate(avg_speed = trip_distance/(as.numeric(trip_time)))

# Get the year and months
taxi_2019 <- taxi_2019 %>% 
  mutate(pickup_year = year(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")), pickup_month = month(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")))
taxi_2020 <- taxi_2020 %>% 
  mutate(pickup_year = year(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")), pickup_month = month(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")))
taxi_2021 <- taxi_2021 %>% 
  mutate(pickup_year = year(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")), pickup_month = month(as.POSIXlt(tpep_pickup_datetime, format = "%Y-%m-%d")))

# Remove all rows with na
taxi_2019 <- taxi_2019[complete.cases(taxi_2019), ]
taxi_2020 <- taxi_2020[complete.cases(taxi_2020), ]
taxi_2021 <- taxi_2021[complete.cases(taxi_2021), ]
taxi_2019 <- do.call(data.frame,lapply
                     (taxi_2019, function(value) replace
                       (value, is.infinite(value), 0)))
taxi_2020 <- do.call(data.frame,lapply
                     (taxi_2020, function(value) replace
                       (value, is.infinite(value), 0)))
taxi_2021 <- do.call(data.frame,lapply
                     (taxi_2021, function(value) replace
                       (value, is.infinite(value), 0)))

# remove all rides with 0 passengers
taxi_2019 <- taxi_2019[!(taxi_2019$passenger_count == 0),]
taxi_2020 <- taxi_2020[!(taxi_2020$passenger_count == 0),]
taxi_2021 <- taxi_2021[!(taxi_2021$passenger_count == 0),]

# Group by and average

taxi_2019_mean <- aggregate(list(taxi_2019$passenger_count, taxi_2019$trip_distance, taxi_2019$total_amount, taxi_2019$trip_time, taxi_2019$avg_speed), by = list(taxi_2019$pickup_year, taxi_2019$pickup_month, taxi_2019$LocationID), mean)
taxi_2019_count <- taxi_2019 %>% count(pickup_year, pickup_month, LocationID)
colnames(taxi_2019_mean) <- c("Year", "Month", "LocationID", "passenger_count", "trip_distance", "total_amount", "trip_time", "avg_speed")
colnames(taxi_2019_count) <- c("Year", "Month", "LocationID", "Count")
taxi_2019_mean <- filter(taxi_2019_mean, Year == 2019)
taxi_2019_mean <- filter(taxi_2019_mean, Month > 9)
taxi_2019_count <- filter(taxi_2019_count, Year == 2019)
taxi_2019_count <- filter(taxi_2019_count, Month > 9)

taxi_2020_mean <- aggregate(list(taxi_2020$passenger_count, taxi_2020$trip_distance, taxi_2020$total_amount, taxi_2020$trip_time, taxi_2020$avg_speed), by = list(taxi_2020$pickup_year, taxi_2020$pickup_month, taxi_2020$LocationID), mean)
taxi_2020_count <- taxi_2020 %>% count(pickup_year, pickup_month, LocationID)
colnames(taxi_2020_mean) <- c("Year", "Month", "LocationID", "passenger_count", "trip_distance", "total_amount", "trip_time", "avg_speed")
colnames(taxi_2020_count) <- c("Year", "Month", "LocationID", "Count")
taxi_2020_mean <- filter(taxi_2020_mean, Year == 2020)
taxi_2020_count <- filter(taxi_2020_count, Year == 2020)

taxi_2021_mean <- aggregate(list(taxi_2021$passenger_count, taxi_2021$trip_distance, taxi_2021$total_amount, taxi_2021$trip_time, taxi_2021$avg_speed), by = list(taxi_2021$pickup_year, taxi_2021$pickup_month, taxi_2021$LocationID), mean)
taxi_2021_count <- taxi_2021 %>% count(pickup_year, pickup_month, LocationID)
colnames(taxi_2021_mean) <- c("Year", "Month", "LocationID", "passenger_count", "trip_distance", "total_amount", "trip_time", "avg_speed")
colnames(taxi_2021_count) <- c("Year", "Month", "LocationID", "Count")
taxi_2021_mean <- filter(taxi_2021_mean, Year == 2021)
taxi_2021_mean <- filter(taxi_2021_mean, Month < 7)
taxi_2021_count <- filter(taxi_2021_count, Year == 2021)
taxi_2021_count <- filter(taxi_2021_count, Month < 7)

final_2019 = merge(taxi_2019_mean, taxi_2019_count)
final_2020 = merge(taxi_2020_mean, taxi_2020_count)
final_2021 = merge(taxi_2021_mean, taxi_2021_count)

total_2019 = merge(final_2019, zones)
total_2020 = merge(final_2020, zones)
total_2021 = merge(final_2021, zones)

# Group by Year, Month, Borough

summary_2019 <- total_2019 %>%
  group_by(Year, Month, Borough) %>% 
  summarize(weighted_passenger_count = weighted.mean(passenger_count, Count), weighted_trip_distance = weighted.mean(trip_distance, Count), weighted_total_amount = weighted.mean(total_amount, Count), weighted_trip_time = weighted.mean(trip_time, Count), weighted_avg_speed = weighted.mean(avg_speed, Count), Count = sum(Count))

summary_2020 <- total_2020 %>%
  group_by(Year, Month, Borough) %>% 
  summarize(weighted_passenger_count = weighted.mean(passenger_count, Count), weighted_trip_distance = weighted.mean(trip_distance, Count), weighted_total_amount = weighted.mean(total_amount, Count), weighted_trip_time = weighted.mean(trip_time, Count), weighted_avg_speed = weighted.mean(avg_speed, Count), Count = sum(Count))

summary_2021 <- total_2021 %>%
  group_by(Year, Month, Borough) %>% 
  summarize(weighted_passenger_count = weighted.mean(passenger_count, Count), weighted_trip_distance = weighted.mean(trip_distance, Count), weighted_total_amount = weighted.mean(total_amount, Count), weighted_trip_time = weighted.mean(trip_time, Count), weighted_avg_speed = weighted.mean(avg_speed, Count), Count = sum(Count))

# Merge all the Summaries Together

summary <- rbind(summary_2019, summary_2020)
summary <- rbind(summary, summary_2021)

# Remove EWR Borough, and Those with Count < 100 (not enough data points, not stable)

summary_filtered <- summary[!(summary$Count < 100),]
summary_filtered <- summary_filtered_count[!(summary_filtered_count$Borough == "EWR"),]

# EXPORT FINAL TABLE TO CSV
write.csv(summary_filtered, "../data/Taxi/cleaned/taxi_data_by_month_boro.csv")
