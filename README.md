# Cyclistic Marketing Analysis Report

## Business Task
The goal of this analysis is to understand how annual members and casual riders utilize Cyclistic bikes differently. This insight will inform the design of a new marketing strategy aimed at converting casual riders into annual members, ultimately maximizing the number of annual memberships.

## Data Sources
The data used for this analysis includes historical bike trip data from Cyclistic, spanning multiple months. This dataset contains information regarding:
- trip durations,
- rider types (annual members vs. casual riders),
- ride start times,
- ride location,
- bicycle type,
- and more.

## Data Cleaning and Manipulation
I started by first downloading applicable packages to aid in the data cleaning and analysis process. I then combined data from multiple CSV files representing different months into a single dataframe. Afterward, I cleaned the data by removing any rows with missing values and converted columns to appropriate data types. Additionally, I extracted relevant features such as hour of the day from the trip start times to facilitate analysis.

```r
library(tidyverse) # metapackage of all tidyverse packages
# Load all the data
divvy_tripdata_2020_04 <- read.csv("/kaggle/input/cyclistic/202004-divvy-tripdata/202004-divvy-tripdata.csv")
divvy_tripdata_2020_05 <- read.csv("/kaggle/input/cyclistic/202005-divvy-tripdata/202005-divvy-tripdata.csv")
divvy_tripdata_2020_06 <- read.csv("/kaggle/input/cyclistic/202006-divvy-tripdata/202006-divvy-tripdata.csv")
divvy_tripdata_2020_07 <- read.csv("/kaggle/input/cyclistic/202007-divvy-tripdata/202007-divvy-tripdata.csv")
divvy_tripdata_2020_08 <- read.csv("/kaggle/input/cyclistic/202008-divvy-tripdata/202008-divvy-tripdata.csv")
divvy_tripdata_2020_09 <- read.csv("/kaggle/input/cyclistic/202009-divvy-tripdata/202009-divvy-tripdata.csv")
divvy_tripdata_2020_10 <- read.csv("/kaggle/input/cyclistic/202010-divvy-tripdata/202010-divvy-tripdata.csv")
divvy_tripdata_2020_11 <- read.csv("/kaggle/input/cyclistic/202011-divvy-tripdata/202011-divvy-tripdata.csv")
divvy_tripdata_2020_12 <- read.csv("/kaggle/input/cyclistic/202012-divvy-tripdata/202012-divvy-tripdata.csv")
divvy_tripdata_2021_01 <- read.csv("/kaggle/input/cyclistic/202101-divvy-tripdata/202101-divvy-tripdata.csv")
divvy_tripdata_2021_02 <- read.csv("/kaggle/input/cyclistic/202102-divvy-tripdata/202102-divvy-tripdata.csv")
divvy_tripdata_2021_03 <- read.csv("/kaggle/input/cyclistic/202103-divvy-tripdata/202103-divvy-tripdata.csv")

# Combine and change 2020-04 through 2020-11 col name start_station_id & end_station_id data type to match 2020-12 through 2021-03.
trip_data_DBL <- rbind(divvy_tripdata_2020_04, divvy_tripdata_2020_05, divvy_tripdata_2020_06, divvy_tripdata_2020_07, divvy_tripdata_2020_08, divvy_tripdata_2020_09, divvy_tripdata_2020_10, divvy_tripdata_2020_11)

trip_data_DBL <- mutate(trip_data_DBL, start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))

trip_data_CHAR <- rbind(divvy_tripdata_2020_12, divvy_tripdata_2021_01, divvy_tripdata_2021_02, divvy_tripdata_2021_03)

# Combine all dataframes
all_trip_data <- rbind(trip_data_DBL, trip_data_CHAR)

# Overview of the Data
print("#####DATA SUMMARY#####")
summary(all_trip_data)
print("#####TABLE PREVIEW#####")
tibble(all_trip_data)

# Let's start cleaning the data
# First, drop all NA
all_trip_data_cln <- drop_na(all_trip_data)

# Make some useful columns for future data analysis
all_trip_data_cln$date <- as.Date(all_trip_data_cln$started_at)
all_trip_data_cln$month <- format(as.Date(all_trip_data_cln$date), "%m")
all_trip_data_cln$day <- format(as.Date(all_trip_data_cln$date), "%d")
all_trip_data_cln$year <- format(as.Date(all_trip_data_cln$date), "%Y")
all_trip_data_cln$day_of_the_week <- format(as.Date(all_trip_data_cln$date), "%A")
all_trip_data_cln$started_at <- as.POSIXct(all_trip_data_cln$started_at)
all_trip_data_cln$hour_of_day <- as.numeric(format(all_trip_data_cln$started_at, "%H"))

# Make a column to compute ride length in minutes
all_trip_data_cln$ride_length_min <- difftime(all_trip_data_cln$ended_at, all_trip_data_cln$started_at) / 60

# Quality check the data to exclude negative ride times
all_trip_data_cln <- all_trip_data_cln[all_trip_data_cln$ride_length_min > 0, ]

```

# Summary of Analysis
- Mean Ride Length: Casual riders tend to have longer ride durations compared to annual members.
- Ride Counts by Day of the Week: Casual riders exhibit higher ride counts on weekends, while annual members show more consistent usage across weekdays.
- Ride Counts per Month: Both casual riders and annual members show increased usage during warmer months, with ride counts peaking in the summer.
- Ride Counts per Hour of the Day: Annual members have higher rider counts than casual riders, with usage peaking during the commuting hours of the day, while casual riders peak during the afternoon.

```r
# Calculation 1: Mean ride_length by member_casual
mean_ride_length <- all_trip_data_cln %>%
  group_by(member_casual) %>%
  summarise(mean_ride_length = mean(ride_length_min, na.rm = TRUE))

# Calculation 2: Ride counts for day_of_the_week by member_casual
ride_counts_day_of_week <- all_trip_data_cln %>%
  group_by(day_of_the_week, member_casual) %>%
  summarise(ride_count = n())

# Calculation 3: Ride counts per hour of the day grouped by rider status
ride_counts_per_hour <- all_trip_data_cln %>%
  group_by(hour_of_day, member_casual) %>%
  summarise(ride_count = n())

# Calculation 4: Ride counts per month by member_casual
ride_counts_per_month <- all_trip_data_cln %>%
  group_by(year, month, member_casual) %>%
  summarise(ride_count = n())
```
## Supporting Visualizations and Key Findings
- Casual riders use the bikes for much longer trips.
- Casual riders prefer weekends for bike rides, whereas annual members show more consistent usage patterns throughout the week.
- Both rider types show increased bike usage during warmer months.
- Casual riders tend to use bikes more during daytime hours, while annual members peak during commute hours.

```r
# Visualization 1: Compare mean ride_length by member_casual
visualization1 <- ggplot(mean_ride_length, aes(x = member_casual, y = mean_ride_length, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Ride Length by Member Type",
       x = "Member Type",
       y = "Mean Ride Length (minutes)",
       fill = "Member Type") +
  theme_minimal()

visualization1

# Visualization 2: Compare ride counts for day_of_the_week by member_casual
visualization2 <- ggplot(ride_counts_day_of_week, aes(x = day_of_the_week, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ride Counts by Day of the Week and Member Type",
       x = "Day of the Week",
       y = "Ride Count",
       fill = "Member Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

visualization2

# Visualization 3: Compare ride counts for hour of the day by member_casual
visualization3 <- ggplot(ride_counts_per_hour, aes(x = hour_of_day, y = ride_count, color = member_casual, group = member_casual)) +
  geom_line() +
  labs(title = "Ride Counts per Hour of the Day by Rider Status",
       x = "Hour of the Day",
       y = "Ride Count",
       color = "Rider Status") +
  theme_minimal()

visualization3

# Visualization 4: Compare ride counts per month by member_casual
visualization4 <- ggplot(ride_counts_per_month, aes(x = month, y = ride_count, fill = member_casual)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ride Counts per Month by Member Type",
       x = "Month",
       y = "Ride Count",
       fill = "Member Type") +
  theme_minimal()

visualization4
```
# Recommendations
Based on our analysis, here are the top three recommendations for Cyclistic's marketing strategy:

- Target Weekend Promotions: Offer special discounts or incentives for casual riders on weekends to encourage them to become annual members.
- Seasonal Membership Deals: Introduce seasonal membership deals or promotions during peak riding months to attract more casual riders to become annual members.
- Digital Media Campaigns: Launch targeted digital media campaigns focusing on the benefits of annual memberships, highlighting cost savings and convenience compared to single-ride or full-day passes.
