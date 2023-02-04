library(nycflights13)
library(tidyverse)
library(lubridate)

# case study
## how does weather affect flight delays?

weather_data <- data.table::copy(weather)

# create date variable
weather_data <- weather_data %>%
  mutate(
    date = make_date(year,month, day)
  ) %>%
  select(-c(2:5)) %>%
  relocate(date, .before = origin) 

# joining flight data with weather data
flights_weather_data = weather_data %>%
  inner_join(flights_data)

# selecting columns of interest
flights_weather_data_update = flights_weather_data %>%
  select(-c(wind_gust,time_hour,sched_arr_time, sched_dep_time,arr_time, 
            dep_time,distance))

# missing data
(colSums(is.na(flights_weather_data_update))/nrow(flights_weather_data_update))*100

# drop missing data
flights_weather_data_clean = flights_weather_data_update %>%
  drop_na()

# plot 1
# how humidity affects departure
flights_weather_data_clean %>%
  sample_n(1000) %>%
  ggplot(aes(x=humid)) +
  geom_smooth(aes(y=dep_delay)) +
  theme_bw()

# how pressure affects departure
flights_weather_data_clean %>%
  sample_n(10000) %>%
  ggplot(aes(x=pressure)) +
  geom_smooth(aes(y=dep_delay)) +
  ggtitle("EAFFECTS OF PRESSURE ON DEP DELAY") +
  theme_bw()

# how temp affects departure
flights_weather_data_clean %>%
  sample_n(10000) %>%
  ggplot(aes(x=temp)) +
  geom_smooth(aes(y=dep_delay)) +
  ggtitle("EAFFECTS OF TEMPERATURE ON DEP DELAY") +
  theme_bw()

# how dewp affects departure
flights_weather_data_clean %>%
  sample_n(10000) %>%
  ggplot(aes(x=dewp)) +
  geom_smooth(aes(y=dep_delay),se=FALSE) +
  ggtitle("EAFFECTS OF DEW ON DEP DELAY") +
  theme_bw()

# how dewp affects departure
flights_weather_data_clean %>%
  sample_n(10000) %>%
  ggplot(aes(x=dewp)) +
  geom_smooth(aes(y=arr_delay),se=FALSE) +
  ggtitle("EAFFECTS OF DEW ON ARR DELAY") +
  theme_bw()
