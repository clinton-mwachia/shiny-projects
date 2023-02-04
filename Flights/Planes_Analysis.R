library(nycflights13)
library(tidyverse)
library(lubridate)
library(reshape2)
library(plotly)

# case study
## how does the plane model, manufacturer, engine,no. of seats affect delay? 

planes_data <- data.table::copy(planes)

flights_plane_data <- flights_data %>% inner_join(planes_data, "tailnum")

# select columns of interest
flights_plane_data_update <- flights_plane_data %>%
  select(
    date, dep_delay, arr_delay, tailnum, year, manufacturer,
    model, engines, engine, seats
  )

# missing data
colSums(is.na(flights_plane_data_update))

# dropping column year
flights_plane_data_clean <- flights_plane_data_update %>%
  select(-year)

colSums(is.na(flights_plane_data_clean))

# which manufacturer has the highest number of delays
flight_plane_summary <- flights_plane_data_clean %>%
  group_by(manufacturer) %>%
  summarize(
    tot_dep_delay = sum(dep_delay),
    tot_arr_delay = sum(arr_delay)
  )

flight_plane_summary %>%
  group_by(manufacturer) %>%
  arrange(desc(tot_dep_delay))

# filter by manufacturer
p <- flight_plane_summary %>%
  filter(manufacturer == "AGUSTA SPA") %>%
  melt(id="manufacturer", variable.name = "Delay",value.name = "delay") %>%
  ggplot(aes(y=delay, x=manufacturer, fill=Delay)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("dep/arrival delay by manufacturer") +
  theme_bw()

ggplotly(p)

# which plane model has the highest number of delays
flight_plane_summary_model <- flights_plane_data_clean %>%
  group_by(model) %>%
  summarize(
    tot_dep_delay = sum(dep_delay),
    tot_arr_delay = sum(arr_delay)
  )

# filter by manufacturer
p2 <- flight_plane_summary_model %>%
  filter(model == "150") %>%
  melt(id="model", variable.name = "Delay",value.name = "delay") %>%
  ggplot(aes(y=delay, x=model, fill=Delay)) + 
  geom_bar(position="dodge", stat="identity") +
  ggtitle("dep/arrival delay by model") +
  theme_bw()

ggplotly(p2)
