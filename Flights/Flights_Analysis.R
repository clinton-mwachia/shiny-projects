library(nycflights13)
library(tidyverse)
library(lubridate)
library(plotly)

# case study
# 1. view the whole data
#  see flights with the highest number of arrival/departure delays
#  see flights by date, departure/arrival time, origin/destination
# 2. time series of departure delay across time
# 3. time series of arrival delay across time

# CREATE A COPY OF THE ORIGINAL DATA
flights_data <- data.table::copy(flights)

# combine year month and day into column date and dropping the columns
flights_data <- flights_data %>%
  mutate(
    date = make_date(year,month, day)
  ) %>%
  select(-c(1:3,17:18)) %>%
  relocate(date, .before = dep_time) %>% # move date to the beginning
  drop_na()

# 1. view the whole data
# using data table
# don't show here, only demonstrate in shiny
flights_data

## arrival and departure delay by flight 
p = flights_data %>%
  filter(tailnum == "N14228") %>%
  select(date, dep_delay, arr_delay, tailnum) %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=dep_delay,colour="dep")) +
  geom_line(aes(y=arr_delay,color="arr")) +
  scale_color_manual(
    name="Delays", values = c("dep" = "blue", "arr" = "red")) +
  ylab("Delay") +
  ggtitle("Arrival/Departure delay Per Plane") +
  theme_bw() 

ggplotly(p)


