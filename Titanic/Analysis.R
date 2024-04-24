# set working directory to current folder
setwd("F:/R/shiny-projects/Titanic")

# loading libraries
library(tidyverse)

# load the data
raw_titanic = read_csv("data/titanic.csv")

# data cleaning
# selecting only required columns
titanic = raw_titanic |>
  select(-starts_with("zero")) |> # dropping columns starting with zero*
  rename(survived = `2urvived`) |> # rename 2urvived to survived
  mutate(
    Age = as.integer(Age) # convert age from float to integer
  ) |> 
  select(-Passengerid) # drop passenderId
  
  
