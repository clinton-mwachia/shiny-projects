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
  rename(survived = `2urvived`) |> # rename '2urvived' to survived
  mutate(
    Age = as.integer(Age) # convert age from float to integer
  ) |> 
  select(-Passengerid) # drop passenger Id
  
# missing data
colSums(is.na(titanic))

# drop missing rows
titanic_clean = titanic |>
  drop_na()

# factor analysis
titanic = titanic_clean |>
  mutate(
    Sex = as_factor(
      ifelse(Sex == 0, "male","female")
    ),
    sibsp = as.factor(sibsp),
    Parch = as.factor(Parch),
    Pclass = as.factor(
      ifelse(
        Pclass == 1, "upper", ifelse(Pclass == 2, "middle","lower"))
    ),
    Embarked = as.factor(
        ifelse(Embarked == 0, "Cherbourg", 
               ifelse(Embarked == 1, "Queenstown","Southampton"))),
    survived = as.factor(ifelse(survived == 0, "no","yes"))
  )
