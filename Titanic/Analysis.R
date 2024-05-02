# set working directory to current folder
setwd("F:/R/shiny-projects/Titanic")

# loading libraries
library(tidyverse)
library(viridis)

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

################## exploratory data analysis ####################################

# the number survivors
# there were 340 survivors and 967 passenger perished
titanic |> 
  group_by(survived) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=survived, x=survived, y=total)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Survived") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passenger Survival Rates",
       subtitle = "Titanic voyage: 1912") +
  theme_bw()

# number of passengers by gender
# male: 843, female: 464
titanic |> 
  group_by(Sex) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=Sex, x=Sex, y=total)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Sex") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passengers by Sex",
       subtitle = "Titanic voyage: 1912") +
  theme_bw()

# number of passengers from there point of embarked
# Cherboug: 270, Queenstown: 123, Southampton: 914
titanic |> 
  group_by(Embarked) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=Embarked, x=Embarked, y=total)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Embarked") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passengers by point of Embarked",
       subtitle = "Titanic voyage: 1912") +
  theme_bw()

# number of passengers by class
# lower: 709, middle: 277, upper: 321
titanic |> 
  group_by(Pclass) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=Pclass, x=Pclass, y=total)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Pclass") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passengers by Pclass",
       subtitle = "Titanic voyage: 1912") +
  theme_bw()

# number of passengers by age-groups
# 0-16 => child
# 17-30 => young adults
# 31-45 => middle aged adult
# > 45 => old aged adult
titanic |>
  mutate(
    age_group = ifelse(Age > 0 & Age <= 16, "child[0-16]", 
                       ifelse(Age > 17 & Age < 30, "young adult[17-30]",
                              ifelse(Age > 31 & Age < 45, 
                                     "middle aged adult[31-45]","old aged adult[>45]")))
  ) |> 
  group_by(age_group) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=age_group, x=age_group, y=total)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Age Group") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passengers by age group",
       subtitle = "Titanic voyage: 1912") +
  scale_x_discrete(labels=c("child","middle aged adult",
                            "old aged adult","young adult")) + # rename x values
  theme_bw()
  
# survival rates by sex
titanic |> 
  group_by(survived, Sex) |>
  summarise(
    total = n()
  ) |>
  ggplot(aes(fill=Sex, x=survived, y=total)) + 
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=total), 
            position=position_dodge(width=0.9), vjust=-0.25) +
  scale_fill_viridis(discrete = T) +
  xlab("Survived") + ylab("Total") +
  labs(caption = "Data Source: Kaggle", 
       title = "Passenger Survival Rates by Sex",
       subtitle = "Titanic voyage: 1912") +
  theme_bw()
