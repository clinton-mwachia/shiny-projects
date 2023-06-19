library(data.table)
library(tidyverse)
library(lubridate)
library(viridis)
library(plotly)

# loading the data sets
# members
members = fread("Data/members.csv") %>%
  select(-V1)

# loans
loans = fread("Data/loans.csv") %>%
  select(-V1)

# accounts
accounts = fread("Data/savings.csv") %>%
  select(-V1)

# total members
total_members = nrow(unique(members))

# total loans
total_loans = loans %>%
  summarise(
    tot = sum(amount)
  )

# total loans paid
total_paid = loans %>%
  filter(default == "no") %>%
  summarise(
    tot = sum(amount)
  )

# total loans defaulted
total_defaulted = loans %>%
  filter(default == "yes") %>%
  summarise(
    tot = sum(amount)
  )

# total loans by gender
tot_members_by_gender <- members %>%
  group_by(gender) %>%
  summarise(
    total = n()
  ) %>%
  mutate(gender = fct_reorder(gender, total)) %>%
  ggplot(aes(x=gender, y=total,fill=gender)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, 
            position = position_stack(vjust = 0.8)) +
  scale_fill_manual(values = c(Male = "#c65353",Female="#75a3a3")) +
  xlab("Gender") +
  ylab("Total") +
  ggtitle("Total members by Gender") +
  theme_light() +
  theme(legend.position = "none")

# total members by occupation
tot_members_by_occ <- members %>%
  group_by(occupation)%>%
  summarise(
    total = n()
  ) %>%
  mutate(occupation = fct_reorder(occupation, total)) %>%
  ggplot(aes(x=occupation, y=total,fill=occupation)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.7) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Occupation") +
  ylab("Total") +
  ggtitle("Total members by Occupation") +
  theme_light() +
  theme(legend.position = "none")

# total members by marital status
tot_members_by_marital <- members %>%
  group_by(marital) %>%
  summarise(
    total = n()
  ) %>%
  mutate(marital = fct_reorder(marital, total)) %>%
  ggplot(aes(x=marital, y=total,fill=marital)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Marital Status") +
  ylab("Total") +
  ggtitle("Total members by Marital Status") +
  theme_light() +
  theme(legend.position = "none")

# total members by education
tot_members_by_education <- members %>%
  group_by(education) %>%
  summarise(
    total = n()
  ) %>%
  mutate(education = fct_reorder(education, total)) %>%
  ggplot(aes(x=education, y=total,fill=education)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Education Level") +
  ylab("Total") +
  ggtitle("Total members by Education") +
  theme_light() +
  theme(legend.position = "none")

# total members by account
tot_members_by_account <- members %>%
  group_by(account) %>%
  summarise(
    total = n()
  ) %>%
  mutate(account = fct_reorder(account, total)) %>%
  ggplot(aes(x=account, y=total,fill=account)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Account") +
  ylab("Total") +
  ggtitle("Total members by Account") +
  theme_light() +
  theme(legend.position = "none")

# total members by branch
tot_members_by_branch <- members %>%
  group_by(branch) %>%
  summarise(
    total = n()
  ) %>%
  mutate(branch = fct_reorder(branch, total)) %>%
  ggplot(aes(x=branch, y=total,fill=branch)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Bank Branch") +
  ylab("Total") +
  ggtitle("Total members by Branch Location") +
  theme_light() +
  theme(legend.position = "none")

# TOTAL account amount by gender
tot_acc_amount_by_gender = function(input){
  accounts %>%
    filter(account == input$account2) %>%
    group_by(gender) %>%
    summarise(
      total = sum(amount)
    ) %>%
    mutate(gender = fct_reorder(gender, total)) %>%
    ggplot(aes(x=gender, y=total,fill=gender)) +
    geom_bar(stat="identity",colour="black",alpha=.8,
             width=.8) +
    geom_text(aes(label = comma_format()(total)), 
              size=6, 
              position = position_stack(vjust = 0.8)) +
    scale_fill_manual(values = c(Male = "#c65353",Female="#75a3a3")) +
    xlab("Gender") +
    ylab(glue("{input$account}")) +
    ggtitle(glue("Total {input$account2} amount by {input$col2}")) +
    theme_light() +
    theme(legend.position = "none")
}

# TOTAL account amount by occupation
tot_acc_amount_by_occupation = function(input){
  accounts %>%
    filter(account == input$account2) %>%
    group_by(occupation) %>%
    summarise(
      total = sum(amount)
    ) %>%
    mutate(occupation = fct_reorder(occupation, total)) %>%
    ggplot(aes(x=occupation, y=total,fill=occupation)) +
    geom_bar(stat="identity",colour="black",alpha=.8,
             width=.8) +
    xlab("Occupation") +
    ylab("Total Amount") +
    ggtitle(glue("Total {input$account2} amount by occupation")) +
    theme_light() +
    theme(legend.position = "none")
}

# TOTAL account amount by marital
tot_acc_amount_by_marital = function(input){
  accounts %>%
    filter(account == input$account2) %>%
    group_by(marital) %>%
    summarise(
      total = sum(amount)
    ) %>%
    mutate(marital = fct_reorder(marital, total)) %>%
    ggplot(aes(x=marital, y=total,fill=marital)) +
    geom_bar(stat="identity",colour="black",alpha=.8,
             width=.8) +
    xlab("Marital") +
    ylab("Total Amount") +
    ggtitle(glue("Total {input$account2} amount by marital")) +
    theme_light() +
    theme(legend.position = "none")
}

# TOTAL account amount by education
tot_acc_amount_by_education = function(input){
  accounts %>%
    filter(account == input$account2) %>%
    group_by(education) %>%
    summarise(
      total = sum(amount)
    ) %>%
    mutate(education = fct_reorder(education, total)) %>%
    ggplot(aes(x=education, y=total,fill=education)) +
    geom_bar(stat="identity",colour="black",alpha=.8,
             width=.8) +
    xlab("Education") +
    ylab("Total Amount") +
    ggtitle(glue("Total {input$account2} amount by education")) +
    theme_light() +
    theme(legend.position = "none")
}

# TOTAL account amount by branch
tot_acc_amount_by_branch = function(input){
  accounts %>%
    filter(account == input$account2) %>%
    group_by(branch) %>%
    summarise(
      total = sum(amount)
    ) %>%
    mutate(branch = fct_reorder(branch, total)) %>%
    ggplot(aes(x=branch, y=total,fill=branch)) +
    geom_bar(stat="identity",colour="black",alpha=.8,
             width=.8) +
    xlab("Branch") +
    ylab("Total Amount") +
    ggtitle(glue("Total {input$account2} amount by branch")) +
    theme_light() +
    theme(legend.position = "none")
}

# total loans by occupation
total_loans_by_occupation = loans %>%
  dplyr::select(occupation, amount) %>%
  group_by(occupation) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  mutate(occupation = fct_reorder(occupation, loans)) %>%
  ggplot(aes(x=occupation, y=loans,group=1)) +
  geom_point(aes(colour=occupation, size=loans)) +
  geom_line(linetype=2) +
  xlab("Occupation") +
  ylab("Loans") +
  ggtitle("Total Loans by Occupation") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90))