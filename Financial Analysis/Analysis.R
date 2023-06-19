library(tidyverse)
library(data.table)
library(scales)
library(viridis)
library(gt)
library(gtExtras)
library(lubridate)

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
total_loans = nrow(unique(loans))

# total accounts
total_accounts = nrow(unique(accounts))

################## start members ############################
# total members by gender
members %>%
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
members %>%
  group_by(occupation)%>%
  summarise(
    total = n()
  ) %>%
  mutate(occupation = fct_reorder(occupation, total)) %>%
  ggplot(aes(x=occupation, y=total,fill=occupation)) +
  geom_bar(stat="identity",colour="black",alpha=.8,
           width=.6) +
  geom_text(aes(label = comma_format()(total)), 
            size=6, angle=90,
            position = position_stack(vjust = 0.6)) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Occupation") +
  ylab("Total") +
  ggtitle("Total members by Occupation") +
  theme_light() +
  theme(legend.position = "none",axis.text.x = element_text(angle = 45))


# total members by marital status
members %>%
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
  theme(legend.position = "none",axis.text.x = element_text(angle = 45))

# total members by education
# plot
members %>%
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
# plot
members %>%
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
# plot
members %>%
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


# get member by name
members %>%
  filter(name == "Asia Roberts") %>%
  gt() %>%
  tab_header(
    title = "Asia Roberts"
  )

# total number of members by branch
members %>%
  select(branch) %>%
  group_by(branch) %>%
  summarise(
    members = n()
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Total Members**"),
    subtitle = md("By **Location Branch**")
  ) %>%
  fmt_number(
    columns = c("members"),
    decimals = 2
  )  %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  gt_theme_538() %>%
  tab_options(
   table.width = pct(50),
   heading.align = "center",
   heading.background.color = "grey"
  )
  
# total members by branch and gender
members %>%
  select(branch, gender) %>%
  group_by(branch, gender) %>%
  summarise(
    members = n()
  ) %>%
  mutate(branch = fct_reorder(branch, members)) %>%
  ggplot(aes(x=branch, y=members, fill=gender)) +
    geom_bar(stat="identity", position = "dodge",width=.4) +
    scale_fill_viridis(discrete = TRUE) +
    theme_light() +
    xlab("Branch") + ylab("Members") +
    ggtitle("Total Members by branch and gender") +
    theme(legend.position = "right")

# total members by branch and occupation
members %>%
  select(branch, occupation) %>%
  group_by(branch, occupation) %>%
  summarise(
    members = n()
  ) %>%
  mutate(branch = fct_reorder(branch, members)) %>%
  ggplot(aes(x=branch, y=members, fill=occupation)) +
    geom_bar(stat="identity", position = "dodge",width=.4) +
    scale_fill_viridis(discrete = TRUE) +
    theme_light() +
    facet_wrap(~occupation) +
    xlab("Branch") + ylab("Members") +
    ggtitle("Total Members by branch and occupation") +
    theme(legend.position = "none")

# filter members by date
members %>%
  filter(date_joined >= "2019-01-01" & date_joined <= "2020-01-01")

# get member by ID
members %>%
  filter(ID == "54353720")

# get member by account number
members %>%
  filter(account_no == "160653")

################# end members ########


#################### start accounts ####################
# total savings(account) by gender
accounts %>%
  filter(account == "savings") %>%
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
  ylab("Savings") +
  ggtitle("Total savings by Gender") +
  theme_light() +
  theme(legend.position = "none")

# total savings(account) by branch
accounts %>%
  filter(account == "savings") %>%
  group_by(branch) %>%
  summarise(
    savings = sum(amount)
  ) %>%
  mutate(gender = fct_reorder(branch, savings)) %>%
  ggplot(aes(x=branch, y=savings,fill=branch)) +
    geom_bar(stat="identity",position = "dodge",colour="black",alpha=.8,
             width=.8) +
    geom_text(aes(label = comma_format()(savings)), 
              size=6, 
              position = position_stack(vjust = 0.5),angle=90) +
    xlab("Gender") +
    ylab("Savings") +
    ggtitle("Total Savings by Branch") +
    theme_light() +
    theme(legend.position = "right")

# total savings(account) by marital
accounts %>%
  filter(account == "savings") %>%
  group_by(marital) %>%
  summarise(
    savings = sum(amount)
  ) %>%
  mutate(marital = fct_reorder(marital, savings))%>%
  ggplot( aes(x=marital, y=savings)) +
    geom_segment(aes(xend=marital, yend=0)) +
    geom_point(size=4, color="blue") +
    geom_text(aes(label = comma_format()(savings)), 
              size=6, 
              position = position_stack(vjust = 0.5),angle=90) +
    theme_light() +
    xlab("Marital Status") +
    ggtitle("Total Savings by Marital Status")


# total savings(account) by gender and occupation
accounts %>%
  filter(account == "savings") %>%
  group_by(gender, occupation) %>%
  summarise(
    total = sum(amount)
  ) %>%
  mutate(gender = fct_reorder(gender, total)) %>%
  ggplot(aes(x=gender, y=total,fill=occupation)) +
  geom_bar(stat="identity",position = "dodge",colour="black",alpha=.8,
           width=.8) +
  xlab("Gender") +
  ylab("Savings") +
  ggtitle("Total Savings by Gender and Occupation") +
  theme_light() +
  theme(legend.position = "right")

################### end accounts ######################

################## start loans ######################
# loans by occupation
loans %>%
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

# loans by occupation and branch
loans %>%
  dplyr::select(occupation, amount, branch) %>%
  group_by(occupation, branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  mutate(occupation = fct_reorder(occupation, loans)) %>%
  ggplot(aes(x=occupation, y=loans,group=1)) +
  geom_point(aes(colour=occupation, size=loans)) +
  geom_line(linetype=2) +
  xlab("Occupation") +
  ylab("Loans") +
  ggtitle("Total Loans by Occupation and branch") +
  theme_light() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~branch)

# loans by branch and year
loans %>%
  dplyr::mutate(year = year(date)) %>%
  dplyr::select(year, amount, branch) %>%
  group_by(year, branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=year, y=loans,group=1)) +
    geom_point(aes(colour=branch, size=loans)) +
    geom_line(linetype=1) +
    xlab("Year") +
    ylab("Total Loan") +
    ggtitle("Total Loans by year and branch") +
    theme_light() +
    theme(legend.position = "none") +
    facet_wrap(~branch)

# total loans by branch
loans %>%
  select(branch, amount) %>%
  group_by(branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Total Loans**"),
    subtitle = md("By **Location Branch**")
  ) %>%
  fmt_number(
    columns = c("loans"),
    decimals = 2
  )  %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  gt_theme_538() %>%
  tab_options(
    table.width = pct(50),
    heading.align = "center",
    heading.background.color = "grey"
  )

# total loans by branch
# plot
loans %>%
  dplyr::select(branch, amount) %>%
  group_by(branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=branch, y=loans, group=1)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Branch") + ylab("Total Loans") +
  ggtitle("Total Loans by Branch")

# total loans by branch
# plot
loans %>%
  dplyr::select(branch, amount) %>%
  group_by(branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=branch, y=loans, group=1)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Branch") + ylab("Total Loans") +
  ggtitle("Total Loans by Branch")

# total loans by branch and year
# plot
loans %>%
  mutate(year = year(date)) %>%
  dplyr::select(branch, amount, year) %>%
  group_by(branch, year) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=year, y=loans, fill=branch)) +
  geom_bar(stat="identity") +
  theme_light() +
  xlab("Year") + ylab("Total Loans") +
  ggtitle("Total Loans by Branch and Year")

# total loans by reason
# plot
loans %>%
  dplyr::select(reason, amount) %>%
  group_by(reason) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=reason, y=loans, group=1)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Branch") + ylab("Total Loans") +
  ggtitle("Total Loans by Branch")

# total loans by reason and branch
# plot
loans %>%
  dplyr::select(reason, amount, branch) %>%
  group_by(reason, branch) %>%
  summarise(
    loans = sum(amount)
  ) %>%
  ggplot(aes(x=reason, y=loans, group=1)) +
  geom_line(aes(colour=branch)) +
  geom_point() +
  theme_light() +
  xlab("Purpose") + ylab("Total Loans") +
  ggtitle("Total Loans by Branch and Purpose") +
  theme(legend.position = "none") +
  facet_wrap(~branch)

# total amount of loans defaulted
loans %>%
  select(default) %>%
  group_by(default) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    perc = (n / sum(n)*100),
    n = comma_format()(n)
    ) %>%
  gt() %>%
  tab_header(
    title = md("**Loan Default**")
  ) %>%
  fmt_number(
    columns = c("perc"),
    decimals = 3
  )  %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  gt_theme_538() %>%
  tab_options(
    table.width = pct(50),
    heading.align = "center",
    heading.background.color = "brown"
  )

# total amount of loans defaulted and branch
loans %>%
  select(default, branch) %>%
  group_by(default, branch) %>%
  summarise(
    n = n()
  ) %>%
  mutate(
    percentage = (n / sum(n)*100),
    n = comma_format()(n)
  ) %>%
  gt() %>%
  tab_header(
    title = md("**Loan Default**"),
    subtitle = md("**By** Branch")
  ) %>%
  fmt_number(
    columns = c("percentage"),
    decimals = 3
  )  %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(
      columns = everything(),
      rows = everything()
    )
  ) %>%
  gt_theme_538() %>%
  tab_options(
    table.width = pct(50),
    heading.align = "center",
    heading.background.color = "brown"
  )

# total loan by default and branch
loans %>%
  dplyr::select(branch, default, amount) %>%
  group_by(branch, default) %>%
  summarise(
    Loan = sum(amount)
  ) %>%
  ggplot(aes(x=branch, y=Loan, group=1)) +
  geom_line() +
  geom_point() +
  theme_light() +
  xlab("Branch") + ggtitle("Total Loans by default and branch") +
  facet_wrap(~default)
################# end loans #######################