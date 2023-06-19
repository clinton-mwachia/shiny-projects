library(randomNames)
library(tidyverse)
library(lubridate)

# generating the members data

# members
n <- 500000
{
occupations = c("bodaboda", "student", "entrepreneur", "self-employed", 
                "housemaid", "manager", "kra", "teacher")
maritals = c("married", "single", "divorced")
education =  c("primary", "secondary", "college", "university")
account =  c("savings", "salary", "student", "kid")
branch = c("voi", "wundanyi", "mwatate", "taveta")

## data
members <- randomNames(n,return.complete.data = TRUE) %>%
  select(-ethnicity) %>%
  unite("name", first_name:last_name, sep = " ") %>%
  relocate(name, .before = gender) %>%
  mutate(
    date_joined = as_date(sample(as_date("2008-10-22"):as_date("2023-3-4"),n, replace=TRUE)),
    gender = ifelse(gender == 0,"Male","Female"),
    age = sample(18:90, n, replace=TRUE),
    occupation = sample(occupations, n, replace=TRUE),
    marital = sample(maritals, n, replace=TRUE),
    education = sample(education, n, replace=TRUE),
    account = sample(account, n, replace=TRUE),
    account_no = sample(1000:600000,n,replace=FALSE),
    ID = sample(10000000:60000000, n, replace=FALSE),
    branch = sample(branch, n, replace=TRUE),
    phone = sample(0710000000:0790000000, n, replace=FALSE),
    next_of_kin = randomNames(n, name.order = "first.last", name.sep = " "),
    kin_ID = sample(10000000:60000000, n, replace=FALSE),
    kin_phone = sample(0710000000:0790000000, n, replace=FALSE)) %>%
  relocate(date_joined, .before = "name")

# write the data
write.csv(members, file = "Data/members.csv")
}

# loans
n2 = 100000
{
loans = members %>%
  sample_n(n2) %>%
  select(-c(date_joined, gender, age, education,next_of_kin, kin_ID, kin_phone)) %>%
  mutate(
    date = as_date(sample(as_date("2010-10-22"):as_date("2023-3-4"),n2, replace=TRUE)),
    reason = sample(c("Fees","Medication","business"),n2,replace=TRUE),
    period = sample(1:20, n2, replace=TRUE),
    amount = sample(1000:300000, n2, replace=TRUE),
    default = sample(c("yes","no"), n2, replace=TRUE),
    guarantor1 = randomNames(n2, name.order = "first.last", name.sep = " "),
    guarantor1_phone = sample(0710000000:0790000000, n2, replace=FALSE),
    guarantor1_ID = sample(1000:200000, n2, replace=FALSE),
    guarantor2 = randomNames(n2, name.order = "first.last", name.sep = " "),
    guarantor2_phone = sample(0710000000:0790000000, n2, replace=FALSE),
    guarantor2_ID = sample(2000:300000, n2, replace=FALSE)
  ) %>%
  relocate(date, .before = "name")

# write the data
write.csv(loans, file = "Data/loans.csv")
}

# savings
# attributes
# date, name, gender, age, occupation, education, ID, account, phone, amount, month
n3 <- 200000
{
savings <- members %>%
    sample_n(n3) %>%
    select(-c(date_joined, next_of_kin, kin_ID, kin_phone)) %>%
    mutate(
      date = as_date(sample(as_date("2013-10-22"):as_date("2023-3-4"),n3, replace=TRUE)),
      amount = sample(1000:30000, n3, replace=TRUE),
    ) %>%
    relocate(date, .before = "name")
  
write.csv(savings, "Data/savings.csv")
}
