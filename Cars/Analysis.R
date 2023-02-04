library(tibble)
library(magrittr)

data("mtcars")

View(mtcars)

# we will filter the cars by brand name.

# first let us turn the row names into a columns
mtcars <- mtcars %>%
  rownames_to_column(var="brand")

colnames(mtcars)

# filter data by brand name
mtcars %>%
  dplyr::filter(
    brand == "Mazda RX4"
  )

# you can do more with this data but for now we will only 
# get the car by brand

# lets build the app