# Exploratory Data Analysis
library(tidyverse)
library(readxl)

# read the data file in as a dataframe
data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

# Find the amount of null values in each column
sapply(data, function(x) sum(is.na(x)))

# Find the amount of null values in each column using dplyr
nulls <- data %>%
  select(everything()) %>%
  summarise_all(funs(sum(is.na(.)))) %>%
  transpose()

nulls <- data.frame(unlist(nulls))


# Check on the nulls in the satisfaction column
# nothing out of the ordinary here. Will drop these rows in our learning
nullSats <- data %>%
  filter(is.na(Satisfaction))

# Create a bar chart to show the counts of each Satisfaction 
data %>%
  filter(!is.na(Satisfaction)) %>%
  ggplot(mapping = aes(x=Satisfaction)) +
    geom_bar()


# Look at only data with null arrivalDelays
nullArrivalDelay <- data %>%
  filter(is.na(`Arrival Delay in Minutes`))

# Not all NA values for Arrival Delay in Minutes are cancelled, interesting
sum(nullArrivalDelay$`Flight cancelled` == "Yes")
sum(nullArrivalDelay$`Flight cancelled` == "No")
