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
  summarise_all(list( ~ sum(is.na(.)))) %>%    # lambda function
  transpose()

nulls <- data.frame(unlist(nulls))


# Check on the nulls in the satisfaction column
# nothing out of the ordinary here. Will drop these rows in our learning
nullSats <- data %>%
  filter(is.na(Satisfaction))

# remove the rows with NAs in Satisfaction
data <- data[!is.na(data$Satisfaction),]

# Create a bar chart to show the counts of each Satisfaction 
data %>%
  ggplot(mapping = aes(x=Satisfaction)) +
    geom_bar()


# Look at only data with null Departure Delays
nullDepDelay <- data %>%
  filter(is.na(`Departure Delay in Minutes`))

# Look at only data with null arrivalDelays
nullArrivalDelay <- data %>%
  filter(is.na(`Arrival Delay in Minutes`))

# Not all NA values for Arrival Delay in Minutes are cancelled, interesting
sum(nullArrivalDelay$`Flight cancelled` == "Yes")
sum(nullArrivalDelay$`Flight cancelled` == "No")

# get the amount of NA values that are related to Flights being cancelled
data %>%
  filter(`Flight cancelled` == "Yes") %>%
  select(c(`Flight cancelled`, `Departure Delay in Minutes`, `Arrival Delay in Minutes`, `Flight time in minutes`)) %>%
  summarise_all(list( ~ sum(is.na(.)))) %>%    # lambda function
  transpose()

# get the amount of NA values that are not related to Flights being cancelled
View(data %>%
  filter(`Flight cancelled` == "No") %>%
  filter(is.na(`Arrival Delay in Minutes`)))

  