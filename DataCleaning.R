# Data Cleaning

library(readxl)

# read the data file in as a dataframe
data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

# remove the rows with NAs in Satisfaction
data <- data[!is.na(data$Satisfaction),]


