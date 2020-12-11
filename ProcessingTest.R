source("./DataCleaning.R")
library(readxl)

# read the data file in as a dataframe
data2 <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

data2 <- process_flight_survey(data2)
# Check to see if the process_flight_survey matches expectations
cat(identical(data2,data))
