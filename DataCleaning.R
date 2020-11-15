# Data Cleaning

library(readxl)

# read the data file in as a dataframe
data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

# remove the rows with NAs in Satisfaction
data <- data[!is.na(data$Satisfaction),]


clean_satisfaction_survey <- function(data){
  # remove all spaces from the column names as this may cause a problem later on
  for (i in 1:length(colnames(data))){
    colnames(data)[i] <- gsub(" ", "_", colnames(data)[i])
  }
  
  # fix the origin city misspell
  data <- rename(data, Origin_City=Orgin_City)
  
}