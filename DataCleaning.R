# Data Cleaning functions for the Flight Surveyr dataset

require(tidyverse)
require(caret)

clean_column_names <- function(data){
  # remove all spaces from the column names as this may cause a problem later on
  for (i in 1:length(colnames(data))){
    colnames(data)[i] <- gsub(" ", "_", colnames(data)[i])
    }
  data <- rename(data, Origin_City=Orgin_City)
  return(data)
}


fillNAs <- function(data){
  
  # Impute the NAs in Satisfaction with the mean
  data$Satisfaction <- ifelse(is.na(data$Satisfaction), 
                              mean(data$Satisfaction, na.rm = TRUE), 
                              data$Satisfaction)

  # drop the 337 flights where the flight was not cancelled, and Arrival Delay in Minutes is NA.
  data <- data[!((data$Flight_cancelled == "No") & is.na(data$Arrival_Delay_in_Minutes)),]
  
  # impute the NAs in Arrival_Delay in 
  data$Departure_Delay_in_Minutes <- ifelse(is.na(data$Departure_Delay_in_Minutes), 
                              0, 
                              data$Departure_Delay_in_Minutes)
  
  data$Arrival_Delay_in_Minutes <- ifelse(is.na(data$Arrival_Delay_in_Minutes), 
                                           0, 
                                           data$Arrival_Delay_in_Minutes)
  
  data$Flight_time_in_minutes <- ifelse(is.na(data$Flight_time_in_minutes), 
                                           0, 
                                           data$Flight_time_in_minutes)
  
  
  return(data)  
}


category_processing <- function(data){
  # Create categories for the Number of loyalty cards a customer has
  data <- data %>%
    mutate(cat_loyalty_cards=cut(No._of_other_Loyalty_Cards, 
                                 breaks=c(-Inf, 0, 3, 6, 12), 
                                 labels=c("None", "Low", "Medium", "High"))) %>%
    mutate(`cat_%_of_FLight_with_other`=cut(`%_of_Flight_with_other_Airlines`, 
                                            breaks=c(-Inf, quantile(data$`%_of_Flight_with_other_Airlines`, 
                                                            c(1/3, 2/3, 1))), 
                                            labels=c("Low", "Medium", "High"))) %>%
    mutate(cat_No_of_Flights=cut(No_of_Flights_p.a., 
                                 breaks=c(-Inf, quantile(data$No_of_Flights_p.a., 
                                                 c(1/3, 2/3, 1))), 
                                 labels=c("Low", "Medium", "High"))) %>%
    mutate(cat_Age=cut(Age,
                       breaks=c(-Inf, 20, 30, 40, 50, 60, 70, Inf)))
  
  
  return(data)
}


dummy_processing <- function(data) {
  # use a for loop to one hot encode all the 
  # Will need to manuall do cat_%_of_FLight_with_other do to the % sign being an issue
  dummy_cols <- c("Airline_Status", "Type_of_Travel", "Class", "cat_loyalty_cards",
                  "cat_No_of_Flights", "cat_Age")
  
  dummy_df <- data.frame()
  for (col in dummy_cols){
    dmy <- dummyVars(paste("~", col, sep = " ") , data=data)
    if (dim(dummy_df)[1] != 0){ 
      # need to return n-1 values of the categorical variable for analysis
      dummy_df <- cbind(dummy_df, data.frame(predict(dmy, newdata = data))[,-1])
    } else {
      dummy_df <- data.frame(predict(dmy, newdata = data))[,-1]
    }
  }
  dmy <- dummyVars(~ `cat_%_of_FLight_with_other`, data=data)
  dummy_df <- cbind(dummy_df, data.frame(predict(dmy, newdata = data))[,-1])
  
  data <- cbind(data, dummy_df)
  return(data)
}


process_flight_survey <- function(data){
  
  data <- clean_column_names(data)
  data <- fillNAs(data)
  data$Satisfaction <- round(data$Satisfaction, 1)
  # Create columns for Departure Delay and Arrival Delay as a percent of Flight Time
  data <- data %>%
    mutate(DepDelayRatio =(Departure_Delay_in_Minutes / Flight_time_in_minutes),
           ArrDelayRatio =(Arrival_Delay_in_Minutes / Flight_time_in_minutes))
  # Fill in the NAs in ratio columns created from 0 / 0
  data$DepDelayRatio <- ifelse(is.na(data$DepDelayRatio), 0, data$DepDelayRatio)
  data$ArrDelayRatio <- ifelse(is.na(data$ArrDelayRatio), 0, data$ArrDelayRatio)
  
  data <- category_processing(data)
  data <- dummy_processing(data)
  
}



