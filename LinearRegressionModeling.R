# Linear Regression
source("./ModelingFuncs.R")
source("./DataCleaning.R")
library(readxl)
library(caTools)

# read the data file in as a dataframe
model_data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

model_data <- process_flight_survey(model_data)
sapply(model_data, function(x) sum(is.na(x)))

# set the seed so we can compare our results on the same splits
set.seed(11)



# split the data into training and test data
split = sample.split(model_data$Satisfaction, SplitRatio=.8)
training = subset(model_data, split == TRUE)
test = subset(model_data, split == FALSE)





y = "Satisfaction"

# Simple linear regression to predict satisfaction score
x1 = c("Gender", "Airline_StatusGold", "Airline_StatusPlatinum", "Airline_StatusSilver", "No.")
lm1 <- lm(paste(y, "~", paste(x1, collapse = " + ")), data=training)
lm1.summary <- summary(lm1)
lm1.summary
rm(lm1)




usable_x_cols <- c("Gender", "Price_Sensitivity", "Shopping_Amount_at_Airport", 
                   "Day_of_Month", "Scheduled_Departure_Hour", 
                   "Flight_cancelled", "Flight_time_in_minutes", "Flight_Distance",
                   "Airline_StatusGold", "Airline_StatusPlatinum"  ,           
                   "Airline_StatusSilver", "Type_of_TravelMileage.tickets", "Type_of_TravelPersonal.Travel",      
                   "ClassEco", "ClassEco.Plus", "cat_loyalty_cards.Low",          
                   "cat_loyalty_cards.Medium", "cat_loyalty_cards.High", "cat_No_of_Flights.Medium",           
                   "cat_No_of_Flights.High", "cat_Age..20.30.", "cat_Age..30.40.",                    
                   "cat_Age..40.50.", "cat_Age..50.60.", "cat_Age..60.70.",                    
                   "cat_Age..70..Inf.", "X.cat_._of_FLight_with_other.Medium", "X.cat_._of_FLight_with_other.High", "Age")

lm2 <- lm(paste(y, "~", paste(usable_x_cols, collapse = " + ")), data=training)
lm2.summary <- summary(lm2)
lm2.summary
lm2_preds <- predict(lm2, test)

lm_rmse <- rmse(test$Satisfaction, lm2_preds)
lm_test2 <- model_trainer(xs=usable_x_cols,
              y=y,
              model=lm,
              train=train,
              test=test,
              type="regression")

lm_test2$score
