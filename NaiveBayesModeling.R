source("./ModelingFuncs.R")
source("./DataCleaning.R")
library(readxl)
library(caTools)
library(e1071)

# read the data file in as a dataframe
model_data <- read_xlsx("./data/Satisfaction Survey(2).xlsx")

model_data <- process_flight_survey(model_data)
sapply(model_data, function(x) sum(is.na(x)))

y = "Satisfaction"

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


# set the seed so we can compare our results on the same splits
set.seed(11)


#########################################
# Classification Models
########################################


# split the data into training and test data
model_data$Satisfaction <- as.factor(model_data$Satisfaction)
split = sample.split(model_data$Satisfaction, SplitRatio=.8)
training = subset(model_data, split == TRUE)
test = subset(model_data, split == FALSE)


library(e1071)
# create a naive bayes model with all usable x_cols

nb_model <- naiveBayes(as.formula(paste(y, "~", paste(c("Gender", "Price_Sensitivity", "Age",
                                                        "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                                        "Flight_time_in_minutes",
                                                        "Airline_Status", "DepDelayRatio", "Flight_Distance",
                                                        "Scheduled_Departure_Hour", "Day_of_Month", 
                                                        "Type_of_Travel"), 
                                                      collapse = " + "))), 
                       data=training, laplace=1)
nb_preds <- predict(nb_model, test)
nb_resutls <- data.frame(actual=test$Satisfaction, preds=nb_preds)
nb_acc <- sum(nb_resutls$actual == nb_resutls$preds) / length(nb_resutls$actual)

nb2_res <- model_trainer(xs=c("Gender", "Price_Sensitivity", "Age",
                              "Shopping_Amount_at_Airport", "Flight_cancelled", 
                              "Flight_time_in_minutes"),
                         y=y,
                         model=naiveBayes,
                         train=train,
                         test=test,
                         type="classification")



models <- list()
params <- list(params1=list(xs=c("Gender", "Price_Sensitivity", "Age",
                                 "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                 "Flight_time_in_minutes"), 
                            y=y, 
                            model=naiveBayes, 
                            train=training, test=test,
                            type="classification",
                            params=list(laplace=1)),
               params2=list(xs=c("Gender", "Price_Sensitivity", "Age",
                                 "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                 "Flight_time_in_minutes"), 
                            y=y, 
                            model=naiveBayes, 
                            train=training, test=test,
                            type="classification",
                            params=list(laplace=.5)),
               params3=list(xs=c("Gender", "Price_Sensitivity", "Age",
                                 "Shopping_Amount_at_Airport", "Flight_cancelled",
                                 "Flight_time_in_minutes", "DepDelayRatio", "ArrDelayRatio",
                                 "Airline_StatusGold", "Airline_StatusPlatinum",           
                                 "Airline_StatusSilver"), 
                            y=y, 
                            model=naiveBayes, 
                            train=training, test=test,
                            type="classification",
                            params=list(laplace=.5)),
               params4=list(xs=c("Gender", "Price_Sensitivity", "Age",
                                 "Shopping_Amount_at_Airport", "Flight_cancelled",
                                 "Flight_time_in_minutes", "DepDelayRatio",
                                 "Airline_Status", "Type_of_Travel"), 
                            y=y, 
                            model=naiveBayes, 
                            train=training, test=test,
                            type="classification",
                            params=list(laplace=1)),
               params5=list(xs=c("Gender", "Price_Sensitivity", "Age",
                                 "Shopping_Amount_at_Airport", "Flight_cancelled",
                                 "Flight_time_in_minutes", "DepDelayRatio",
                                 "Airline_Status", "Type_of_Travel", "Scheduled_Departure_Hour",
                                 "Shopping_Amount_at_Airport", "Day_of_Month"), 
                            y=y, 
                            model=naiveBayes, 
                            train=training, test=test,
                            type="classification",
                            params=list())
               )



i = 0
for(i in 1:length(params)) {
  id <- paste("params", i, sep="")
  print(id)
  #
  
  model_results <- do.call(model_trainer, params[[id]])
  # model_results <- model_trainer(xs=params[[id]]$xs,
  #                                y=params[[id]]$y,
  #                                train=train,
  #                                test=test,
  #                                model=params[[id]]$model,
  #                                type=params[[id]]$type
  #                                )

  models[[id]] <- model_results
}

for (model in models){
  cat("Accuracy: ", model$score, "\n")
}

best_model <- models[["params1"]]$model
save(best_model, file="./models/best_naiveBayes.rda")

load(file="./models/best_naiveBayes.rda")

best_preds <- predict(best_model, test)
best_res <- data.frame(test, preds=best_preds)
best_res$result <- best_res$Satisfaction == best_res$preds
best_acc <- mean(best_res$result)
best_res$result <- as.factor(best_res$result)


library(ggplot2)
ggplot(best_res, aes(x=Age, y=Flight_Distance, color=preds, shape=result)) +
  geom_point() +
  ggtitle(paste("NaiveBayes Predictions on Test Data - Accuracy: ", round(best_acc * 100, 2) ,"%", sep=""))
ggsave("./images/best_NB_preds_Distance_Age.png")
