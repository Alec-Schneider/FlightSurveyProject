## SVM modeling
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
# Support Vector Machine Model Training
########################################

# Clean up and scale some of the columns

model_data$Satisfaction <- as.numeric(factor(model_data$Satisfaction,
                                             levels=c(1.0, 2.0, 2.5, 3.0, 3.4, 3.5, 4.0, 4.5, 5.0),
                                             labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9)))
model_data$Gender <- as.numeric(factor(model_data$Gender,
                                       levels=c("Male", "Female"),
                                       labels=c(0, 1)))
model_data$Flight_cancelled <- as.numeric(factor(model_data$Flight_cancelled,
                                                 levels=c("No", "Yes"),
                                                 labels=c(0, 1)))


scale_cols <- c("Shopping_Amount_at_Airport",
                "Flight_time_in_minutes",
                "Flight_Distance", "No_of_Flights_p.a.", "Eating_and_Drinking_at_Airport", 
                "Departure_Delay_in_Minutes", "Arrival_Delay_in_Minutes")

for (col in scale_cols){
  print(col)
  model_data[, col] <- scale(model_data[,col])
}


# split the data into training and test data
split = sample.split(model_data$Satisfaction, SplitRatio=.8)
training = subset(model_data, split == TRUE)
test = subset(model_data, split == FALSE)


library(e1071)
svm1 <- svm(as.formula(paste(y, "~", paste(c( "Price_Sensitivity", "Age",
                                              "Shopping_Amount_at_Airport",
                                              "Flight_time_in_minutes",
                                              "Flight_Distance",
                                              "Scheduled_Departure_Hour", "Day_of_Month"), 
                                              collapse = " + "))), 
                                              data=training, type="C-classification")
save(svm1, file="./models/svm1.rda")
svm1_preds = predict(svm1, test)
# svm1_probs <- predict(svm1, test, type="raw")

svm1_acc = sum(test$Satisfaction == svm1_preds) / length(svm1_preds)
table(test$Satisfaction, svm1_preds)


svm2 <- svm(as.formula(paste(y, "~", paste(c( "Price_Sensitivity", "Age",
                                              "Shopping_Amount_at_Airport",
                                              "Flight_time_in_minutes",
                                              "Flight_Distance"), 
                                              collapse = " + "))), 
                                              data=training, type="C-classification")
save(svm2, file="./models/svm2.rda")
load("./models/svm2.rda")
svm2_preds = predict(svm2, test)
svm2_acc = sum(test$Satisfaction == svm2_preds) / length(svm2_preds)
# svm2_probs <- predict(svm2, test)

svm2_results <- data.frame(test, predictions=svm2_preds)
svm2_results$result <- svm2_results$Satisfaction == svm2_results$predictions
#svm2_results$result <- as.numeric(svm2_results$result)
table(svm2_results$Satisfaction, svm2_results$predictions)

library(ggplot2)
# library(plotROC)
# library(pROC)
ggplot(svm2_results, aes(x=Age, y=Flight_Distance, color=predictions, shape=result)) +
  geom_point() +
  ggtitle(paste("SVM Predictions on Test Data - Accuracy: ", round(svm2_acc * 100, 2) ,"%", sep=""))
ggsave("./images/SVM2_preds_Distance_Age.png")

ggplot(svm2_results, aes(x=Age, y=Flight_Distance, color=result, shape=predictions)) +
  geom_point() +
  ggtitle("SVM Predictions on Test Data")


# # ROC AUC plot
# svm2_roc <- ggplot(svm2_results, aes(m=Satisfaction, d=result, color=Satisfaction)) + geom_roc()
# svm2_roc <- svm2_roc + annotate("text", x = .75, y = .25, 
#                                   label = paste("AUC =", round(calc_auc(svm2_roc)$AUC, 2))) +
#   ggtitle("ROC and AUC Plot") +
#   style_roc()
# svm2_roc

####################################
# Train multiple models
####################################
models <- list()
# params <- list(params1=list(xs=c("Gender", "Price_Sensitivity", "Age",
#                                  "Shopping_Amount_at_Airport", "Flight_cancelled", 
#                                  "Flight_time_in_minutes",
#                                  "Flight_Distance",
#                                  "Scheduled_Departure_Hour", "Day_of_Month"), 
#                             y=y, 
#                             model=svm, 
#                             train=training,
#                             test=test,
#                             type="classification",
#                             params=list(kernel="raidal basis",
#                                         cost=10,
#                                         type="C-classification")),
#                params2=list(xs=c("Gender", "Price_Sensitivity", "Age",
#                                  "Shopping_Amount_at_Airport", "Flight_cancelled",
#                                  "Flight_time_in_minutes", "Eating_and_Drinking_at_Airport",
#                                  "No_of_Flights_p.a.","Departure_Delay_in_Minutes"), 
#                             y=y, 
#                             model=svm, 
#                             type="classification",
#                             train=training,
#                             test=test,
#                             params=list(kernel="raidal basis",
#                                         cost=5,
#                                         type="C-classification")),
#                params3=list(xs=c("Gender", "Price_Sensitivity", "Age",
#                                  "Shopping_Amount_at_Airport", "Flight_cancelled", 
#                                  "Flight_time_in_minutes",
#                                  "Flight_Distance",
#                                  "Scheduled_Departure_Hour", "Day_of_Month",
#                                  "Type_of_TravelMileage.tickets", "Type_of_TravelPersonal.Travel"), 
#                             y=y, 
#                             model=svm, 
#                             train=training,
#                             test=test,
#                             type="classification",
#                             params=list(kernel="raidal basis",
#                                         cost=3,
#                                         type="C-classification")),
#                params4=list(xs=c("Gender", "Price_Sensitivity", "Age",
#                                  "Shopping_Amount_at_Airport", "Flight_cancelled", 
#                                  "Flight_time_in_minutes",
#                                  "Flight_Distance",
#                                  "Scheduled_Departure_Hour", "Day_of_Month",
#                                  "Type_of_TravelMileage.tickets", "Type_of_TravelPersonal.Travel"), 
#                             y=y, 
#                             model=svm, 
#                             train=training,
#                             test=test,
#                             type="classification",
#                             params=list(kernel="raidal basis",
#                                         cost=1,
#                                         type="C-classification"))
#                )
# 


svm3 <- svm(as.formula(paste(y, "~", paste(c("Gender", "Price_Sensitivity", "Age",
                                                    "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                                    "Flight_time_in_minutes",
                                                    "Flight_Distance",
                                                    "Scheduled_Departure_Hour", "Day_of_Month"), 
                                                  collapse = " + "))), 
                   data=training, type="C-classification", kernel="radial",
                   cost=10)
save(svm3, "./models/svm3.rda")


svm4 <- svm(as.formula(paste(y, "~", paste(c("Gender", "Price_Sensitivity", "Age",
                                            "Shopping_Amount_at_Airport", "Flight_cancelled",
                                            "Flight_time_in_minutes", "Eating_and_Drinking_at_Airport",
                                            "No_of_Flights_p.a.","Departure_Delay_in_Minutes"), 
                                           collapse = " + "))), 
            data=training, type="C-classification", kernel="radial",
            cost=5)
save(svm4, "./models/svm4.rda")


svm5 <- svm(as.formula(paste(y, "~", paste(c("Gender", "Price_Sensitivity", "Age",
                                             "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                             "Flight_time_in_minutes",
                                             "Flight_Distance",
                                             "Scheduled_Departure_Hour", "Day_of_Month",
                                             "Type_of_TravelMileage.tickets", "Type_of_TravelPersonal.Travel"), 
                                           collapse = " + "))), 
            data=training, type="C-classification", kernel="radial",
            cost=3)
save(svm5, "./models/svm5.rda")

svm6 <- svm(as.formula(paste(y, "~", paste(c("Gender", "Price_Sensitivity", "Age",
                                             "Shopping_Amount_at_Airport", "Flight_cancelled", 
                                             "Flight_time_in_minutes",
                                             "Flight_Distance",
                                             "Scheduled_Departure_Hour", "Day_of_Month",
                                             "Type_of_TravelMileage.tickets", "Type_of_TravelPersonal.Travel"), 
                                           collapse = " + "))), 
            data=training, type="C-classification", kernel="radial",
            cost=1)
save(svm6, "./models/svm6.rda")


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
  # )
  
  models[[id]] <- model_results
}

best_acc <- 0
best_model <- NULL

for (model in models){
  cat("Accuracy: ", model$score, "\n")
  if (model$score > best_acc){
    best_model <- model$model
    save(best_model, "./models/best_svm.rda")
  }

}
