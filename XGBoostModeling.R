

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
                   "cat_No_of_Flights.High", "X.cat_._of_FLight_with_other.Medium", 
                   "X.cat_._of_FLight_with_other.High", "Age", "DepDelayRatio", "No_of_Flights_p.a.")


# set the seed so we can compare our results on the same splits
set.seed(11)

# split the data into training and test data
unique(model_data$Satisfaction)

model_data$Satisfaction <- as.numeric(factor(model_data$Satisfaction,
                                     levels=c(1.0, 2.0, 2.5, 3.0, 3.4, 3.5, 4.0, 4.5, 5.0),
                                     labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9)))

model_data$Satisfaction <- model_data$Satisfaction - 1
model_data$Type_of_Travel <- as.factor(model_data$Type_of_Travel)
model_data$Gender <- as.numeric(factor(model_data$Gender,
                                       levels=c("Male", "Female"),
                                       labels=c(0, 1)))
model_data$Flight_cancelled <- as.numeric(factor(model_data$Flight_cancelled,
                                                 levels=c("No", "Yes"),
                                                 labels=c(0, 1)))


split = sample.split(model_data$Satisfaction, SplitRatio=.8)
training = subset(model_data, split == TRUE)
test = subset(model_data, split == FALSE)

library(xgboost)
xs <- c( "Price_Sensitivity", "Shopping_Amount_at_Airport", "Age", 
         "DepDelayRatio", "No_of_Flights_p.a.", "Type_of_Travel")
classifier <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror") ,
                      data=as.matrix(training[, usable_x_cols]),
                      label=training$Satisfaction, nrounds=250,
                      verbose=2)
xgb.save(classifier, "./models/classifier250tree.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier))
feature_importance <- data.frame(xgb.importance(xs, model=classifier))

library(ggplot2)

ggplot(feature_importance, aes(x=Feature, y=Gain)) +
  geom_bar(stat="identity")


preds <- predict(classifier,
                 newdata=as.matrix(test[, usable_x_cols]))

merror = sum(!(test$Satisfaction == preds)) / length(test$Satisfaction)
merror
table(test$Satisfaction, preds)


## 2
classifier2 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=3) ,
                      data=as.matrix(training[, usable_x_cols]),
                      label=training$Satisfaction, nrounds=500,
                      verbose=2)
xgb.save(classifier2, "./models/classifier500tree.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier2))
preds2 <- predict(classifier2,
                 newdata=as.matrix(test[, usable_x_cols]))

merror2 = sum(!(test$Satisfaction == preds2)) / length(test$Satisfaction)

## 3
classifier3 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=6) ,
                       data=as.matrix(training[, usable_x_cols]),
                       label=training$Satisfaction, nrounds=500,
                       verbose=1)
xgb.save(classifier3, "./models/classifier500tree_6depth.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier3))
preds3 <- predict(classifier3,
                  newdata=as.matrix(test[, usable_x_cols]))

merror3 = sum(!(test$Satisfaction == preds3)) / length(test$Satisfaction)

## 4
classifier4 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=3,
                                   "eta"=0.1) ,
                       data=as.matrix(training[, usable_x_cols]),
                       label=training$Satisfaction, nrounds=1000,
                       verbose=1)
xgb.save(classifier4, "./models/classifier1000tree_3depth.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier4))
preds4 <- predict(classifier4,
                  newdata=as.matrix(test[, usable_x_cols]))

merror4 = sum(!(test$Satisfaction == preds4)) / length(test$Satisfaction)
class4_res <- data.frame(test, preds=preds4)
class4_res$result <- class4_res$Satisfaction == class4_res$preds
class4_res$preds <- as.factor(class4_res$preds) 

library(ggplot2)
ggplot(class4_res, aes(x=Age, y=Flight_Distance, color=preds, shape=result)) +
geom_point() +
ggtitle(paste("XGBoost Predictions on Test Data - Accuracy: ", round((1 - merror4) * 100, 2) ,"%", sep=""))
ggsave("./images/classifier4_preds_Distance_Age.png")
    
  


## 5
classifier5 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=6,
                                   "eta"=0.3) ,
                       data=as.matrix(training[, usable_x_cols]),
                       label=training$Satisfaction, nrounds=1000,
                       verbose=1)
xgb.save(classifier5, "./models/classifier1000tree_6depth_3eta.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier5))
preds5 <- predict(classifier5,
                  newdata=as.matrix(test[, usable_x_cols]))

merror5 = sum(!(test$Satisfaction == preds5)) / length(test$Satisfaction)


## 6
classifier6 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=3,
                                   "eta"=0.3) ,
                       data=as.matrix(training[, usable_x_cols]),
                       label=training$Satisfaction, nrounds=2000,
                       verbose=1)
xgb.save(classifier6, "./models/classifier2000tree_3depth_3eta.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier6))
preds6 <- predict(classifier6,
                  newdata=as.matrix(test[, usable_x_cols]))

merror6 = sum(!(test$Satisfaction == preds6)) / length(test$Satisfaction)

## 7
classifier7 <- xgboost(params=list(objective="multi:softmax", "num_class"=9, "eval_metric"="merror", "max_depth"=3,
                                   "eta"=0.1) ,
                       data=as.matrix(training[, usable_x_cols]),
                       label=training$Satisfaction, nrounds=5000,
                       verbose=1)
xgb.save(classifier7, "./models/classifier5000tree_3depth_1eta.model")

xgb.plot.importance(xgb.importance(usable_x_cols, model=classifier7))
preds7 <- predict(classifier7,
                  newdata=as.matrix(test[, usable_x_cols]))

merror7 = sum(!(test$Satisfaction == preds7)) / length(test$Satisfaction)


classifier <-xgb.load("./models/classifier250tree.model")
classifier2 <- xgb.load("./models/classifier500tree.model")
classifier3 <- xgb.load("./models/classifier500tree_6depth.model")
classifier4 <- xgb.load("./models/classifier1000tree_3depth.model")
classifier5 <- xgb.load("./models/classifier1000tree_6depth_3eta.model")
classifier6 <- xgb.load("./models/classifier2000tree_3depth_3eta.model")