

rmse <- function(actuals, predictions){
  # Compute the Root Mean Squared Error
  rmse <- sqrt(sum((actuals - predictions)^2) / length(actuals)) 
  return(rmse)
}

model_trainer <- function(xs, y, train, test, model, type, params=list()){
  # train a model, make predictions, and score the model on the test data
  # reutrns a list containing the trained model, the predictions, and the 
  # score
  
  cat("Training model:\n")
  # start the clock
  ptm <- proc.time()
  trained_model <- do.call(model, list(formula=as.formula(paste(y, "~", paste(xs, collapse=" + "))), 
                         params=params, data=train))
  print(proc.time() - ptm)
  # trained_model <- model(as.formula(paste(y, "~", paste(xs, collapse=" + "))), 
  #                        params, data=training)
  cat("Predicting on test data:\n")
  predictions <- predict(trained_model, test)
  results <- data.frame(actual=test[, y], predictions=predictions)
  cat("Scoring model:\n")
  if (type == "classification") {
    score <- sum(results$actual == results$predictions) / length(results$actual)

  } else if (type == "regression") {
    score <- rmse(results$actual, results$predictions)
  }
  
  return(list(model=trained_model, results=results, score=score))
}