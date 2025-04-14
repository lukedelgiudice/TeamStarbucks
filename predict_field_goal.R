# predict_field_goal.R

# Load the dataset and separate the field goal attempts
footballData <- readRDS("pbp2014-2024.rds")
fieldGoalData <- footballData[!is.na(footballData$play_type) & footballData$play_type == "field_goal", ]

# Convert "field_goal_result" to a binary where "made" = 1 and "missed" = 0 and fit the model
fieldGoalData$field_goal_result <- factor(fieldGoalData$field_goal_result, levels = c("missed", "made"))
logistic_model <- glm(field_goal_result ~ yardline_100, data = fieldGoalData, family = "binomial")

# Prediction function
predict_field_goal <- function(yardline) {
  if (yardline < 0 || yardline > 100) {
    stop("Yardline must be between 0 and 100.")
  }
  
  predicted_prob <- predict(logistic_model, newdata = data.frame(yardline_100 = yardline), type = "response")
  return(predicted_prob)
}