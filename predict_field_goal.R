footballData <- readRDS("pbp2014-2024.rds")
fieldGoalData <- footballData[!is.na(footballData$play_type) & footballData$play_type == "field_goal", ]
fieldGoalData$field_goal_result <- factor(fieldGoalData$field_goal_result, levels = c("missed", "made"))
logistic_model <- glm(field_goal_result ~ yardline_100, data = fieldGoalData, family = "binomial")

predict_field_goal <- function(yardline) {
  if (yardline < 0 || yardline > 100) {
    stop("yardline must be between 0 and 100")
  }
  
  predicted_prob <- predict(logistic_model, newdata = data.frame(yardline_100 = yardline), type = "response")
  return(predicted_prob)
}
