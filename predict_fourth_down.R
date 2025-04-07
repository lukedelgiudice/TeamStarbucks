# Multinomial regression library
library(nnet)

# Load dataset
footballData <- readRDS("pbp2014-2024.rds")

# Get all 4th down data matching the 3 options
fourthDownData <- footballData[footballData$down == 4 & footballData$play_type %in% c("pass", "run", "field_goal", "punt"), ]

# Make fourth_down_options a factor
fourthDownData$fourth_down_options <- factor(
  ifelse(fourthDownData$play_type == "field_goal", "FG", 
         ifelse(fourthDownData$play_type == "punt", "PUNT", "GFI")),
  levels = c("FG", "GFI", "PUNT")
)

# Fit multinomial regression model
multinom_model <- multinom(fourth_down_options ~ yardline_100 + ydstogo, data = fourthDownData)

# Function to return predicted probabilities
predict_fourth_down <- function(fp, ytg) {
  # Use predict with the inputs given as the 2 predictors
  predictions <- predict(multinom_model, newdata = data.frame(yardline_100 = fp, ydstogo = ytg), type = "probs")
  predictions
}

# test
# predict_fourth_down(43, 3)



