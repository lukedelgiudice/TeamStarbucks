
library(tidyverse)
library(lubridate)


data <- read_csv("C:/Users/luked/Downloads/nhl_pbp20162017.csv")

data$Time_Seconds <- period_to_seconds(hms(data$Time_Elapsed)) / 60

data$Is_Shot <- data[data$Event %in% c("SHOT", "GOAL"), ]

# 4 blocks of 5 minutes for each period
data$Time_Block <- (data$Period - 1) * 4 + ifelse(data$Time_Seconds <= 300, 1, 
                                        ifelse(data$Time_Seconds <= 600, 2,
                                               ifelse(data$Time_Seconds <= 900, 3, 4)))

poisson.model <- glm(Is_Shot ~ ., family = 'poisson', data = data)


predict_fourth_down <- function(time, point_diff) {
  
  # Convert time
  
  predictions <- predict(poisson.model, newdata = data.frame(Time_Block = fp, ydstogo = ytg), type = "probs")
  predictions
}

