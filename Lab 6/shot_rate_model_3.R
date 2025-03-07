library(tidyverse)
library(lubridate)
library(logistf)

# Data
data <- read_csv("C:/Users/luked/Downloads/nhl_pbp20162017.csv")

# Quadrant variable (case when > ifelse)
data <- data %>%
  mutate(quadrant = case_when(
    xC >= 0 & yC >= 0 ~ 1,
    xC < 0 & yC >= 0 ~ 2,
    xC < 0 & yC < 0 ~ 3,
    xC >= 0 & yC < 0 ~ 4,
    TRUE ~ NA_real_
  ))

# Filter to JUST shot attempts
shot_data <- data %>%
  filter(Event %in% c("SHOT", "GOAL", "MISS"))

# Indicator for shot success (goal or on-goal)
shot_data <- shot_data %>%
  mutate(On_Goal = case_when(
    Event == "GOAL" ~ 1,
    Event == "SHOT" & !is.na(Description) & 
      str_detect(Description, regex("ONGOAL", ignore_case = TRUE)) ~ 1, # Fancy string detection for ONGOAL
    TRUE ~ 0
  ))

x_center <- mean(shot_data$xC, na.rm = TRUE)
x_scale  <- sd(shot_data$xC, na.rm = TRUE)
y_center <- mean(shot_data$yC, na.rm = TRUE)
y_scale  <- sd(shot_data$yC, na.rm = TRUE)

# Scaled coordinates
shot_data <- shot_data %>%
  mutate(xC_scaled = (xC - x_center) / x_scale,
         yC_scaled = (yC - y_center) / y_scale)

# Logistic regression model (for separation issues)
shot_success_model <- logistf(On_Goal ~ xC_scaled + yC_scaled, data = shot_data)

# New prediction function (probability of a shot being on goal)
predict_shot_success <- function(x, y) {
  new_data <- data.frame(
    xC_scaled = (x - x_center) / x_scale,
    yC_scaled = (y - y_center) / y_scale
  )
  predict(shot_success_model, newdata = new_data, type = "response")
}

# Test
predict_shot_success(x = 50, y = 0)
predict_shot_success(x = -2, y = 20)
