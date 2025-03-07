library(tidyverse)
library(lubridate)

# Data
data <- read_csv("C:/Users/luked/Downloads/nhl_pbp20162017.csv")

# Get minutes
data <- data %>%
  mutate(time_minutes = Seconds_Elapsed / 60)

# SHOT/GOAL indicator
data <- data %>%
  mutate(is_shot = if_else(Event %in% c("SHOT", "GOAL"), 1, 0))

# Make 4 blocks for each period
data <- data %>%
  mutate(time_block = (Period - 1) * 4 + case_when(
    time_minutes <= 5 ~ 1,
    time_minutes <= 10 ~ 2,
    time_minutes <= 15 ~ 3,
    time_minutes <= 20 ~ 4
  )) %>%
  mutate(time_block = as.factor(time_block))

# Ev_Team (event team) is the shooting team
data <- data %>%
  mutate(shooting_team = Ev_Team)

# Point differential for the team with possession
data <- data %>%
  mutate(point_diff = case_when(
    shooting_team == Home_Team ~ Home_Score - Away_Score,
    shooting_team == Away_Team ~ Away_Score - Home_Score,
    TRUE ~ NA_real_
  ))

# Include all valid events
model_data <- data %>%
  filter(!is.na(shooting_team) & !is.na(point_diff)) %>%
  select(time_block, point_diff, is_shot)

# Fit the poisson regression
poisson_model <- glm(is_shot ~ time_block + point_diff, 
                     family = poisson(link = "log"),
                     data = model_data)

# Predict the shot rate
predict_shot_rate <- function(time_block, point_diff) {
  new_data <- data.frame(
    time_block = as.factor(time_block),
    point_diff = point_diff
  )
  predict(poisson_model, newdata = new_data, type = "response")
}

# Test
predict_shot_rate(time_block = 10, point_diff = 20)
