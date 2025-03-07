library(tidyverse)
library(lubridate)

data <- read_csv("nhl_pbp20162017.csv")

data$Shot_Success <- ifelse(data$Event %in% c("GOAL", "SHOT"), 1, 0)

createquadrant <- function(data) {
  data$quadrant <- ifelse(data$xC >= 0 & data$yC >= 0, 1,
                          ifelse(data$xC < 0 & data$yC >= 0, 2,
                                 ifelse(data$xC < 0 & data$yC < 0, 3,
                                        ifelse(data$xC >= 0 & data$yC < 0, 4, NA))))
  return(data)
}

data <- createquadrant(data)

shot_success_rates <- data %>%
  filter(Event %in% c("SHOT", "GOAL", "MISS")) %>% 
  group_by(quadrant) %>%
  summarise(success_count = sum(Shot_Success),
            total_shots = n(),
            success_rate = success_count / total_shots, .groups = "drop")

shot_success_model <- glm(success_count ~ as.factor(quadrant), 
                          offset(log(total_shots)),
                          data = shot_success_rates, 
                          family = "poisson")

summary(shot_success_model)



















