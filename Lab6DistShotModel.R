hockey<-read.csv("nhl_pbp20162017.csv")

#Create Quadrants for distance and create respective categories

createquadrant <- function(data) {
  data$quadrant <- ifelse(data$xC >= 0 & data$yC >= 0, 1,
                          ifelse(data$xC < 0 & data$yC >= 0, 2,
                                 ifelse(data$xC < 0 & data$yC < 0, 3,
                                        ifelse(data$xC >= 0 & data$yC < 0, 4, NA))))
  return(data)
  }

hockeyquad<- createquadrant(hockey)

pointdif <- function(data) {
  data$pointdiff <- data$Home_Score - data$Away_Score
  return(data)
}

hockeypointdiffquad<- pointdif(hockeyquad)

shot_rates <- hockeypointdiffquad %>%
  filter(Event == "SHOT") %>% 
  group_by(quadrant, pointdiff) %>%
  summarise(shot_count = n(), .groups = "drop")

hockey_poisson_model <- glm(shot_count ~ as.factor(quadrant) + pointdiff, 
                            data = shot_rates, 
                            family = "poisson")


  