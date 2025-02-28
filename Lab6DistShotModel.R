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

hockeypointdiffquad$Type <- factor(hockeypointdiffquad$Type, levels = c("attempted","dna"))
logistic_model <- glm(predictshotdist ~ , data = hockeypointdiffquad, family = "poisson")

predictshotdist <- function(quadrant, pointdif) {

  
  
  
  }
  