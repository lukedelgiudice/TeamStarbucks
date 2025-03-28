library(ggplot2)
library(mclust)

examine_leaf_distribution <- function(play_call, player_position, red_zone = FALSE) {
  data <- readRDS("pbp2014-2024.rds")
  data <- assign_player_position(data)
  
  subsetData <- data[data$play_type == play_call & data$player_position == player_position, ]
  
  if (red_zone) {
    subsetData <- subsetData[subsetData$yardline_100 <= 20, ]
  } 
  
  else {
    subsetData <- subsetData[subsetData$yardline_100 > 20, ]
  }
  subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  
  if (nrow(subsetData) < 30){
    message("too few observations for this leaf; using all ", play_call)
    subsetData <- data[data$play_type == play_call, ]
    subsetData <- subsetData[!is.na(subsetData$yards_gained), ]
  }
  
  if (nrow(subsetData) == 0){
    stop("no data for this leaf")
  }
  
  p <- ggplot(subsetData, aes(x = yards_gained)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = paste("pdf for ", play_call, "plays by ", player_position, 
                       ifelse(red_zone, "in Red Zone", "in Non-Red Zone")),
         x = "yards gained", y = "density")
  print(p)
  
  fit <- Mclust(subsetData$yards_gained, G = 1:3, verbose = FALSE)
  print(summary(fit))
  
  return(fit)
}


fit_leaf <- examine_leaf_distribution("run", "hb", red_zone = FALSE)
