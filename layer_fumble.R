data <- readRDS("pbp2014-2024.rds")

simulate_fumble <- function(play_type, player_position) {
  if (play_type == "run") {
    subsetData <- data[data$play_type == "run", ]
    rate <- mean(as.numeric(subsetData$fumble), na.rm = TRUE)
  } 
  
  else if (play_type == "pass") {
    subsetData <- data[data$play_type == "pass", ]
    rate <- mean(as.numeric(subsetData$fumble), na.rm = TRUE)
  } 
  
  else {
    rate <- 0
  }
  
  return(runif(1) < rate)
}
