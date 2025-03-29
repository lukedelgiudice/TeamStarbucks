simulate_fumble <- function(play_call, player_position, play_data) {
  if (play_call == "run") {
    subsetData <- play_data[play_data$play_call == "run", ]
    rate <- mean(as.numeric(subsetData$fumble), na.rm = TRUE)
  } 
  else if (play_call == "pass") {
    subsetData <- play_data[play_data$play_call == "pass", ]
    rate <- mean(as.numeric(subsetData$fumble), na.rm = TRUE)
  } 
  else {
    rate <- 0
  }
  runif(1) < rate
}