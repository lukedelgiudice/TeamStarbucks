simulate_interception <- function(play_call, play_data) {
  if (play_call != "pass") return(FALSE)
  subsetData <- play_data[play_data$play_call == "pass", ]
  rate <- mean(subsetData$interception, na.rm = TRUE)
  runif(1) < rate
}