simulate_incompletion <- function(play_call, play_data) {
  if (play_call != "pass") {
    return(FALSE)
  }
  
  subsetData <- play_data[play_data$play_call == "pass", ]
  rate <- mean(as.numeric(subsetData$incomplete_pass), na.rm = TRUE)
  runif(1) < rate
}