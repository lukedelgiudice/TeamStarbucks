data <- readRDS("pbp2014-2024.rds")

simulate_interception <- function(play_type) {
  if (play_type != "pass") return(FALSE)
  subsetData <- data[data$play_type == "pass", ]
  rate <- mean(as.numeric(subsetData$interception), na.rm = TRUE)
  
  return(runif(1) < rate)
}
