data <- readRDS("pbp2014-2024.rds")

simulate_incompletion <- function(play_type) {
  if (play_type != "pass") return(FALSE)
  subsetData <- data[data$play_type == "pass", ]
  rate <- mean(as.numeric(subsetData$incomplete_pass), na.rm = TRUE)
  
  return(runif(1) < rate)
}
