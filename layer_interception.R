library(dplyr)

simulate_interception <- function(play_call, pass_length, ref_data) {
  if (play_call != "pass") return(FALSE)
  
  subset_data <- ref_data %>%
    filter(play_call == "pass", pass_length == pass_length)
  
  interception_rate <- mean(subset_data$interception, na.rm = TRUE)
  if (is.na(interception_rate)) interception_rate <- 0.02
  
  return(runif(1) < interception_rate)
}
