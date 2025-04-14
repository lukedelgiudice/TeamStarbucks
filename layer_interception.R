library(dplyr)

simulate_interception <- function(play_call, pass_length, ref_data) {
  if (play_call != "pass") return(FALSE)
  
  # Filter data for passes with the given pass length
  subset_data <- ref_data %>%
    filter(play_call == "pass",
           pass_length == !!pass_length)
  
  # Calculate interception rate
  interception_rate <- mean(subset_data$interception, na.rm = TRUE)
  if (is.na(interception_rate)) interception_rate <- 0.02 # Default rate
  
  # Simulate interception
  return(runif(1) < interception_rate)
}