library(dplyr)

simulate_incompletion <- function(play_call, pass_length, ref_data) {
  if (play_call != "pass") return(FALSE)
  
  # Filter data for passes with the given pass length, excluding interceptions
  subset_data <- ref_data %>%
    filter(play_call == "pass",
           pass_length == !!pass_length,
           interception == 0 | is.na(interception))
  
  # Calculate incompletion rate
  incompletion_rate <- mean(subset_data$complete_pass == 0, na.rm = TRUE)
  if (is.na(incompletion_rate)) incompletion_rate <- 0.35 # Default rate
  
  # Simulate incompletion
  return(runif(1) < incompletion_rate)
}