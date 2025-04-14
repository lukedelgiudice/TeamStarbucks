library(dplyr)

simulate_fumble <- function(play_call, player_position, ref_data) {
  if (play_call != "run") return(FALSE)
  
  # Filter data for runs with the given player position
  subset_data <- ref_data %>%
    filter(play_call == "run",
           player_position == !!player_position)
  
  # Calculate fumble rate
  fumble_rate <- mean(subset_data$fumble, na.rm = TRUE)
  if (is.na(fumble_rate)) fumble_rate <- 0.015 # Default rate
  
  # Simulate fumble
  return(runif(1) < fumble_rate)
}