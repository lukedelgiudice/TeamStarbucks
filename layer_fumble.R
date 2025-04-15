library(dplyr)

simulate_fumble <- function(play_call, player_position, ref_data) {
  if (play_call != "run") return(FALSE)
  
  subset_data <- ref_data %>%
    filter(play_call == "run", player_position == player_position)
  
  fumble_rate <- mean(subset_data$fumble, na.rm = TRUE)
  if (is.na(fumble_rate)) fumble_rate <- 0.015
  
  return(runif(1) < fumble_rate)
}
