# layer_incompletion.R
library(dplyr)

simulate_incompletion <- function(play_call, pass_length, ref_data) {
  if (play_call != "pass") return(FALSE)
  
  subset_data <- ref_data %>%
    filter(play_call == "pass",
           pass_length == pass_length,
           (interception == 0 | is.na(interception)))
  
  incompletion_rate <- mean(subset_data$complete_pass == 0, na.rm = TRUE)
  if (is.na(incompletion_rate)) incompletion_rate <- 0.35
  
  return(runif(1) < incompletion_rate)
}
