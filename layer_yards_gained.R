sample_yards_gained <- function(play_call, player_position, pass_length, red_zone, unexpected) {
  distributions <- readRDS("yards_distributions.rds")
  
  # Construct key
  key <- if (play_call == "pass") {
    paste(play_call, player_position, pass_length, red_zone, unexpected, sep = "_")
  } else {
    paste(play_call, player_position, NA, red_zone, unexpected, sep = "_")
  }
  
  # Fetch distribution
  dist <- distributions[[key]]
  if (is.null(dist)) {
    # Fallback to simple mean/sd
    return(rnorm(1, mean = 5, sd = 5))
  }
  
  # Sample from mixture model
  component <- sample(1:length(dist$lambda), 1, prob = dist$lambda)
  yards <- rnorm(1, mean = dist$mu[component], sd = dist$sigma[component])
  return(max(0, yards)) # Ensure non-negative yards
}