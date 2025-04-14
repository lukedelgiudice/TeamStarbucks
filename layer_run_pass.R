library(dplyr)

assign_play_type <- function(player_position, down, ytg, fp, red_zone, ref_data) {
  # Filter historical data based on player position and game state
  subset_data <- ref_data %>%
    filter(player_position == !!player_position,
           down == !!down,
           ydstogo == !!ytg,
           yardline_100 == 100 - !!fp,
           red_zone == !!red_zone)
  
  # Broaden filter if insufficient data
  if (nrow(subset_data) < 10) {
    subset_data <- ref_data %>%
      filter(player_position == !!player_position,
             down == !!down,
             ydstogo >= !!ytg - 2 & ydstogo <= !!ytg + 2,
             yardline_100 >= (100 - !!fp) - 10 & yardline_100 <= (100 - !!fp) + 10)
  }
  
  # Calculate probabilities of run and pass
  play_type_counts <- subset_data %>%
    count(play_call) %>%
    mutate(prob = n / sum(n))
  
  # Sample play type based on probabilities
  if (nrow(play_type_counts) > 0) {
    play_call <- sample(play_type_counts$play_call, 1, prob = play_type_counts$prob)
  } else {
    # Fallback based on player position
    play_call <- if (player_position %in% c("hb", "qb")) "run" else "pass"
  }
  
  return(play_call)
}