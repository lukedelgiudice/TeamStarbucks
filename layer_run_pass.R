# layer_run_pass.R
library(dplyr)

assign_play_type <- function(player_position, down, ytg, fp, red_zone, ref_data) {
  subset_data <- ref_data %>%
    filter(player_position == player_position,
           down == down,
           ydstogo == ytg,
           yardline_100 == (100 - fp),
           red_zone == red_zone)
  
  if (nrow(subset_data) < 10) {
    subset_data <- ref_data %>%
      filter(player_position == player_position,
             down == down,
             ydstogo >= ytg - 2, ydstogo <= ytg + 2,
             yardline_100 >= (100 - fp) - 10, yardline_100 <= (100 - fp) + 10)
  }
  
  play_type_counts <- subset_data %>%
    count(play_call) %>%
    mutate(prob = n / sum(n))
  
  if (nrow(play_type_counts) > 0) {
    play_call <- sample(play_type_counts$play_call, 1, prob = play_type_counts$prob)
  } else {
    play_call <- if (player_position %in% c("hb", "qb")) "run" else "pass"
  }
  
  return(play_call)
}
