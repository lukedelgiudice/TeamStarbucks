# layer_player_position.R
library(dplyr)

assign_player_position <- function(down, ytg, fp, red_zone, ref_data) {
  subset_data <- ref_data %>%
    filter(down == !!down,
           ydstogo == !!ytg,
           yardline_100 == 100 - !!fp,
           red_zone == !!red_zone)
  
  if (nrow(subset_data) < 10) {
    subset_data <- ref_data %>%
      filter(down == !!down,
             ydstogo >= !!ytg - 2, ydstogo <= !!ytg + 2,
             yardline_100 >= (100 - !!fp) - 10, yardline_100 <= (100 - !!fp) + 10)
  }
  
  position_counts <- subset_data %>%
    count(player_position) %>%
    mutate(prob = n / sum(n))
  
  if (nrow(position_counts) > 0) {
    player_position <- sample(position_counts$player_position, 1, prob = position_counts$prob)
  } else {
    player_position <- sample(c("hb", "qb", "wr", "te"), 1)
  }
  
  return(player_position)
}
