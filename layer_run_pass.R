library(dplyr)
data <- readRDS("pbp2014-2024.rds")

get_run_probability <- function(position) {
  if (position == "hb") {
    subsetData <- data[data$play_type %in% c("run", "pass") &
                                 data$rusher_player_id != "" &
                                 !is.na(data$rusher_player_id), ]
    
    return(mean(subsetData$play_type == "run", na.rm = TRUE))
  } 
  
  else if (position %in% c("wr", "te")) {
    subsetData <- data[data$play_type %in% c("run", "pass") &
                                 data$receiver_player_id != "" &
                                 !is.na(data$receiver_player_id), ]
    
    return(mean(subsetData$play_type == "pass", na.rm = TRUE))
  } 
  
  else if (position == "qb") {
    return(0)
  } 
  
  else {
    return(0.5)
  }
}

assign_run_pass <- function(df) {
  df <- df %>% 
    rowwise() %>% 
    mutate(
      play_call = if (player_position == "hb") {
        if (runif(1) < get_run_probability(player_position)) "run" else "pass"
      }
      else if (player_position %in% c("wr", "te")) {
        if (runif(1) < get_run_probability(player_position)) "pass" else "run"
      }
      else if (player_position == "qb") {
        "pass"
      } 
      else {
        NA_character_
      }
    ) %>% 
    ungroup()
  return(df)
}
