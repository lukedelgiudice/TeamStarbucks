library(dplyr)

get_run_probability <- function(position, data) {
  if (position == "hb") {
    subsetData <- data[data$play_call %in% c("run", "pass") &
                         data$rusher_player_id != "" &
                         !is.na(data$rusher_player_id), ]
    
    return(mean(subsetData$play_call == "run", na.rm = TRUE))
  }
  
  else if (position %in% c("wr", "te")) {
    subsetData <- data[data$play_call %in% c("run", "pass") &
                         data$receiver_player_id != "" &
                         !is.na(data$receiver_player_id), ]
    
    return(mean(subsetData$play_call == "pass", na.rm = TRUE))
  }
  
  else if (position == "qb") {
    return(0)
  }
  
  else {
    return(0.5)
  }
}

assign_run_pass <- function(df, data) {
  df <- df %>% 
    rowwise() %>% 
    mutate(
      play_call = if (player_position == "hb") {
        if (runif(1) < get_run_probability(player_position, data)) "run" else "pass"
      }
      else if (player_position %in% c("wr", "te")) {
        if (runif(1) < get_run_probability(player_position, data)) "pass" else "run"
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
