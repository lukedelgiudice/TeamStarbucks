library(dplyr)

data <- readRDS("pbp2014-2024.rds")

sample_player_position <- function(play) {
  if (play$play_type %in% c("qb_kneel", "qb_spike")) {
    return("qb")
  }
  
  if (play$play_type == "run") {
    runData <- data[data$play_type == "run", ]
    freq <- table(ifelse(runData$rusher_player_id != "" & !is.na(runData$rusher_player_id), "hb", "qb"))
    probs <- as.numeric(freq) / sum(freq)
    return(sample(names(freq), size = 1, prob = probs))
  }
  
  if (play$play_type == "pass") {
    if (!is.null(play$pass_length) && !is.na(play$pass_length)) {
      if (play$pass_length == "deep") {
        freq <- c(wr = 0.80, te = 0.20)
        return(sample(names(freq), size = 1, prob = freq))
      }
      else {  # short
        freq <- c(te = 0.50, wr = 0.30, hb = 0.20)
        return(sample(names(freq), size = 1, prob = freq))
      }
    } 
    
    else if (!is.null(play$pass_location) && !is.na(play$pass_location)) {
      if (play$pass_location == "middle") {
        freq <- c(te = 0.70, wr = 0.30)
        return(sample(names(freq), size = 1, prob = freq))
      } 
      
      else {
        freq <- c(wr = 0.70, te = 0.30)
        return(sample(names(freq), size = 1, prob = freq))
      }
    } 
    
    else {
      freq <- c(te = 0.40, wr = 0.40, hb = 0.20)
      return(sample(names(freq), size = 1, prob = freq))
    }
  }
  
  return("hb")
}

assign_player_position <- function(df) {
  df <- df %>% 
    rowwise() %>% 
    mutate(player_position = sample_player_position(cur_data_all())) %>% 
    ungroup()
  return(df)
}
