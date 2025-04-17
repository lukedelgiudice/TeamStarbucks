source("layer_player_position.R")
source("layer_run_pass.R")
source("layer_fumble.R")
source("layer_interception.R")
source("layer_incompletion.R")
source("layer_yards_gained.R")
source("predict_fourth_down.R")
source("predict_field_goal.R")

library(dplyr)

classify_drive_tendency <- function(play_history) {
  n <- length(play_history)
  if (n == 0) return("neutral")

  recent <- tail(play_history, min(5, n))

  weights <- seq(0.5, 1, length.out = length(recent))
  types <- sapply(recent, function(x) x$play_type)

  run_score <- sum(weights[types == "run"])
  pass_score <- sum(weights[types == "pass"])
  total_weight <- sum(weights[types %in% c("run","pass")])
  
  if (total_weight == 0) return("neutral")
  
  run_ratio <- run_score / total_weight
  pass_ratio <- pass_score / total_weight
  
  if (run_ratio > 0.65) return("run-heavy")
  if (pass_ratio > 0.65) return("pass-heavy")
  
  return("neutral")
}

get_unexpected_trigger <- function(play_history) {
  if (length(play_history) == 0) {
    n_since_last <- 0
  }
  
  else {
    unexpected_indices <- which(sapply(play_history, function(x) x$unexpected))
    
    if (length(unexpected_indices) == 0) {
      n_since_last <- length(play_history)
    }
    
    else {
      last_unexpected_index <- max(unexpected_indices)
      n_since_last <- length(play_history) - last_unexpected_index
    }
  }

  n_plays <- n_since_last + 1
  
  if (exists("UNEXPECTED_RATE", envir = .GlobalEnv)) {
    lambda <- get("UNEXPECTED_RATE", envir = .GlobalEnv)
  }
  
  else {
    lambda <- 0
  }
  
  p_trigger <- 1 - exp(-lambda * n_plays)
  return(runif(1) < p_trigger)
}

get_expected_play_type <- function(down, ytg, fp) {

  prob_pass <- 0.5
  
  if (down == 1) {
    if (ytg >= 10) prob_pass <- 0.55
    else prob_pass <- 0.45
  }
  
  else if (down == 2) {
    if (ytg >= 8) prob_pass <- 0.65
    else if (ytg <= 3) prob_pass <- 0.4
  }
  
  else if (down >= 3) {
    if (ytg >= 7) prob_pass <- 0.85
    else if (ytg <= 2) prob_pass <- 0.6
  }
  
  if (fp < 20) {
    prob_pass <- prob_pass + 0.05
  }
  
  else if (fp > 80) {
    prob_pass <- prob_pass - 0.05
  }
  
  prob_pass <- max(min(prob_pass, 1), 0)
  
  play_type <- sample(c("pass", "run"), size = 1, prob = c(prob_pass, 1 - prob_pass))
  return(play_type)
}

raw_data <- readRDS("pbp2014-2024.rds")

play_data <- raw_data %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    play_call = play_type,
    player_position = case_when(
      play_type == "run" ~ if_else(qb_scramble == 1, "qb", "hb"),
      play_type == "pass" ~ if_else(!is.na(pass_length),
                                    if_else(pass_length == "deep", "wr", "te"),
                                    "wr"),
      TRUE ~ "unknown"
    ),
    red_zone = yardline_100 <= 20
  )

simulate_punt <- function(fp) {
  available <- 100 - fp
  punt_data <- raw_data %>% filter(punt_attempt == 1, !is.na(kick_distance))
  
  subset_punts <- punt_data %>% 
    filter(abs((100 - yardline_100) - fp) < 10)
  
  if (nrow(subset_punts) < 20) {
    subset_punts <- punt_data
  }
  
  punt_yards <- round(sample(subset_punts$kick_distance, 1))
  punt_yards <- max(10, punt_yards)
  
  return(punt_yards)
}

simulate_fourth_down <- function(fp, ytg) {
  options_prob <- predict_fourth_down(fp, ytg)
  option <- sample(names(options_prob), 1, prob = options_prob)
  
  if (option == "FG" && fp < 60) {
    option <- "PUNT"
  }
  
  if (option == "FG") {
    prob_fg <- predict_field_goal(100 - fp)
    made <- runif(1) < prob_fg
    
    if (made) {
      return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "fg", yards = 0, play_type = "field_goal", unexpected = FALSE))
    }
    
    else {
      return(list(down = 5, ytg = ytg, fp = fp, exit_drive = 1, event = "missed_fg", yards = 0, play_type = "field_goal", unexpected = FALSE))
    }
  }
  
  else if (option == "PUNT") {
    punt_yards <- simulate_punt(fp)
    
    new_fp <- 100 - (fp + punt_yards)
    new_fp <- max(1, min(new_fp, 99))
    
    return(list(down = 5, ytg = ytg, fp = new_fp, exit_drive = 1, event = "punt", yards = punt_yards, play_type = "punt", unexpected = FALSE))
  }
  
  else if (option == "GFI") {
    new_state <- simulate_play_internal(4, ytg, fp, (100 - fp) <= 20)
    
    if (new_state$yards < ytg) {
      new_state$exit_drive <- 1
      new_state$event <- "turnover_on_downs"
      new_state$fp <- 100 - fp
      new_state$down <- 5
      
      return(new_state)
    }
    
    else {
      new_state$down <- 1
      new_state$ytg <- 10
      
      return(new_state)
    }
  }
}

simulate_play_internal <- function(down, ytg, fp, red_zone, play_history = list()) {
  
  unexpected_trigger <- get_unexpected_trigger(play_history)
  
  drive_tendency <- classify_drive_tendency(play_history)
  expected_play <- get_expected_play_type(down, ytg, fp)
  
  if (unexpected_trigger && drive_tendency != "neutral") {
    if (drive_tendency == "run-heavy") {
      play_call <- "pass"
      pass_length <- if (red_zone) "short" else "deep"
      player_position <- "wr"
      unexpected_flag <- TRUE
    }
    
    else if (drive_tendency == "pass-heavy") {
      play_call <- "run"
      unexpected_flag <- TRUE
      pass_length <- NA_character_
      player_position <- "hb"
    }
  }
  
  else {
    player_position <- assign_player_position_drive(down, ytg, fp, red_zone, play_data)
    play_call <- assign_play_type(player_position, down, ytg, fp, red_zone, play_data)
    
    pass_length <- if (play_call == "pass") {
      if (runif(1) < if (down == 3 && ytg > 8) 0.85 else 0.15) {
        "deep"
      }
      
      else {
        "short"
      }
    }
    
    else NA_character_
    unexpected_flag <- FALSE
  }
  
  if (play_call == "run") {
    if (simulate_fumble(play_call, player_position, play_data)) {
      return(list(down = down, ytg = ytg, fp = fp, exit_drive = 1, event = "fumble", yards = 0, play_type = play_call, unexpected = unexpected_flag))
    }
  }
  
  else if (play_call == "pass") {
    if (simulate_interception(play_call, pass_length, play_data)) {
      return(list(down = down, ytg = ytg, fp = fp, exit_drive = 1, event = "interception", yards = 0, play_type = play_call, unexpected = unexpected_flag))
    }
    
    if (simulate_incompletion(play_call, pass_length, play_data)) {
      new_down <- down + 1
      exit_drive <- if (new_down > 4) 1 else 0
      event <- if (new_down > 4) "turnover_on_downs" else "incompletion"
      
      return(list(down = new_down, ytg = ytg, fp = fp, exit_drive = exit_drive, event = event, yards = 0, play_type = play_call, unexpected = unexpected_flag))
    }
  }
  
  yards <- sample_yards_gained(play_call, player_position, pass_length, red_zone, unexpected_flag)
  max_yards <- 100 - fp
  yards <- min(yards, max_yards)
  
  new_fp <- fp + yards
  
  if(new_fp > 90) {
    new_ytg <- 100 - new_fp
  }
  
  else {
    new_ytg <- max(0, ytg - yards)
  }
  
  new_down <- if (new_ytg == 0) 1 else (down + 1)
  new_ytg <- if (new_ytg == 0) 10 else new_ytg
  exit_drive <- 0
  event <- "play_complete"
  
  if (new_fp >= 100) {
    return(list(down = NA, ytg = NA, fp = 105, exit_drive = 1, event = "td", yards = yards, play_type = play_call, unexpected = unexpected_flag))
  }
  
  if (new_down > 4) {
    exit_drive <- 1
    event <- "turnover_on_downs"
  }
  
  list(
    down = new_down,
    ytg = new_ytg,
    fp = new_fp,
    exit_drive = exit_drive,
    event = event,
    yards = yards,
    play_type = play_call,
    unexpected = unexpected_flag
  )
}

simulate_play <- function(down, ytg, fp, red_zone, play_history = list()) {
  if (down == 4) {
    return(simulate_fourth_down(fp, ytg))
  }
  
  else {
    return(simulate_play_internal(down, ytg, fp, red_zone, play_history))
  }
}

run_play <- simulate_play
